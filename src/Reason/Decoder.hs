{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Reason.Decoder
  ( toReasonDecoderRef
  , toReasonDecoderRefWith
  , toReasonDecoderSource
  , toReasonDecoderSourceWith
  , renderDecoder
  ) where

import Control.Monad.RWS
import qualified Data.Text as T
import Reason.Common
import Reason.Type
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

class HasDecoder a where
  render :: a -> RenderM Doc

class HasDecoderRef a where
  renderRef :: a -> RenderM Doc

instance HasDecoder ReasonDatatype where
  render d@(ReasonDatatype _ constructor) = do
    fnName <- renderRef d
    ctor <- render constructor
    return $ "let rec" <+> fnName <+> "= json =>" <$$> indent 4 ctor
  render (ReasonPrimitive primitive) = renderRef primitive

instance HasDecoderRef ReasonDatatype where
  renderRef (ReasonDatatype name _) = pure $ "decode" <> stext name
  renderRef (ReasonPrimitive primitive) = renderRef primitive

instance HasDecoder ReasonConstructor where
  render (NamedConstructor name ReasonEmpty) =
    return $ "json |> Json.Decode.nullAs" <> parens (stext name)
  render (NamedConstructor name (ReasonPrimitiveRef RUnit)) =
    return $ "json |> Json.Decode.nullAs" <> parens (stext name)
  render (NamedConstructor name value@(Values _ _)) = do
    (_, tyargs) <- renderConstructorArgs' 0 value
    pure $ case tyargs of
      [] -> stext name
      [(r0,_)] -> stext name <> parens (stext "json |> " <> r0)
      [(r0,a0), (r1,a1)] ->
        parens (parens (tupled [a0, a1]) <+> "=>" <+> stext name <> tupled [a0, a1])
        <> parens (stext "json |> Json.Decode.tuple2" <> tupled [r0, r1])
      [(r0,a0), (r1,a1), (r2,a2)] ->
        parens (parens (tupled [a0, a1]) <+> "=>" <+> stext name <> tupled [a0, a1, a2])
        <> parens (stext "json |> Json.Decode.tuple3" <> tupled [r0, r1, r2])
      [(r0,a0), (r1,a1), (r2,a2) , (r3,a3)] ->
        parens (parens (tupled [a0, a1]) <+> "=>" <+> stext name <> tupled [a0, a1, a2, a3])
        <> parens (stext "json |> Json.Decode.tuple4" <> tupled [r0, r1, r2, r3])
      _ -> error "Bare constructors with more than 4 arguments are not supported, use records"
  render (NamedConstructor name value) = do
    (_, val) <- renderConstructorArgs 0 value
    return $ (stext name <+> parens val)
  render (RecordConstructor _ value) = do
    dv <- render value
    return $ braces (line <> indent 4 dv)
  render mc@(MultipleConstructors constrs) = do
    cstrs <- mapM renderSum constrs
    pure $ line
              <> indent 4
               ("json |>" <+> parens
                 ((if isEnumeration mc then
                     "Json.Decode.string" else
                     "Json.Decode.field(\"tag\", Json.Decode.string)")
                   <$$> indent 4
                  ("|> Json.Decode.andThen" <$$>
                   parens ("(x) => switch(x)" <$$> braces (
                              line <> indent 4 (foldl1 (<$$>) cstrs
                                                <$$> "|" <+> "_ =>" <+> "failwith(\"unknown constructor\")"))))))
  
-- | "<name>" -> decode <name>
renderSumCondition :: T.Text -> Doc -> RenderM Doc
renderSumCondition name contents =
  pure $ "|" <+> dquotes (stext name) <+> "=> json =>" <+> contents

-- | Render a sum type constructor in context of a data type with multiple
-- constructors.
renderSum :: ReasonConstructor -> RenderM Doc
renderSum (NamedConstructor name ReasonEmpty) = do
  renderSumCondition name (stext name)
renderSum n@(NamedConstructor name (Values _ _)) = do
  r <- render n
  renderSumCondition name r
renderSum (NamedConstructor name value) = do
  r <- render value
  renderSumCondition name (stext name <> parens ("json |> Json.Decode.field(\"contents\", " <> r <> ")"))
renderSum (RecordConstructor name value) = do
  val <- render value
  renderSumCondition name (stext name <> indent 0 (parens ("{" <> val <> "}")))
renderSum (MultipleConstructors constrs) =
  foldl1 (<$$>) <$> mapM renderSum constrs

-- | Render the decoding of a constructor's arguments. Note the constructor must
-- be from a data type with multiple constructors and that it has multiple
-- constructors itself.
renderConstructorArgs :: Int -> ReasonValue -> RenderM (Int, Doc)
renderConstructorArgs i (Values l r) = do
  (iL, rndrL) <- renderConstructorArgs i l
  (iR, rndrR) <- renderConstructorArgs (iL + 1) r
  pure (iR, rndrL <$$> rndrR)
renderConstructorArgs i val = do
  rndrVal <- render val
  pure (i, "json |> Json.Decode.field" <+> tupled [dquotes ("arg" <> int i), rndrVal] <> comma)

renderConstructorArgs' :: Int -> ReasonValue -> RenderM (Int, [(Doc,Doc)])
renderConstructorArgs' i (Values l r) = do
  (iL, rndrL) <- renderConstructorArgs' i l
  (iR, rndrR) <- renderConstructorArgs' (iL + 1) r
  pure (iR, rndrL <> rndrR)
renderConstructorArgs' i val = do
  rndrVal <- render val
  pure (i, [(rndrVal, ("arg" <> int i))])

instance HasDecoder ReasonValue where
  render (ReasonRef name) = pure $ "decode" <> stext name
  render (ReasonPrimitiveRef primitive) = renderRef primitive
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> dy
  render (ReasonField name value) = do
    fieldModifier <- asks fieldLabelModifier
    dv <- render value
    -- return $ "|> required" <+> dquotes (stext (fieldModifier name)) <+> dv
    return $ (stext (fieldModifier name)) <+> ":" <+> "json |> Json.Decode.field" <+> tupled [dquotes (stext (fieldModifier name)), dv] <> comma
  render ReasonEmpty = pure (stext "")

instance HasDecoderRef ReasonPrimitive where
  renderRef (RList (ReasonPrimitive RChar)) = pure "Json.Decode.string"
  renderRef (RList datatype) = do
    dt <- renderRef datatype
    return $ "Json.Decode.list" <> parens dt
  renderRef (RMap key value) = do
    k <- renderRef key
    d <- renderRef value
    let kname = primitiveName key
    return $ "Json.Decode.map" <> tupled ["l => List.fold_left" <+> tupled ["(m,(k,v)) =>" <+> "Map_" <> stext kname <> ".add(k,v,m)"
                                                               ,"Map_" <> stext kname <> ".empty"
                                                               ,"l"]
                             ,"Json.Decode.list(Json.Decode.tuple2(" <> k <> "," <> d <> "))"]
  renderRef (ROption datatype) = do
    dt <- renderRef datatype
    return $ "Json.Decode.optional" <> parens dt
  renderRef (RTuple2 x y) = do
    dx <- renderRef x
    dy <- renderRef y
    return $ "Json.Decode.tuple2" <> tupled [dx, dy]
  renderRef (RTuple3 x y z) = do
    dx <- renderRef x
    dy <- renderRef y
    dz <- renderRef z
    return $ "Json.Decode.tuple3" <> tupled [dx, dy, dz]
  renderRef RUnit = pure "Json.Decode.nullAs"
  renderRef RTimePosix = pure "Json.Decode.date"
  renderRef RInt = pure "Json.Decode.int"
  renderRef RBool = pure "Json.Decode.bool"
  renderRef RChar = pure "Json.Decode.char"
  renderRef RFloat = pure "Json.Decode.float"
  renderRef RString = pure "Json.Decode.string"

toReasonDecoderRefWith
  :: ReasonType a
  => Options -> a -> T.Text
toReasonDecoderRefWith options x =
  pprinter . fst $ evalRWS (renderRef (toReasonType x)) options Nothing

toReasonDecoderRef
  :: ReasonType a
  => a -> T.Text
toReasonDecoderRef = toReasonDecoderRefWith defaultOptions

toReasonDecoderSourceWith
  :: ReasonType a
  => Options -> a -> T.Text
toReasonDecoderSourceWith options x =
  pprinter . fst $ evalRWS (render (toReasonType x)) options Nothing

toReasonDecoderSource
  :: ReasonType a
  => a -> T.Text
toReasonDecoderSource = toReasonDecoderSourceWith defaultOptions

renderDecoder
  :: ReasonType a
  => a -> RenderM ()
renderDecoder x = do
  collectDeclaration . render . toReasonType $ x
