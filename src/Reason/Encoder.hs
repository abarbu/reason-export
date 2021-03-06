{-# LANGUAGE OverloadedStrings #-}

module Reason.Encoder
  ( toReasonEncoderRef
  , toReasonEncoderRefWith
  , toReasonEncoderSource
  , toReasonEncoderSourceWith
  , renderEncoder
  ) where

import Control.Monad.RWS
import qualified Data.Text as T
import Reason.Common
import Reason.Type
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

class HasEncoder a where
  render :: a -> RenderM Doc

class HasEncoderRef a where
  renderRef :: Int -> a -> RenderM Doc

instance HasEncoder ReasonDatatype where
  render d@(ReasonDatatype name constructor) = do
    fnName <- renderRef 0 d
    ctor <- render constructor
    return $ ("let rec" <+> fnName <+> " = (x : " <> stextLower name <> ")" <+> "=>" <$$> indent 4 ctor)
  render (ReasonPrimitive primitive) = renderRef 0 primitive

instance HasEncoderRef ReasonDatatype where
  renderRef _ (ReasonDatatype name _) = pure $ "encode" <> stext name
  renderRef level (ReasonPrimitive primitive) = renderRef level primitive

instance HasEncoder ReasonConstructor where
  -- Single constructor, no values: empty array
  render (NamedConstructor _name ReasonEmpty) =
    return $ "Json.Encode.null"
  render (NamedConstructor _name (ReasonPrimitiveRef RUnit)) =
    return $ "Json.Encode.null"
  -- Single constructor, multiple values: create array with values
  render (NamedConstructor name value@(Values _ _)) = do
    ps <- collectParameters' 0 value
    pure $ nest 4 $ "switch(x)" <+>
      (braces $ line <> "|" <+> stext name <> tupled (map snd ps) <+> "=>"
        <$$> indent 2 "Json.Encode.jsonArray([|"
        <$$> indent 4 (sep (punctuate comma (map (\(t,arg) -> t <> parens arg) ps)))
        <$$> indent 2 "|])")
  -- Single constructor, one value: skip constructor and r just the value
  render (NamedConstructor name value) = do
    dv <- render value
    let cs = "|" <+> stext name <> parens "y0" <+> "=>"
    return $ nest 4 $ "switch(x)" <+> (braces $ line <> cs <+> nest 4 (nest 4 dv <> parens "y0"))
  render (RecordConstructor _ value) = do
    dv <- render value
    return . nest 4 $ "Json.Encode.object_" <$$> parens ("[" <+> dv <$$> "]")
  render mc@(MultipleConstructors constrs) = do
    let rndr = if isEnumeration mc then renderEnumeration else renderSum
    dc <- mapM rndr constrs
    return . nest 4 $ "switch(x)" <+> (braces $ line <> foldl1 (<$$>) dc)

jsonEncodeObject :: Doc -> Doc -> Either [(Doc,Doc)] Doc -> Doc
jsonEncodeObject constructor tag (Left []) =
  nest 4 $ "|" <+> constructor <$$> nest 4 ("Json.Encode.object_" <$$> parens (brackets (tag)))
jsonEncodeObject constructor tag (Left contents) =
  nest 4 $ "|" <+> constructor <$$>
    nest 4 ("Json.Encode.object_" <$$>
            parens (brackets (tag <> comma <>
                               (case contents of
                                   [] -> empty
                                   [(v,a)] -> line <> indent 2 (parens ("\"contents\"" <> comma <+> v <> parens a))
                                   _ -> line <> indent 2 (parens ("\"contents\"" <> comma <+> "Json.Encode.jsonArray([|"
                                               <$$> indent 2 (vcat (punctuate "," (map (\(r,a) -> r <> parens a) contents)))
                                               <$$> "|])"))))))
jsonEncodeObject constructor tag (Right contents) =
  nest 4 $ "|" <+> constructor <$$>
    nest 4 ("Json.Encode.object_" <$$>
            parens (brackets (tag <> comma <> contents)))

renderSum :: ReasonConstructor -> RenderM Doc
renderSum (NamedConstructor name ReasonEmpty) = do
  let cs = stext name <+> "=>"
  let tag = pair (dquotes "tag") ("Json.Encode.string" <> parens (dquotes (stext name)))
  return $ jsonEncodeObject cs tag (Left [])
renderSum (NamedConstructor name value) = do
  ps <- collectParameters' 0 value
  let cs = stext name <> tupled (map snd ps) <+> "=>"
  let tag = pair (dquotes "tag") ("Json.Encode.string" <> parens (dquotes (stext name)))
  return $ jsonEncodeObject cs tag (Left ps)
renderSum (RecordConstructor name value) = do
  -- dv <- render value
  -- let ct = comma <+> dv
  ct <- renderFields value
  let cs = stext name <> parens (encloseSep "{" "}" "," (map (\(n,_,_) -> n) ct)) <+> "=>"
  let tag = pair (dquotes "tag") (dquotes $ stext name)
  return $ jsonEncodeObject cs tag (Right (line <> indent 2 (vcat (punctuate "," (map (\(_,a,r) -> parens (dquotes a <> comma <+> r)) ct)))))
renderSum (MultipleConstructors constrs) = do
  dc <- mapM renderSum constrs
  return $ foldl1 (<$+$>) dc


renderEnumeration :: ReasonConstructor -> RenderM Doc
renderEnumeration (NamedConstructor name _) =
  return . nest 4 $ "|" <+> stext name <+> "=>" <+> "Json.Encode.string" <> (parens (dquotes (stext name)))
renderEnumeration (MultipleConstructors constrs) = do
  dc <- mapM renderEnumeration constrs
  return $ foldl1 (<$$>) dc
renderEnumeration c = render c


renderFields :: ReasonValue -> RenderM [(Doc, Doc, Doc)]
renderFields (ReasonField name value) = do
    fieldModifier <- asks fieldLabelModifier
    valueBody <- render value
    pure $ [(stext name, stext (fieldModifier name), valueBody <> parens (stext name))]
renderFields (Values x y) = do
    dx <- renderFields x
    dy <- renderFields y
    pure $ dx ++ dy
renderFields _ = error "renderFields ReasonValue: should not happen"

instance HasEncoder ReasonValue where
  render (ReasonField name value) = do
    fieldModifier <- asks fieldLabelModifier
    valueBody <- render value
    return . spaceparens $
      dquotes (stext (fieldModifier name)) <> comma <+> (valueBody <> parens ("x." <> stext (fieldModifier name)))
  render (ReasonPrimitiveRef primitive) = renderRef 0 primitive
  render (ReasonRef name) = pure $ "encode" <> stext name
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> comma <+> dy
  render _ = error "HasEncoderRef ReasonValue: should not happen"

instance HasEncoderRef ReasonPrimitive where
  renderRef _ RTimePosix = pure $ "Json.Encode.date"
  renderRef _ RUnit = pure "Json.Encode.null"
  renderRef _ RInt = pure "Json.Encode.int"
  renderRef _ RInt64 = pure "Json.Encode.int"
  renderRef _ RChar = pure "Json.Encode.char"
  renderRef _ RBool = pure "Json.Encode.bool"
  renderRef _ RFloat = pure "Json.Encode.float"
  renderRef _ RString = pure "Json.Encode.string"
  renderRef _ (RList (ReasonPrimitive RChar)) = pure "Json.Encode.string"
  renderRef level (RList datatype) = do
    dd <- renderRef level datatype
    return . parens $ "Json.Encode.list" <> parens dd
  renderRef level (ROption datatype) = do
    dd <- renderRef level datatype
    return . parens $ "Json.Encode.nullable" <> parens dd
  renderRef level (RTuple2 x y) = do
    dx <- renderRef (level + 1) x
    dy <- renderRef (level + 1) y
    return $ "Json.Encode.tuple2" <> tupled [dx, dy]
  renderRef level (RTuple3 x y z) = do
    dx <- renderRef (level + 1) x
    dy <- renderRef (level + 1) y
    dz <- renderRef (level + 1) z
    return $ "Json.Encode.tuple3" <> tupled [dx, dy, dz]
  renderRef level (RMap k v) = do
    dk <- renderRef level k
    dv <- renderRef level v
    let kname = primitiveName k
    return $ parens $ "(x) =>" <+> "Json.Encode.list" <> parens ("Json.Encode.tuple2" <> tupled [dk, dv])
      <> parens ("Map_" <> stext kname <> ".bindings(x)")

toReasonEncoderRefWith
  :: ReasonType a
  => Options -> a -> T.Text
toReasonEncoderRefWith options x =
  pprinter . fst $ evalRWS (renderRef 0 (toReasonType x)) options Nothing

toReasonEncoderRef
  :: ReasonType a
  => a -> T.Text
toReasonEncoderRef = toReasonEncoderRefWith defaultOptions

toReasonEncoderSourceWith
  :: ReasonType a
  => Options -> a -> T.Text
toReasonEncoderSourceWith options x =
  pprinter . fst $ evalRWS (render (toReasonType x)) options Nothing

toReasonEncoderSource
  :: ReasonType a
  => a -> T.Text
toReasonEncoderSource = toReasonEncoderSourceWith defaultOptions

renderEncoder
  :: ReasonType a
  => a -> RenderM ()
renderEncoder x = do
  require "Json.Encode"
  collectDeclaration . render . toReasonType $ x

collectParameters' :: Int -> ReasonValue -> RenderM [(Doc,Doc)]
collectParameters' _ ReasonEmpty = pure []
collectParameters' i (Values l r) = do
  left <- collectParameters' i l
  right <- collectParameters' (length left + i) r
  pure $ left ++ right
collectParameters' i v = do
  r <- render v
  pure $ [(r, "arg" <> int i)]
