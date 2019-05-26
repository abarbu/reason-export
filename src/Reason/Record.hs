{-# LANGUAGE OverloadedStrings #-}

module Reason.Record
  ( toReasonTypeRef
  , toReasonTypeRefWith
  , toReasonTypeSource
  , toReasonTypeSourceWith
  , renderType
  ) where

import Control.Monad.RWS
import qualified Data.Text as T
import Reason.Common
import Reason.Type
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

class HasType a where
  render :: a -> RenderM Doc

class HasRecordType a where
  renderRecord :: a -> RenderM Doc

class HasTypeRef a where
  renderRef :: a -> RenderM Doc

instance HasType ReasonDatatype where
  render d@(ReasonDatatype name constructor@(RecordConstructor _ _)) = do
    put $ Just name
    name' <- renderRef d
    ctor <- render constructor
    return . nest 4 $ "type" <+> name' <+> "=" <$$> ctor
  render d@(ReasonDatatype name constructor) = do
    put $ Just name
    name' <- renderRef d
    ctor <- render constructor
    return . nest 4 $ "type" <+> name' <+> "=" <$$> ("|" <> space <> ctor)
  render (ReasonPrimitive primitive) = renderRef primitive

namespaceIfNeeded :: T.Text -> T.Text -> RenderM Doc
namespaceIfNeeded typeName obj = do
  currentName <- get
  case currentName of
      Nothing -> pure (stextSuffix ("." <> obj) (typeName <> "Type"))
      Just cn -> if cn == typeName then
                  pure "t" else
                  pure (stextSuffix ("." <> obj) (typeName <> "Type"))

instance HasTypeRef ReasonDatatype where
  renderRef (ReasonDatatype typeName _) = pure (stextLower typeName) -- namespaceIfNeeded typeName "t"
  renderRef (ReasonPrimitive primitive) = renderRef primitive

instance HasType ReasonConstructor where
  render (RecordConstructor _ value) = do
    dv <- renderRecord value
    return $ "{" <+> dv <$$> "}"
  render (NamedConstructor constructorName value) = do
    dv <- render value
    return $ stext constructorName <> (if isEmpty dv then
                                         empty else
                                         parens dv)
  render (MultipleConstructors constructors) =
    mintercalate (line <> "|" <> space) <$> sequence (render <$> constructors)

instance HasType ReasonValue where
  render (ReasonRef name) = pure (stextLower name)
  render (ReasonPrimitiveRef primitive) = elmRefParens primitive <$> renderRef primitive
  render ReasonEmpty = pure (text "")
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <> "," <+> dy
  render (ReasonField name value) = do
    fieldModifier <- asks fieldLabelModifier
    dv <- renderRecord value
    return $ stext (fieldModifier (maybeReserved name)) <+> ":" <+> dv

instance HasRecordType ReasonValue where
  renderRecord (ReasonPrimitiveRef primitive) = renderRef primitive
  renderRecord (Values x y) = do
    dx <- renderRecord x
    dy <- renderRecord y
    return $ dx <$$> comma <+> dy
  renderRecord value = render value

instance HasTypeRef ReasonPrimitive where
  renderRef (RList (ReasonPrimitive RChar)) = renderRef RString
  renderRef (RList datatype) = do
    dt <- renderRef datatype
    return $ "list" <+> parens dt
  renderRef (RTuple2 x y) = do
    dx <- renderRef x
    dy <- renderRef y
    return . parens $ dx <> comma <+> dy
  renderRef (RTuple3 x y z) = do
    dx <- renderRef x
    dy <- renderRef y
    dz <- renderRef z
    return . parens $ dx <> comma <+> dy <+> comma <+> dz
  renderRef (ROption datatype) = do
    dt <- renderRef datatype
    return $ "option" <+> parens dt
  renderRef (RMap k v) = do
    dk <- renderRef k
    let kname = primitiveName k
    require ("module Map_" <> kname -- displayTStrict (renderCompact dk)
             <> " = Map.Make({ type t = "
             <> displayTStrict (renderCompact dk)
             <> "; let compare = compare });")
    dv <- renderRef v
    return $ "Map_" <> stext kname <> ".t" <+> parens dv
  renderRef RInt = pure "int"
  renderRef RInt64 = pure "int64"
  renderRef RTimePosix = pure "Js.Date.t"
  renderRef RBool = pure "bool"
  renderRef RChar = pure "char"
  renderRef RString = pure "string"
  renderRef RUnit = pure ""
  renderRef RFloat = pure "float"

-- | Puts parentheses around the doc of an elm ref if it contains spaces.
elmRefParens :: ReasonPrimitive -> Doc -> Doc
elmRefParens (RList (ReasonPrimitive RChar)) = id
elmRefParens (RList _) = parens
elmRefParens (ROption _) = parens
elmRefParens (RMap _ _) = parens
elmRefParens _ = id

toReasonTypeRefWith
  :: ReasonType a
  => Options -> a -> T.Text
toReasonTypeRefWith options x =
  pprinter . fst $ evalRWS (renderRef (toReasonType x)) options Nothing

toReasonTypeRef
  :: ReasonType a
  => a -> T.Text
toReasonTypeRef = toReasonTypeRefWith defaultOptions

toReasonTypeSourceWith
  :: ReasonType a
  => Options -> a -> T.Text
toReasonTypeSourceWith options x =
  pprinter . fst $ evalRWS (render (toReasonType x)) options Nothing

toReasonTypeSource
  :: ReasonType a
  => a -> T.Text
toReasonTypeSource = toReasonTypeSourceWith defaultOptions

renderType
  :: ReasonType a
  => a -> RenderM ()
renderType = collectDeclaration . render . toReasonType
