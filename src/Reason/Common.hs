{-# LANGUAGE OverloadedStrings #-}

module Reason.Common where

import Control.Monad.RWS
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.Text.Lazy as LT
import Formatting hiding (text)
import Text.PrettyPrint.Leijen.Text hiding ((<$>))

data Options = Options
  { fieldLabelModifier :: Text -> Text
  }

defaultOptions :: Options
defaultOptions = Options {fieldLabelModifier = id}

cr :: Format r r
cr = now "\n"

mintercalate
  :: Monoid m
  => m -> [m] -> m
mintercalate _ [] = mempty
mintercalate _ [x] = x
mintercalate seperator (x:xs) = x <> seperator <> mintercalate seperator xs

pprinter :: Doc -> Text
pprinter = LT.toStrict . displayT . renderPretty 0.4 100

stext :: Data.Text.Text -> Doc
stext = text . LT.fromStrict

stextLower :: Data.Text.Text -> Doc
stextLower = text . LT.fromStrict . (\x -> C.toLower (T.head x) `T.cons` T.tail x)

stextLowerSuffix :: Data.Text.Text -> Data.Text.Text -> Doc
stextLowerSuffix suffix = text . LT.fromStrict . (\x -> C.toLower (T.head x) `T.cons` T.tail x `T.append` suffix)

stextSuffix :: Data.Text.Text -> Data.Text.Text -> Doc
stextSuffix suffix = text . LT.fromStrict . (`T.append` suffix)

spaceparens :: Doc -> Doc
spaceparens doc = "(" <+> doc <+> ")"

-- | Parentheses of which the right parenthesis exists on a new line
newlineparens :: Doc -> Doc
newlineparens doc = "(" <> doc <$$> ")"

-- | An empty line, regardless of current indentation
emptyline :: Doc
emptyline = nest minBound linebreak

-- | Like <$$>, but with an empty line in between
(<$+$>) :: Doc -> Doc -> Doc
l <$+$> r = l <> emptyline <$$> r

-- TODO Replace require / imports everywhere!

--
type RenderM = RWS Options (Set Text -- The set of instances
                            , [Text] -- Generated declarations
                            ) (Maybe Text) -- The type of the current module

{-| Add an instance to the set.
-}
require :: Text -> RenderM ()
require dep = tell (S.singleton dep, [])

{-| Take the result of a RenderM computation and put it into the Writer's
declarations.
-}
collectDeclaration :: RenderM Doc -> RenderM ()
collectDeclaration =
  mapRWS ((\(defn, s, (imports, _)) -> ((), s, (imports, [pprinter defn]))))

squarebracks :: Doc -> Doc
squarebracks doc = "[" <+> doc <+> "]"

pair :: Doc -> Doc -> Doc
pair l r = spaceparens $ l <> comma <+> r

reservedKeywords :: [T.Text]
reservedKeywords = ["and","as","assertbegin","constraint","done","downto","end"
                   ,"exception","external","for","fun","function","functor","in","include"
                   ,"inherit","initializer","let","match","method","module","mutable","of"
                   ,"open","or","struct","to","true","try","type","virtual","while","with"]

maybeReserved :: T.Text -> T.Text
maybeReserved name | name `elem` reservedKeywords = name <> "_"
                   | otherwise = name
