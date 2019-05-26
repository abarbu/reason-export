{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ExportSpec where

import qualified Data.Algorithm.Diff as Diff
import qualified Data.Algorithm.DiffOutput as DiffOutput
import Data.Char
import Data.Int
import Data.IntMap
import Data.Map
import Data.Proxy
import Data.Text hiding (head, lines, unlines)
import Data.Time
import Reason
import GHC.Generics
import Test.HUnit (Assertion, assertBool)
import Test.Hspec hiding (Spec)
import Test.Hspec as Hspec
import Text.Printf

-- Debugging hint:
-- ghci> import GHC.Generics
-- ghci> :kind! Rep Post
-- ...
data Post = Post
  { id :: Int
  , name :: String
  , age :: Maybe Double
  , comments :: [Comment]
  , promoted :: Maybe Comment
  , author :: Maybe String
  } deriving (Generic, ReasonType)

data Comment = Comment
  { postId :: Int
  , text :: Text
  , mainCategories :: (String, String)
  , published :: Bool
  , created :: UTCTime
  , tags :: Map String Int
  } deriving (Generic, ReasonType)

data Position
  = Beginning
  | Middle
  | End
  deriving (Generic, ReasonType)

data Timing
  = Start
  | Continue Double
  | Stop
  deriving (Generic, ReasonType)

data Monstrosity
  = NotSpecial
  | OkayIGuess Monstrosity
  | Ridiculous Int String [Monstrosity]
  deriving (Generic, ReasonType)

newtype Useless =
  Useless ()
  deriving (Generic, ReasonType)

data Unit = Unit
  deriving (Generic, ReasonType)

newtype Wrapper = Wrapper Int
  deriving (Generic, ReasonType)

newtype FavoritePlaces = FavoritePlaces
  { positionsByUser :: Map String [Position]
  } deriving (Generic, ReasonType)

-- | We don't actually use this type, we just need to see that it compiles.
data LotsOfInts = LotsOfInts
  { intA :: Int8
  , intB :: Int16
  , intC :: Int32
  , intD :: Int64
  } deriving (Generic, ReasonType)


data Shadowing = Shadowing
  { prop :: ( (Int, Int), ( String, String ) )
  } deriving (Generic, ReasonType)


spec :: Hspec.Spec
spec = do
  toReasonTypeSpec
  toReasonDecoderSpec
  toReasonEncoderSpec
  moduleSpecsSpec

toReasonTypeSpec :: Hspec.Spec
toReasonTypeSpec =
  describe "Convert to Reason types." $ do
    it "toReasonTypeSource Post" $
      shouldMatchTypeSource
        (unlines
           ["open CommentType"
           , ""
           ,"%s"])
        defaultOptions
        (Proxy :: Proxy Post)
        "test/PostType.re"
    it "toReasonTypeSource Comment" $
      shouldMatchTypeSource
        (unlines
           [ "module Map_651798451719185100 = Map.Make({ type t = string; let compare = compare });"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Comment)
        "test/CommentType.re"
    it "toReasonTypeSource Position" $
      shouldMatchTypeSource
        (unlines ["%s"])
        defaultOptions
        (Proxy :: Proxy Position)
        "test/PositionType.re"
    it "toReasonTypeSource Timing" $
      shouldMatchTypeSource
        (unlines ["%s"])
        defaultOptions
        (Proxy :: Proxy Timing)
        "test/TimingType.re"
    it "toReasonTypeSource Monstrosity" $
      shouldMatchTypeSource
        (unlines ["%s"])
        defaultOptions
        (Proxy :: Proxy Monstrosity)
        "test/MonstrosityType.re"
    it "toReasonTypeSource Useless" $
      shouldMatchTypeSource
        (unlines ["%s"])
        defaultOptions
        (Proxy :: Proxy Useless)
        "test/UselessType.re"
    it "toReasonTypeSource Unit" $
      shouldMatchTypeSource
        (unlines ["%s"])
        defaultOptions
        (Proxy :: Proxy Unit)
        "test/UnitType.re"
    it "toReasonTypeSource Wrapper" $
      shouldMatchTypeSource
        (unlines ["%s"])
        defaultOptions
        (Proxy :: Proxy Wrapper)
        "test/WrapperType.re"
    it "toReasonTypeSource FavoritePlaces" $
      shouldMatchTypeSource
        (unlines
           [ "open PositionType;"
           , "module Map_651798451719185100 = Map.Make({ type t = string; let compare = compare });"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy FavoritePlaces)
        "test/FavoritePlacesType.re"
    it "toReasonTypeSourceWithOptions Post" $
      shouldMatchTypeSource
        (unlines
           [ "open CommentType;"
           , ""
           , "%s"
           ])
        (defaultOptions {fieldLabelModifier = withPrefix "post"})
        (Proxy :: Proxy Post)
        "test/PostTypeWithOptions.re"
    it "toReasonTypeSourceWithOptions Comment" $
      shouldMatchTypeSource
        (unlines
           [ "module Map_651798451719185100 = Map.Make({ type t = string; let compare = compare });"
           , ""
           , "%s"
           ])
        (defaultOptions {fieldLabelModifier = withPrefix "comment"})
        (Proxy :: Proxy Comment)
        "test/CommentTypeWithOptions.re"
    it "toReasonTypeSource Shadowing" $
      shouldMatchTypeSource
        (unlines
          [ "%s"
          ])
        defaultOptions
        (Proxy :: Proxy Shadowing)
        "test/ShadowingType.re"
    describe "Convert to Reason type references." $ do
      it "toReasonTypeRef Post" $
        toReasonTypeRef (Proxy :: Proxy Post) `shouldBe` "post"
      it "toReasonTypeRef [Comment]" $
        toReasonTypeRef (Proxy :: Proxy [Comment]) `shouldBe` "list (comment)"
      it "toReasonTypeRef (comment, String)" $
        toReasonTypeRef (Proxy :: Proxy (Comment, String)) `shouldBe`
        "(comment, string)"
      it "toReasonTypeRef String" $
        toReasonTypeRef (Proxy :: Proxy String) `shouldBe` "string"
      it "toReasonTypeRef (Maybe String)" $
        toReasonTypeRef (Proxy :: Proxy (Maybe String)) `shouldBe` "option (string)"
      it "toReasonTypeRef [Maybe String]" $
        toReasonTypeRef (Proxy :: Proxy [Maybe String]) `shouldBe`
        "list (option (string))"
      it "toReasonTypeRef (Map String (Maybe String))" $
        toReasonTypeRef (Proxy :: Proxy (Map String (Maybe String))) `shouldBe`
        "Map_651798451719185100.t (option (string))"
      it "toReasonTypeRef (IntMap (Maybe String))" $
        toReasonTypeRef (Proxy :: Proxy (IntMap (Maybe String))) `shouldBe`
        "Map_4250183808575061913.t (option (string))"

toReasonDecoderSpec :: Hspec.Spec
toReasonDecoderSpec =
  describe "Convert to Reason decoders." $ do
    it "toReasonDecoderSource Comment" $
      shouldMatchDecoderSource
        (unlines
           [ "open CommentType;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Comment)
        "test/CommentDecoder.re"
    it "toReasonDecoderSource Post" $
      shouldMatchDecoderSource
        (unlines
           [ "open PostType;"
           , "open CommentDecoder;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Post)
        "test/PostDecoder.re"
    it "toReasonDecoderSourceWithOptions Post" $
      shouldMatchDecoderSource
        (unlines
           [ "open PostTypeWithOptions;"
           , "open CommentDecoder;"
           , ""
           , "%s"
           ])
        (defaultOptions {fieldLabelModifier = withPrefix "post"})
        (Proxy :: Proxy Post)
        "test/PostDecoderWithOptions.re"
    it "toReasonDecoderSource Position" $
      shouldMatchDecoderSource
        (unlines
           [ "open PositionType;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Position)
        "test/PositionDecoder.re"
    it "toReasonDecoderSource Timing" $
      shouldMatchDecoderSource
        (unlines
           [ "open TimingType;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Timing)
        "test/TimingDecoder.re"
    it "toReasonDecoderSource Monstrosity" $
      shouldMatchDecoderSource
        (unlines
           [ "open MonstrosityType;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Monstrosity)
        "test/MonstrosityDecoder.re"
    it "toReasonDecoderSourceWithOptions Comment" $
      shouldMatchDecoderSource
        (unlines
           [ "open CommentTypeWithOptions;"
           , ""
           , "%s"
           ])
        (defaultOptions {fieldLabelModifier = withPrefix "comment"})
        (Proxy :: Proxy Comment)
        "test/CommentDecoderWithOptions.re"
    it "toReasonDecoderSource Useless" $
      shouldMatchDecoderSource
        (unlines
           [ "open UselessType;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Useless)
        "test/UselessDecoder.re"
    it "toReasonDecoderSource Unit" $
      shouldMatchDecoderSource
        (unlines
           [ "open UnitType;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Unit)
        "test/UnitDecoder.re"
    it "toReasonDecoderSource Wrapper" $
      shouldMatchDecoderSource
        (unlines
           [ "open WrapperType;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Wrapper)
        "test/WrapperDecoder.re"
    it "toReasonDecoderSource Shadowing" $
      shouldMatchDecoderSource
        (unlines
            [ "open ShadowingType;"
           , ""
           , "%s"
            ])
        defaultOptions
        (Proxy :: Proxy Shadowing)
        "test/ShadowingDecoder.re"
    describe "Convert to Reason decoder references." $ do
      it "toReasonDecoderRef Post" $
        toReasonDecoderRef (Proxy :: Proxy Post) `shouldBe` "decodePost"
      it "toReasonDecoderRef Position" $
        toReasonDecoderRef (Proxy :: Proxy Position) `shouldBe` "decodePosition"
      it "toReasonDecoderRef Timing" $
        toReasonDecoderRef (Proxy :: Proxy Timing) `shouldBe` "decodeTiming"
      it "toReasonDecoderRef Monstrosity" $
        toReasonDecoderRef (Proxy :: Proxy Monstrosity) `shouldBe` "decodeMonstrosity"
      it "toReasonDecoderRef [Comment]" $
        toReasonDecoderRef (Proxy :: Proxy [Comment]) `shouldBe`
        "Json.Decode.list(decodeComment)"
      it "toReasonDecoderRef String" $
        toReasonDecoderRef (Proxy :: Proxy String) `shouldBe` "Json.Decode.string"
      it "toReasonDecoderRef (Maybe String)" $
        toReasonDecoderRef (Proxy :: Proxy (Maybe String)) `shouldBe`
        "Json.Decode.optional(Json.Decode.string)"
      it "toReasonDecoderRef [Maybe String]" $
        toReasonDecoderRef (Proxy :: Proxy [Maybe String]) `shouldBe`
        "Json.Decode.list(Json.Decode.optional(Json.Decode.string))"
      it "toReasonDecoderRef (Map String (Maybe String))" $
        toReasonDecoderRef (Proxy :: Proxy (Map String (Maybe String))) `shouldBe`
        "Json.Decode.map(l => List.fold_left ((m,(k,v)) => Map_651798451719185100.add(k,v,m)\n                                    ,Map_651798451719185100.empty\n                                    ,l)\n               ,Json.Decode.list(Json.Decode.tuple2(Json.Decode.string,Json.Decode.optional(Json.Decode.string))))"
      it "toReasonDecoderRef (IntMap (Maybe String))" $
        toReasonDecoderRef (Proxy :: Proxy (IntMap (Maybe String))) `shouldBe`
        "Json.Decode.map(l => List.fold_left ((m,(k,v)) => Map_4250183808575061913.add(k,v,m)\n                                    ,Map_4250183808575061913.empty\n                                    ,l)\n               ,Json.Decode.list(Json.Decode.tuple2(Json.Decode.int,Json.Decode.optional(Json.Decode.string))))"

toReasonEncoderSpec :: Hspec.Spec
toReasonEncoderSpec =
  describe "Convert to Reason encoders." $ do
    it "toReasonEncoderSource Comment" $
      shouldMatchEncoderSource
        (unlines
           [ "open CommentType;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Comment)
        "test/CommentEncoder.re"
    it "toReasonEncoderSource Post" $
      shouldMatchEncoderSource
        (unlines
           [ "open PostType;"
           , "open CommentEncoder;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Post)
        "test/PostEncoder.re"
    it "toReasonEncoderSourceWithOptions Comment" $
      shouldMatchEncoderSource
        (unlines
           [ "open CommentTypeWithOptions;"
           , ""
           , "%s"
           ])
        (defaultOptions {fieldLabelModifier = withPrefix "comment"})
        (Proxy :: Proxy Comment)
        "test/CommentEncoderWithOptions.re"
    it "toReasonEncoderSourceWithOptions Post" $
      shouldMatchEncoderSource
        (unlines
           [ "open PostTypeWithOptions;"
           , "open CommentEncoder;"
           , ""
           , "%s"
           ])
        (defaultOptions {fieldLabelModifier = withPrefix "post"})
        (Proxy :: Proxy Post)
        "test/PostEncoderWithOptions.re"
    it "toReasonEncoderSource Position" $
      shouldMatchEncoderSource
        (unlines
           [ "open PositionType;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Position)
        "test/PositionEncoder.re"
    it "toReasonEncoderSource Position" $
      shouldMatchEncoderSource
        (unlines
            [ "open ShadowingType;"
            , ""
            , "%s"
            ])
        defaultOptions
        (Proxy :: Proxy Shadowing)
        "test/ShadowingEncoder.re"
    it "toReasonEncoderSourceWithOptions Timing" $
      shouldMatchEncoderSource
        (unlines
           [ "open TimingType;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Timing)
        "test/TimingEncoder.re"
    it "toReasonEncoderSourceWithOptions Monstrosity" $
      shouldMatchEncoderSource
        (unlines
           [ "open MonstrosityType;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Monstrosity)
        "test/MonstrosityEncoder.re"
    it "toReasonEncoderSourceWithOptions Useless" $
      shouldMatchEncoderSource
        (unlines
           [ "open UselessType;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Useless)
        "test/UselessEncoder.re"
    it "toReasonEncoderSourceWithOptions Unit" $
      shouldMatchEncoderSource
        (unlines
           [ "open UnitType;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Unit)
        "test/UnitEncoder.re"
    it "toReasonEncoderSourceWithOptions Wrapper" $
      shouldMatchEncoderSource
        (unlines
           [ "open WrapperType;"
           , ""
           , "%s"
           ])
        defaultOptions
        (Proxy :: Proxy Wrapper)
        "test/WrapperEncoder.re"
    describe "Convert to Reason encoder references." $ do
      it "toReasonEncoderRef Post" $
        toReasonEncoderRef (Proxy :: Proxy Post) `shouldBe` "encodePost"
      it "toReasonEncoderRef [Comment]" $
        toReasonEncoderRef (Proxy :: Proxy [Comment]) `shouldBe`
        "(Json.Encode.list(encodeComment))"
      it "toReasonEncoderRef Position" $
        toReasonEncoderRef (Proxy :: Proxy Position) `shouldBe` "encodePosition"
      it "toReasonEncoderRef Timing" $
        toReasonEncoderRef (Proxy :: Proxy Timing) `shouldBe` "encodeTiming"
      it "toReasonEncoderRef Monstrosity" $
        toReasonEncoderRef (Proxy :: Proxy Monstrosity) `shouldBe` "encodeMonstrosity"
      it "toReasonEncoderRef String" $
        toReasonEncoderRef (Proxy :: Proxy String) `shouldBe` "Json.Encode.string"
      it "toReasonEncoderRef (Maybe String)" $
        toReasonEncoderRef (Proxy :: Proxy (Maybe String)) `shouldBe`
        "(Json.Encode.nullable(Json.Encode.string))"
      it "toReasonEncoderRef [Maybe String]" $
        toReasonEncoderRef (Proxy :: Proxy [Maybe String]) `shouldBe`
        "(Json.Encode.list((Json.Encode.nullable(Json.Encode.string))))"
      it "toReasonEncoderRef (Map String (Maybe String))" $
        toReasonEncoderRef (Proxy :: Proxy (Map String (Maybe String))) `shouldBe`
        "((x) => Json.Encode.list(Json.Encode.tuple2(Json.Encode.string\n                                           ,(Json.Encode.nullable(Json.Encode.string))))(Map_651798451719185100.bindings(x)))"
      it "toReasonEncoderRef (IntMap (Maybe String))" $
        toReasonEncoderRef (Proxy :: Proxy (IntMap (Maybe String))) `shouldBe`
        "((x) => Json.Encode.list(Json.Encode.tuple2(Json.Encode.int\n                                           ,(Json.Encode.nullable(Json.Encode.string))))(Map_4250183808575061913.bindings(x)))"

moduleSpecsSpec :: Hspec.Spec
moduleSpecsSpec =
  describe "Generating a module Spec" $ do
    let mySpec =
          moduleSpec ["My", "Module"] $ do
            renderType (Proxy :: Proxy Post)
            renderDecoder (Proxy :: Proxy Post)
            renderType (Proxy :: Proxy Comment)
    it "sets the module namespace" $
      namespace mySpec `shouldBe` ["My", "Module"]
    it "inserts the correct imports" $
      head (declarations mySpec) `shouldBe`
      intercalate
        "\n"
        [ "import module Map_651798451719185100 = Map.Make({ type t = string; let compare = compare });" ]

shouldMatchTypeSource
  :: ReasonType a
  => String -> Options -> a -> FilePath -> IO ()
shouldMatchTypeSource wrapping options x =
  shouldMatchFile . printf wrapping $ toReasonTypeSourceWith options x

shouldMatchDecoderSource
  :: ReasonType a
  => String -> Options -> a -> FilePath -> IO ()
shouldMatchDecoderSource wrapping options x =
  shouldMatchFile . printf wrapping $ toReasonDecoderSourceWith options x

shouldMatchEncoderSource
  :: ReasonType a
  => String -> Options -> a -> FilePath -> IO ()
shouldMatchEncoderSource wrapping options x =
  shouldMatchFile . printf wrapping $ toReasonEncoderSourceWith options x

shouldMatchFile :: String -> FilePath -> IO ()
shouldMatchFile actual fileExpected = do
  source <- readFile fileExpected
  actual `shouldBeDiff` (fileExpected, source)

shouldBeDiff :: String -> (String, String) -> Assertion
shouldBeDiff a (fpath, b) =
  assertBool
    ("< generated\n" <> "> " <> fpath <> "\n" <>
     DiffOutput.ppDiff (Diff.getGroupedDiff (lines a) (lines b)))
    (a == b)

initCap :: Text -> Text
initCap t =
  case uncons t of
    Nothing -> t
    Just (c, cs) -> cons (Data.Char.toUpper c) cs

withPrefix :: Text -> Text -> Text
withPrefix prefix s = prefix <> initCap s
