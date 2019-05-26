# Reason Export

[![Build Status](https://travis-ci.org/abarbu/reason-export.svg)](https://travis-ci.org/abarbu/reason-export)
<img src="https://cdn.svgporn.com/logos/reasonml.svg" alt="reason" height="20"/>
<img src="https://www.haskell.org/img/haskell-logo.svg" alt="reason" height="20"/>

Create Reason classes and JSON encoders/decoders from Haskell DataTypes.
Originally build by converting [elm-export](http://hackage.haskell.org/package/elm-export) to Reason.

More docs on [Hackage](http://hackage.haskell.org/package/reason-export).

## Usage

If you're using this package you almost certainly want to use [servant-reason](http://hackage.haskell.org/package/servant-reason) as well. There are tests in both packages
that show to use this library.

Usage is trivial, derive `Generic` and `ReasonType`.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Db where

import Reason
import GHC.Generics

data Person = Person
  { id :: Int
  , name :: Maybe String
  } deriving (Show, Eq, Generic, ReasonType)
```

Then you can expose your API:

```haskell
module Main where

import Data.Proxy
import Reason
import           Data.Text hiding (intercalate, map)
import Db

main :: IO ()
main = do
  let code = defReasonImports :
            toReasonTypeSource (Proxy :: Proxy Person) :
            toReasonDecoderSource (Proxy :: Proxy Person) :
  writeFile "client/Types.re" $ intercalate "\n\n" $ map unpack code
```

That's about it. Just do this for every type that you want to expose. You can make encoders as well, and configure various settings. See [Hackage](http://hackage.haskell.org/package/reason-export).

## Reason setup

The generated Reason code needs access to [@glennsl/bs-json](https://github.com/glennsl/bs-json). Get the latest install instructions from there, but at the time of writing these were:

```sh
npm install --save @glennsl/bs-json
```

Then add `@glennsl/bs-json` to `bs-dependencies` in your `bsconfig.json`:
```js
{
  ...
  "bs-dependencies": ["@glennsl/bs-json"]
}
```
