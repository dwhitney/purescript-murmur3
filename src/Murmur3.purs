-- | Murmur 3 hash function for hashing strings
module Murmur3
  ( hashString
  ) where

import Prelude hiding (zero, one)

import Data.Array (foldl)
import Data.BigInt (BigInt, and, fromBase, or, xor)
import Data.BigInt as BigInt
import Data.Char (toCharCode)
import Data.Maybe (fromJust)
import Data.String (toCharArray)
import Partial.Unsafe (unsafePartial)

type HashData =
  { shift :: Number
  , seed :: BigInt
  , hash :: BigInt
  , charsProcessed :: BigInt
  }

-- | Takes a seed and a string. Produces a hash (integer).
-- |
-- | Given the same seed and string, it will always produce the same hash.
-- |
-- | ```purescript
-- | hashString 1234 "Turn me into a hash" == 4138100590
-- | ````
hashString :: BigInt -> String -> BigInt
hashString seed str =
  str
    # toCharArray
    >>> map (toCharCode >>> BigInt.fromInt)
    >>> foldl hashFold { shift: 0.0, seed, hash: zero, charsProcessed: zero }
    >>> finalize

hashFold :: HashData -> BigInt -> HashData
hashFold data_ c =
  let
    res = c
      # shl data_.shift
      >>> or data_.hash
  in
    case data_.shift of
      24.0 ->
        let
          newHash =
            res
              # mix data_.seed
              >>> step
        in
          { shift: 0.0
          , seed: newHash
          , hash: zero
          , charsProcessed: data_.charsProcessed + one
          }
      _ ->
        { shift: data_.shift + 8.0
        , seed: data_.seed
        , hash: res
        , charsProcessed: data_.charsProcessed + one
        }

finalize :: HashData -> BigInt
finalize data_ =
  let
    acc =
      if data_.hash /= zero
        then mix data_.seed data_.hash
        else data_.seed

    h1 = acc `xor` data_.charsProcessed

    h2 = h1
      # shr 16.0 -- zshr
      >>> xor h1
      >>> mur x85EBCA6B

    h3 = h2
      # shr 13.0 -- zshr
      >>> xor h2
      >>> mur xC2B2AE35
    in
      h3
        # shr 16.0 -- zshr
        >>> xor h3
        >>> shr 0.0 -- zshr

mix :: BigInt -> BigInt -> BigInt
mix h1 h2 =
  let
    k1 = mur xCC9E2D51 h2
  in
    k1
      # shl 15.0
      >>> or (shr 17.0 k1) -- zshr
      >>> mur x1B873593
      >>> xor h1


mur :: BigInt -> BigInt -> BigInt
mur c h =
  and xFFFFFFFF ((and h xFFFF * c) + shl 16.0 (and xFFFF (shr 16.0 h * c))) -- zshr

step :: BigInt -> BigInt
step acc =
  let
    h1 = shl 13.0 acc
      # or (shr 19.0 acc) -- zshr
      >>> mur five
  in
    (and h1 xFFFF + x6B64) + shl 16.0 (and xFFFF (shr 16.0 h1 + xE654)) -- zshr

shl :: Number -> BigInt -> BigInt
shl = flip BigInt.shl

shr :: Number -> BigInt -> BigInt
shr = flip BigInt.shr

zero = BigInt.fromInt 0 :: BigInt
one = BigInt.fromInt 1 :: BigInt
five = BigInt.fromInt 5 :: BigInt

x :: String -> BigInt
x = unsafePartial $ fromJust <<< fromBase 16

x6B64 = x "6B64" :: BigInt
xE654 = x "E654" :: BigInt
xFFFF = x "FFFF" :: BigInt
x1B873593 = x "1B873593" :: BigInt
x85EBCA6B = x "85EBCA6B" :: BigInt
xC2B2AE35 = x "C2B2AE35" :: BigInt
xCC9E2D51 = x "CC9E2D51" :: BigInt
xFFFFFFFF = x "FFFFFFFF" :: BigInt
