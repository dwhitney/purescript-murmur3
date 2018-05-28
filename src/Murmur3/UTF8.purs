module Murmur3.UTF8
  ( foldl
  ) where

import Prelude

import Data.Char (toCharCode)
import Data.Foldable as Foldable
import Data.Int.Bits (and, or)
import Data.Int.Bits as Int
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Tuple (Tuple(..), fst)

foldl :: forall a. (Int -> a -> a) -> a -> Array Char -> a
foldl op acc input =
  let
    helper res char = accumulate op (toCharCode char) res
  in
    Foldable.foldl helper (Tuple acc Nothing) input # fst

type Accumulator a = Tuple a (Maybe Int)

accumulate
  :: forall a
   . (Int -> a -> a)
  -> Int
  -> Tuple a (Maybe Int)
  -> Tuple a (Maybe Int)
accumulate add char (Tuple acc combine) =
  case combine of
    Nothing
      | char < 0x80 -> Tuple (acc # add char) Nothing
      | char < 0x0800 ->
        Tuple
          ( acc
              # add (or 0xC0 $ zshr 6 char)
              >>> add (or 0x80 $ and 0x3F char)
          )
          Nothing
      | char < 0xD800 || char >= 0xE000 ->
        Tuple
          ( acc
              # add (or 0xE0 $ zshr 12 char)
              >>> add (or 0x80 <<< and 0x3F $ zshr 6 char)
              >>> add (or 0x80 $ and 0x3F char)
          )
          Nothing
      | otherwise -> Tuple acc (Just char)

    Just prev ->
      let
        combined :: Int
        combined =
          ( prev
              # and 0x03FF
              >>> shl 10
              >>> or (and 0x03FF char)
          )
          + 0x00010000
      in
        Tuple
          ( acc
              # add (or 0xF0 $ zshr 18 combined)
              >>> add (or 0x80 <<< and 0x3F $ zshr 12 combined)
              >>> add (or 0x80 <<< and 0x3F $ zshr 6 combined)
              >>> add (or 0x80 $ and 0x3F combined)
          )
          Nothing

shl :: Int -> Int -> Int
shl = flip Int.shl

shr :: Int -> Int -> Int
shr = flip Int.shr

zshr :: Int -> Int -> Int
zshr x n = shr x $ abs n
