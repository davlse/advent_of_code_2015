{-# LANGUAGE OverloadedStrings #-}

import Data.Bits
-- This module depends on the package cryptohash. 
import Crypto.Hash.MD5
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Char8 as B

mineAdventCoins :: B.ByteString -> Int
mineAdventCoins str = head $ dropWhile pred [1..]
  where pred = not
             . all (==0)
             . zipWith (.&.) [0xFF,0xFF,0xF0] 
             . BS.unpackBytes
             . B.take 3 
             . hashing str
                

hashing :: B.ByteString -> Int -> B.ByteString
hashing str = hash . B.append str . B.pack . show 

test1 = mineAdventCoins "abcdef" == 609043
     && mineAdventCoins "pqrstuv" == 1048970

{-

--- Part Two ---

Now find one that starts with six zeroes.

-}

mineAdventCoins2 :: B.ByteString -> Int
mineAdventCoins2 str = head $ dropWhile pred [1..]
  where pred = not . B.all (=='\0') . B.take 3 . hashing str

