{-
 - NOTE: T9 (R) is supposedly patented
 - (http://en.wikipedia.org/wiki/T9_%28predictive_text%29). A brief reading
 - of the abstract of the patent describes a different process than the one
 - implemented here. Send your cease and desist letters to
 - cheecheeo@gmail.com.
 -
 - This program is free software. It comes without any warranty, to
 - the extent permitted by applicable law. You can redistribute it
 - and/or modify it under the terms of the Do What The Fuck You Want
 - To Public License, Version 2, as published by Sam Hocevar. See
 - http://sam.zoy.org/wtfpl/COPYING for more details.
-}

import qualified Data.Char as C
import Control.Applicative ((<$>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe as Maybe
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import qualified Data.Text as T
import Data.Enumerator (Iteratee, (>>==), (=$))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as TE
import System.IO (BufferMode(..))
import qualified System.IO as IO

import Data.Foldable (Foldable)
import qualified Data.Foldable as F
import Data.Judy (JE, JudyL, Key)
import qualified Data.Judy as J
import qualified Control.Monad as M

charToNumeric :: Monad m => Char -> m Int
charToNumeric c =
  let n = C.ord c - 65
      -- a b c -> 2
      go 0 = return 2
      go 1 = return 2
      go 2 = return 2
      go 32 = return 2
      go 33 = return 2
      go 34 = return 2
      -- d e f -> 3
      go 3 = return 3
      go 4 = return 3
      go 5 = return 3
      go 35 = return 3
      go 36 = return 3
      go 37 = return 3
      -- g h i -> 4
      go 38 = return 4
      go 39 = return 4
      go 40 = return 4
      go 6 = return 4
      go 7 = return 4
      go 8 = return 4
      -- j k l -> 5
      go 41 = return 5
      go 42 = return 5
      go 43 = return 5
      go 9 = return 5
      go 10 = return 5
      go 11 = return 5
      -- m n o -> 6
      go 44 = return 6
      go 45 = return 6
      go 46 = return 6
      go 12 = return 6
      go 13 = return 6
      go 14 = return 6
      -- p q r s -> 7
      go 47 = return 7
      go 48 = return 7
      go 49 = return 7
      go 50 = return 7
      go 15 = return 7
      go 16 = return 7
      go 17 = return 7
      go 18 = return 7
      -- t u v -> 8
      go 51 = return 8
      go 52 = return 8
      go 53 = return 8
      go 19 = return 8
      go 20 = return 8
      go 21 = return 8
      -- w x y z -> 9
      go 54 = return 9
      go 55 = return 9
      go 56 = return 9
      go 57 = return 9
      go 22 = return 9
      go 23 = return 9
      go 24 = return 9
      go 25 = return 9
      go _  = fail ("Non-letter character passed to charToNumeric: " ++ [c])
  in go n

exponentiateDecimal :: [Int] -> Int
exponentiateDecimal ns =
  (sum . zipWith (*) (reverse tens)) ns
  where tens = map (10^) [0..(length ns - 1)]

-- | Convert a word to it's numeric equivalent. For example 'toNumeric' "cat" = 228
toNumeric :: (Functor m, Monad m) => String -> m Int
toNumeric s = exponentiateDecimal <$> (mapM charToNumeric) s

possibleMatches :: IntMap a -> String -> [a]
possibleMatches table key =
  (map snd . IM.toAscList) (IM.filterWithKey (\k _ -> key `L.isPrefixOf` (show k)) table)

safeChar :: Char -> Bool
safeChar = (Maybe.isJust . charToNumeric)

safeString :: Text -> Bool
safeString = T.all safeChar

makeTable :: (Functor m, Monad m) => [Text] -> m (IntMap (Set Text))
makeTable ss = do
  hashes <- mapM (toNumeric . T.unpack) safeStrings
  return (IM.fromListWith (\x y -> S.union x y) (zip hashes (map S.singleton safeStrings)))
  where safeStrings = filter safeString ss

betterWords :: Text -> [Text]
betterWords = T.split (\c -> c `elem` " \".,?!:\n")

interactIteratee :: (Text -> Text) -> Iteratee Text IO ()
interactIteratee f =
  EL.foldM (\_ line -> (TIO.putStrLn . f) line) ()

showText :: Show a => a -> Text
showText = T.pack . show

main :: IO ()
main = do
  table <- makeTable =<< (betterWords <$> (TIO.readFile "alice_in_wonderland.txt"))
  E.run_ (TE.lines =$ interactIteratee (\input -> showText (possibleMatches table (T.unpack input))) >>== TE.enumHandle IO.stdin)
