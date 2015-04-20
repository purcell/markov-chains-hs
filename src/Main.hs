{-# LANGUAGE OverloadedStrings #-}
module Main ( markov
            , main
            ) where

import           Control.Applicative ((<$>))
import           Data.List           (tails)
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           System.IO           (stdin)
import qualified System.Random       as R

type ChainState a = [a]
type ChainTransitions a = M.Map (ChainState a) [ChainState a]

buildChains :: Ord a => Int -> [a] -> ChainTransitions a
buildChains len xs = foldr step M.empty allChains
  where
    allChains = filter ((len <=) . length) $ take len <$> tails xs
    step ch m = let (prior, next:_) = splitAt (len - 1) ch in
      M.insertWith (++) prior [tail prior ++ [next]] m

randomEntry :: R.RandomGen g => g -> [a] -> (a, g)
randomEntry rand options =
  let (idx, rand') = R.randomR (0, (length options - 1)) rand
  in (options !! idx, rand')

nextState :: (Ord a, R.RandomGen g) => ChainTransitions a -> (ChainState a, g) -> (ChainState a, g)
nextState chains (state, g) = case M.lookup state chains of
  Just nextStates -> randomEntry g nextStates
  Nothing -> randomEntry g $ M.keys chains

generate :: (Ord a, R.RandomGen g) => ChainTransitions a -> g -> [a]
generate chains rand = (head . fst) <$> tail (iterate (nextState chains) ([], rand))

markov :: (Ord a, R.RandomGen g) => Int -> [a] -> g -> [a]
markov len input = generate (buildChains len input)

main :: IO ()
main = do
  text <- TIO.hGetContents stdin
  generated <- markov 3 (T.words text) <$> R.getStdGen
  mapM_ (\w -> putStr (T.unpack w ++ " ")) generated
