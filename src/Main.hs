{-# LANGUAGE OverloadedStrings #-}
module Main ( markov
            , main
            ) where

import           Control.Applicative            ((<$>))
import qualified Control.Monad.Trans.State.Lazy as ST
import           Data.List                      (tails)
import qualified Data.Map                       as M
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import           System.IO                      (stdin)
import qualified System.Random                  as R

type ChainState a = [a]
type ChainTransitions a = M.Map (ChainState a) [ChainState a]

buildChains :: Ord a => Int -> [a] -> ChainTransitions a
buildChains len xs = foldr step M.empty allChains
  where
    allChains = filter ((len <=) . length) $ take len <$> tails xs
    step ch m = let (prior, next:_) = splitAt (len - 1) ch in
      M.insertWith (++) prior [tail prior ++ [next]] m

randomEntry :: R.RandomGen g => [a] -> ST.State g a
randomEntry options = do
  idx <- ST.state $ R.randomR (0, length options - 1)
  return (options !! idx)

nextState :: (Ord a, R.RandomGen g) => ChainTransitions a -> ChainState a -> ST.State g (ChainState a)
nextState chains cur =  case M.lookup cur chains of
   Just nextStates -> randomEntry nextStates
   Nothing -> randomEntry $ M.keys chains

iterateM :: Monad m => (a -> m a) -> m a -> m [a]
iterateM step start = do
    first <- start
    rest <- iterateM step (step first)
    return (first:rest)

generate :: (Ord a, R.RandomGen g) => ChainTransitions a -> ST.State g [a]
generate chains = fmap head . tail <$> iterateM (nextState chains) (return [])

markov :: (Ord a, R.RandomGen g) => Int -> [a] -> ST.State g [a]
markov len input = generate (buildChains len input)

main :: IO ()
main = do
  text <- TIO.hGetContents stdin
  rand <- R.getStdGen
  let generated = ST.evalState (markov 3 (T.words text)) rand
  mapM_ (\w -> putStr (T.unpack w ++ " ")) generated
