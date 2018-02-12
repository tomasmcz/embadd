module Main where

import Embadd

import qualified Data.Map.Strict as M (fromList, toList, size, lookup)
import Numeric.LinearAlgebra as LA
import System.Environment

main :: IO ()
main = do
  (vectorFile:args) <- getArgs
  case vectorFile of
    "--help" -> putStrLn "Usage: ./emb.hs FILE"
    "NORM" -> computeNorm (head args)
    _ -> do
      vectorCont <- readFile vectorFile
      let vLine (w:ws) = (w, vector $ map read ws :: Vector Double)
          vectorDict = M.fromList . map (vLine . words) . drop 1 . lines $ vectorCont
      putStrLn "Embedding vizualization tool"
      putStrLn "============================"
      putStrLn ""
      putStrLn "W1 + W2 - W3 -> nearest neighbours of v(W1) + v(W2) - v(W3); you can use any expression with words, '+' and '-'"
      putStrLn "PCA W1 W2 ... Wn  -> pca.png will contain {v(W1), ..., v(Wn)} projected to their principal components"
      readEvalPrintLoop vectorDict
