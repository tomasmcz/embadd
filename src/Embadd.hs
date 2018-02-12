module Embadd where

import Control.Arrow (second)
import Data.Function (on)
import Data.List (foldl', sortBy)
import Data.Map.Strict (Map, empty, findWithDefault, insertWith, size)
import qualified Data.Map.Strict as M (fromList, toList, size, lookup)
import Data.Array (listArray, elems)

import Numeric.LinearAlgebra as LA
import System.Console.Readline

import Numeric.Statistics.PCA

import Graphics.Rendering.Chart hiding (Vector)
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Default.Class
import Control.Lens hiding ((<.>))

type VDict = Map String (Vector R)

data Expr = Wrd String 
          | Plus Expr Expr
          | UnMinus Expr
          | NullExpr

cosineD a b = (a LA.<.> b) / (norm_2 a * norm_2 b)

chart ws = toRenderable layout
  where
    layout = layout_title .~ ""
           $ layout_plots .~ [toPlot texts]
           $ def

    texts = plot_annotation_values .~ ws
          $ plot_annotation_angle .~ 0 -- degrees
          $ plot_annotation_style .~ fontStyle
          $ plot_annotation_hanchor .~ HTA_Centre -- the horizontal anchor around which the text is rotated
          $ plot_annotation_vanchor .~ VTA_Centre -- the vertical anchor
          $ def

    fontStyle = font_size .~ 16
              $ font_weight .~ FontWeightBold
              $ def

parseExpr :: Expr -> [String] -> Expr
parseExpr NullExpr ("-":w:ws) = parseExpr (UnMinus (Wrd w)) ws
parseExpr NullExpr (w:ws) = parseExpr (Wrd w) ws
parseExpr e ("+":w:ws) = parseExpr (Plus e (Wrd w)) ws
parseExpr e ("-":w:ws) = parseExpr (Plus e (UnMinus (Wrd w))) ws
parseExpr e [] = e
parseExpr _ _ = NullExpr

sumExpr vecDict (Plus e1 e2) = do
  a <- sumExpr vecDict e1
  b <- sumExpr vecDict e2
  return $ a + b
sumExpr vecDict (UnMinus e) = do
  x <- sumExpr vecDict e
  return $ - x
sumExpr vecDict (Wrd w) = M.lookup w vecDict

nearest vDict v1 = map fst . sortBy (flip compare `on` (cosineD v1 . snd)) $ M.toList vDict

eWrds (Wrd w) = [w]
eWrds (UnMinus e) = eWrds e
eWrds (Plus a b) = eWrds a ++ eWrds b

readEvalPrintLoop :: VDict -> IO ()
readEvalPrintLoop vecDict = do
  maybeLine <- readline "> "
  case words <$> maybeLine of
    Nothing     -> return () -- EOF / control-d
    Just ("PCA":wrds) -> do
      addHistory $ unwords ("PCA":wrds)
      case mapM (`M.lookup` vecDict) wrds :: Maybe [Vector R] of
        Nothing -> putStrLn "error: OOV"
        Just vcs -> do
          let vData = listArray (1, LA.size $ head vcs) . toRows $ fromColumns vcs
              reduced = toColumns . fromRows . elems . pcaTransform vData . snd $ pcaN vData 2
          print reduced
          renderableToFile def "pca.png" . chart . map (\(w,v) -> (v ! 0, v ! 1, w)) $ zip wrds reduced
          return ()
      readEvalPrintLoop vecDict
    Just wrds -> do
      addHistory $ unwords wrds
      case parseExpr NullExpr wrds of
        NullExpr -> putStrLn "bad parse"
        e -> case sumExpr vecDict e of
          Just eSum -> putStrLn . unlines . take 7 . filter (\x -> (x `notElem` eWrds e)) $ nearest vecDict eSum
          Nothing -> putStrLn "error: OOV"
      readEvalPrintLoop vecDict

computeNorm :: FilePath -> IO ()
computeNorm vectorFile = do
  vectorCont <- readFile vectorFile
  let vLine (w:ws) = (w, vector $ map read ws :: Vector Double)
      norms = map (second (show . norm_2) . vLine . words) . drop 1 . lines $ vectorCont
  mapM_ (putStrLn .  (\(a, b) -> a ++ "\t" ++ b)) norms

