{- HLINT ignore "Avoid lambda" -}

module Main (main) where

import XLam (evaluateWorkbook, writeWorkbook)
import XLam.Lang(XLam(..), (<:>), toFormula)
import XLam.Library(fibRecursive)

main :: IO ()
main = do
  let e = fibRecursive <:> Number 30
      formula = toFormula e
  writeWorkbook "template.xlsx" "input.xlsx" formula
  print =<< evaluateWorkbook "input.xlsx"
