module XLam.Lang
  ( XLam(..)
  , (<:>)
  , toFormula
  )
where

import Data.String.Conversions (cs)
import Data.Text (Text, intercalate)

newtype Var = VarI { unVar :: Int }

data XLam
  = Var Var
  | Number Float
  | Abs (Var -> XLam)
  | App XLam XLam
  | Function Text [XLam]
  | Infix Text [XLam]

infixl 9 <:>
(<:>) :: XLam -> XLam -> XLam
(<:>) = App

paren :: Text -> Text
paren t = "(" <> t <> ")"

toFormula :: XLam -> Text
toFormula = go 0
  where
    go :: Int -> XLam -> Text
    go _ (Var x) = "_xlpm.x" <> cs (show $ unVar x)
    go _ (Number n) = cs $ show n
    go ct (Abs e) = "_xlfn.LAMBDA(_xlpm.x" <> cs (show ct) <> ", (" <> go (1 + ct) (e (VarI ct)) <> "))"
    go ct (App e1 e2) = go ct e1 <> "(" <> go ct e2 <> ")"
    go ct (Function f es) = f <> "(" <> intercalate ", " (paren . go ct <$> es) <> ")"
    go ct (Infix op es) = intercalate (" " <> op <> " ") $ paren . go ct <$> es
