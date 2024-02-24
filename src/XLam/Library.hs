{- HLINT ignore "Redundant bracket" -}

module XLam.Library
  ( fibRecursive
  , z
  )
where

import XLam.Lang (XLam(..), (<:>))

z :: XLam
z = Abs \f -> (Abs \x -> Var f <:> (Abs \v -> Var x <:> Var x <:> Var v)) <:>
              (Abs \x -> Var f <:> (Abs \v -> Var x <:> Var x <:> Var v))

fibRecursive :: XLam
fibRecursive = z <:> (Abs \fib -> Abs \n ->
                         Function "IF" [ Infix "<=" [Var n, Number 1]
                                       , Var n
                                       , Infix "+" [ Var fib <:> Infix "-" [Var n, Number 1]
                                                   , Var fib <:> Infix "-" [Var n, Number 2]
                                                   ]
                                       ])
