import Criterion
import Criterion.Main

import Lib (evaluate, parse, Expr(..))

main :: IO ()
main = defaultMain
    [ bench "evaluate (Logarithm (Number 10) (Number 1000))"
        (whnf evaluate (Logarithm (Number (10 :: Double)) (Number 1000)))
    , bench "parse 'log 1000'" (whnf parse "log (999 + 1)") ]
