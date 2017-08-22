

import Prelude
import qualified NanoParsec as NP

main :: IO ()
main = do
  putStr "> "
  a <- getLine
  print $ NP.eval $ NP.run a
  main

testFoo = NP.foo