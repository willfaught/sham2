module Test (runTests) where

import Test.HUnit
import Test.Parse

runTests :: IO Counts
runTests = runTestTT $ test [
    {-checkTests,
    contextTests,-}
    parseTests{-,
    reduceTests,
    substituteTests,
    syntaxTests-}
  ]
