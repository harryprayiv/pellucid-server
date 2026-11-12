module Main (main) where

import Test.Domain qualified
import Test.Flow qualified
import Test.Scrub qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain (testGroup "pellucid-server" [Test.Domain.tests, Test.Scrub.tests, Test.Flow.tests])
