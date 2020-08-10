module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.QuickCheck (quickCheck')
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.TestAllerRetour (testAllerRetour)
import Test.TestOperationDefinition (testOperationDefinition)

main :: Effect Unit
main =
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Randomly generated operation definitions clear" do
          it "should test the op 100 times" do
            quickCheck' 100 testOperationDefinition
        testAllerRetour
