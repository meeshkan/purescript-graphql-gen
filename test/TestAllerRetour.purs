module Test.TestAllerRetour where

import Prelude
import Data.Either (either)
import Data.GraphQL.AST as AST
import Data.GraphQL.Gen (genDocument)
import Data.GraphQL.Parser as GP
import Data.Lens (view)
import Data.Lens.Common (simple)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (length)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Random.LCG (mkSeed)
import Test.QuickCheck.Gen (evalGen)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Text.Parsing.Parser (runParser)

parseDocument :: String -> Aff AST.Document
parseDocument t = (liftEffect $ readTextFile UTF8 t) >>= parseString

parseString :: String -> Aff AST.Document
parseString rtf = liftEffect (either (throw <<< show) pure (runParser rtf GP.document))

defLen :: AST.Document -> Int
defLen = length <<< view (simple _Newtype)

docTest :: String -> Aff Unit
docTest ds = do
  doc <- parseDocument ds
  gen <-
    pure
      $ evalGen (genDocument doc) { newSeed: mkSeed 0, size: 10 }
  doc2 <- parseString gen
  doc `shouldEqual` doc2

testAllerRetour :: forall m. Monad m => SpecT Aff Unit m Unit
testAllerRetour = do
  describe "Aller-retour with graphql specs works" do
    it "should convert to and back correctly" do
      docTest "schemas/swapi.graphql"
    it "should convert to and back correctly" do
      docTest "schemas/codesandbox.graphql"
