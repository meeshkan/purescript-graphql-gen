module Data.GraphQL.Gen where

import Prelude

import Data.Foldable (fold)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser as GP
import Data.List (List(..), (:))
import Data.NonEmpty (NonEmpty(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import Test.QuickCheck.Gen (Gen, oneOf, elements, arrayOf)

whitespace ∷ Gen String
whitespace = (arrayOf $ pure ' ') >>= pure <<< fromCharArray

strNoNewline ∷ Gen String
strNoNewline = (arrayOf $ elements (NonEmpty '_' (GP.lower <> GP.upper <> GP.digits))) >>= pure <<< fromCharArray
 
comment ∷ Gen String
comment = (<>) <$> pure "#" <*> strNoNewline

comma ∷ Gen String
comma = pure ","

lineTerminator ∷ Gen String
lineTerminator = pure "\n"

ignorable :: Gen String
ignorable = oneOf $ NonEmpty whitespace [comment, comma, lineTerminator]

_genListish :: List String -> Gen (List String)
_genListish lst = sequence $ map (\x -> (<>) <$> ignorable <*> pure x) lst

genListish :: String -> String -> List String -> Gen String
genListish open close lst = (_genListish lst) >>= \l -> 
    (sequence $ ((pure open : (map pure l)) <> (ignorable : pure close : Nil))) >>= pure <<< fold

genOperationDefinition :: AST.OperationDefinition -> Gen String
genOperationDefinition (AST.OperationDefinition_SelectionSet x) = genSelectionSet x
genOperationDefinition (AST.OperationDefinition_OperationType x) = genT_OperationDefinition_OperationType x

genSelectionSet :: AST.SelectionSet -> Gen String
genSelectionSet (AST.SelectionSet s) = pure "" -- replace me

genT_OperationDefinition_OperationType :: AST.T_OperationDefinition_OperationType -> Gen String
genT_OperationDefinition_OperationType _ = pure "" -- replace me