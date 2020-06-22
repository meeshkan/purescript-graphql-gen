module Data.GraphQL.Gen where

import Prelude

import Data.Foldable (fold)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser as GP
import Data.List (List)
import Data.NonEmpty (NonEmpty(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import Test.QuickCheck.Gen (Gen, oneOf, elements, arrayOf)

whitespace ∷ Gen String
whitespace = (arrayOf $ pure ' ') >>= pure <<< fromCharArray

-- arrayOf -> Gen (Array Char)
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

__genListish :: String -> String -> List String -> List (Gen String)
__genListish open close lst = map (\x -> (<>) <$> ignorable <*> pure x) lst

_genListish :: String -> String -> List String -> Gen (List String)
_genListish open close lst = sequence $ __genListish open close lst

genListish :: String -> String -> List String -> Gen String
genListish open close lst = _genListish open close lst >>= pure <<< fold

genOperationDefinition :: AST.OperationDefinition -> Gen String
genOperationDefinition (AST.OperationDefinition_SelectionSet x) = genSelectionSet x
genOperationDefinition (AST.OperationDefinition_OperationType x) = genT_OperationDefinition_OperationType x

genSelectionSet :: AST.SelectionSet -> Gen String
genSelectionSet (AST.SelectionSet s) = pure "" -- replace me

genT_OperationDefinition_OperationType :: AST.T_OperationDefinition_OperationType -> Gen String
genT_OperationDefinition_OperationType _ = pure "" -- replace me