module Test.Main where

import Prelude
import Control.Lazy (fix)
import Control.Monad.Gen (resize)
import Data.Either (either)
import Data.GraphQL.AST as AST
import Data.GraphQL.Gen as GG
import Data.GraphQL.Parser as GP
import Data.List (fromFoldable, List)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Test.QuickCheck (Result(..), arbitrary, quickCheck')
import Test.QuickCheck.Gen (Gen, arrayOf, frequency, oneOf, elements, suchThat)
import Text.Parsing.Parser (runParser)

listOf :: forall a. Gen a -> Gen (List a)
listOf x = arrayOf x >>= pure <<< fromFoldable

genMaybe :: forall a. Number -> Gen a -> Gen (Maybe a)
genMaybe i g = frequency $ NonEmpty (Tuple 1.0 $ pure Nothing) (fromFoldable [ Tuple i (g >>= pure <<< Just) ])

genMaybe3 :: forall a. Gen a -> Gen (Maybe a)
genMaybe3 = genMaybe 3.0

genOperationType :: Gen AST.OperationType
genOperationType = oneOf $ NonEmpty (pure AST.Query) [ pure AST.Mutation, pure AST.Subscription ]

genName ∷ Gen String
genName = ((<>) <$> (sequence [ (elements $ NonEmpty '_' (GP.lower <> GP.upper)) ]) <*> (arrayOf (elements $ NonEmpty '_' (GP.lower <> GP.upper <> GP.digits)))) >>= pure <<< fromCharArray

genVariable :: Gen AST.Variable
genVariable = AST.Variable <$> genName

genNamedType :: Gen AST.NamedType
genNamedType = AST.NamedType <$> genName

genListType :: Gen AST.Type -> Gen AST.ListType
genListType v = AST.ListType <$> v

genNonNullType :: Gen AST.Type -> Gen AST.NonNullType
genNonNullType v = oneOf $ NonEmpty (AST.NonNullType_NamedType <$> genNamedType) [ AST.NonNullType_ListType <$> genListType v ]

genType_NamedType :: Gen AST.Type
genType_NamedType = AST.Type_NamedType <$> genNamedType

genType_ListType :: Gen AST.Type -> Gen AST.Type
genType_ListType p = AST.Type_ListType <$> genListType p

genType_NonNullType :: Gen AST.Type -> Gen AST.Type
genType_NonNullType p = AST.Type_NonNullType <$> genNonNullType p

genType :: Gen AST.Type
genType = fix \p -> resize (\x -> max 0 $ x - 2) $ oneOf (NonEmpty genType_NamedType [ genType_ListType p, genType_NonNullType p ])

genValue_Variable :: Gen AST.Value
genValue_Variable = AST.Value_Variable <$> genVariable

genValue_IntValue :: Gen AST.Value
genValue_IntValue = AST.Value_IntValue <$> (AST.IntValue <$> arbitrary)

genValue_FloatValue :: Gen AST.Value
genValue_FloatValue = AST.Value_FloatValue <$> (AST.FloatValue <$> arbitrary)

genValue_StringValue :: Gen AST.Value
genValue_StringValue = AST.Value_StringValue <$> (AST.StringValue <$> genName) -- should open up to more possible strings

genValue_BooleanValue :: Gen AST.Value
genValue_BooleanValue = AST.Value_BooleanValue <$> (AST.BooleanValue <$> arbitrary)

genValue_NullValue :: Gen AST.Value
genValue_NullValue = AST.Value_NullValue <$> pure AST.NullValue

genValue_EnumValue :: Gen AST.Value
genValue_EnumValue = AST.Value_EnumValue <$> (AST.EnumValue <$> suchThat genName (\x -> not (x == "null" || x == "true" || x == "false")))

genValue_ListValue :: Gen AST.Value -> Gen AST.Value
genValue_ListValue v = AST.Value_ListValue <$> (AST.ListValue <$> listOf v)

genValue_ObjectValue :: Gen AST.Value -> Gen AST.Value
genValue_ObjectValue v = AST.Value_ObjectValue <$> (AST.ObjectValue <$> listOf (genArgument v))

genValue :: Gen AST.Value
genValue =
  fix \p ->
    resize (\x -> max 0 $ x - 2)
      $ oneOf
          ( NonEmpty genValue_Variable
              [ genValue_IntValue
              , genValue_FloatValue
              , genValue_StringValue
              , genValue_BooleanValue
              , genValue_NullValue
              , genValue_EnumValue
              , genValue_ListValue p
              , genValue_ObjectValue p
              ]
          )

genDefaultValue :: Gen AST.DefaultValue
genDefaultValue = AST.DefaultValue <$> genValue

genVariableDefinition :: Gen AST.VariableDefinition
genVariableDefinition =
  map AST.VariableDefinition $ { variable: _, type: _, defaultValue: _ }
    <$> genVariable
    <*> genType
    <*> genMaybe3 genDefaultValue

genVariableDefinitions :: Gen AST.VariableDefinitions
genVariableDefinitions = AST.VariableDefinitions <$> listOf genVariableDefinition

genArgument :: Gen AST.Value -> Gen AST.Argument
genArgument v =
  map AST.Argument $ { name: _, value: _ }
    <$> genName
    <*> v

genArguments :: Gen AST.Value -> Gen AST.Arguments
genArguments v = AST.Arguments <$> listOf (genArgument v)

genDirective :: Gen AST.Directive
genDirective =
  map AST.Directive $ { name: _, arguments: _ }
    <$> genName
    <*> genMaybe3 (genArguments genValue)

genDirectives :: Gen AST.Directives
genDirectives = AST.Directives <$> listOf genDirective

genField :: Gen AST.SelectionSet -> Gen AST.Field
genField ss =
  map AST.Field $ { alias: _, name: _, arguments: _, directives: _, selectionSet: _ }
    <$> genMaybe3 genName
    <*> genName
    <*> genMaybe3 (genArguments genValue)
    <*> genMaybe3 genDirectives
    <*> genMaybe3 ss

genSelection_Field :: Gen AST.SelectionSet -> Gen AST.Selection
genSelection_Field ss = AST.Selection_Field <$> (genField ss)

genFragmentSpread :: Gen AST.FragmentSpread
genFragmentSpread =
  map AST.FragmentSpread $ { fragmentName: _, directives: _ }
    <$> genName
    <*> genMaybe3 genDirectives

genSelection_FragmentSpread :: Gen AST.Selection
genSelection_FragmentSpread = AST.Selection_FragmentSpread <$> genFragmentSpread

genTypeCondition :: Gen AST.TypeCondition
genTypeCondition = AST.TypeCondition <$> (AST.NamedType <$> genName)

genSelectionSet :: Gen AST.SelectionSet
genSelectionSet = fix \p -> resize (\x -> max 0 $ x - 2) $ AST.SelectionSet <$> listOf (genSelection p)

genInlineFragment :: Gen AST.SelectionSet -> Gen AST.InlineFragment
genInlineFragment ss =
  map AST.InlineFragment $ { typeCondition: _, directives: _, selectionSet: _ }
    <$> genMaybe3 genTypeCondition
    <*> genMaybe3 genDirectives
    <*> ss

genSelection_InlineFragment :: Gen AST.SelectionSet -> Gen AST.Selection
genSelection_InlineFragment ss = AST.Selection_InlineFragment <$> (genInlineFragment ss)

genSelection :: Gen AST.SelectionSet -> Gen AST.Selection
genSelection ss = oneOf $ NonEmpty (genSelection_Field ss) [ genSelection_FragmentSpread, (genSelection_InlineFragment ss) ]

genOperationDefinition_OperationType :: Gen AST.OperationDefinition
genOperationDefinition_OperationType =
  map AST.OperationDefinition_OperationType $ { operationType: _, name: _, variableDefinitions: _, directives: _, selectionSet: _ }
    <$> genOperationType
    <*> genMaybe3 genName
    <*> genMaybe3 genVariableDefinitions
    <*> genMaybe3 genDirectives
    <*> genSelectionSet

genOperationDefinition_SelectionSet :: Gen AST.OperationDefinition
genOperationDefinition_SelectionSet = AST.OperationDefinition_SelectionSet <$> genSelectionSet

genOperationDefinition :: Gen AST.OperationDefinition
genOperationDefinition = oneOf $ NonEmpty genOperationDefinition_SelectionSet [ genOperationDefinition_OperationType ]

testOperationDefinition :: Gen Result
testOperationDefinition = do
  od0 <- genOperationDefinition
  -- because of the way parsing works, some information is lost in parsing
  -- for example, Just (Nil) will turn into Nothing after something is parsed and unparsed
  -- there is no way to preserve this distinction
  -- so, testing for equality between od0 and the new result is (unfortunately) a pain
  -- all we can do for now is make sure it parses
  -- eventually, we should do an equality comparison
  GG.genOperationDefinition od0 >>= \s -> either (\_ -> pure $ Failed ("Could not parse !! " <> show od0 <> " ^^^ " <> s)) (\_ -> pure Success) (runParser s GP.operationDefinition)

main :: Effect Unit
main = do
  quickCheck' 100 testOperationDefinition
