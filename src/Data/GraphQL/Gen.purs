module Data.GraphQL.Gen where

import Prelude
import Data.Foldable (class Foldable, fold, intercalate)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser as GP
import Data.List (List(..), (:), singleton)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (class Traversable, sequence)
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
ignorable = oneOf $ NonEmpty whitespace [ pure "", comment, comma, lineTerminator ]

mandatoryIgnorable :: Gen String
mandatoryIgnorable = oneOf $ NonEmpty whitespace [ comment, comma, lineTerminator ]

_genListish :: List String -> Gen (List String)
_genListish = sequence <<< intercalate (singleton mandatoryIgnorable) <<< map (singleton <<< pure)

genListish :: String -> String -> List String -> Gen String
genListish open close lst =
  (_genListish lst)
    >>= \l ->
        (sequence $ ((pure open : ignorable : (map pure l)) <> (ignorable : pure close : Nil))) >>= pure <<< fold

genListish' :: forall a. String -> String -> (a -> Gen String) -> List a -> Gen String
genListish' open close g lst = (sequence $ (map g lst)) >>= (genListish open close)

genOperationDefinition :: AST.OperationDefinition -> Gen String
genOperationDefinition (AST.OperationDefinition_SelectionSet x) = genSelectionSet x

genOperationDefinition (AST.OperationDefinition_OperationType x) = genT_OperationDefinition_OperationType x

genShowable :: forall n a. Newtype n a => Show a => n -> Gen String
genShowable = pure <<< show <<< unwrap

genListValue :: AST.ListValue -> Gen String
genListValue (AST.ListValue l) = genListish' "[" "]" genValue l

genObjectValue :: AST.ObjectValue -> Gen String
genObjectValue (AST.ObjectValue l) = genListish' "{" "}" genArgument l

genValue :: AST.Value -> Gen String
genValue (AST.Value_BooleanValue b) = genShowable b

genValue (AST.Value_EnumValue b) = genShowable b

genValue (AST.Value_FloatValue b) = genShowable b

genValue (AST.Value_IntValue b) = genShowable b

genValue (AST.Value_ListValue b) = genListValue b

genValue (AST.Value_NullValue b) = pure "null"

genValue (AST.Value_ObjectValue b) = genObjectValue b

genValue (AST.Value_StringValue b) = genShowable b

genValue (AST.Value_Variable b) = genShowable b

spf :: forall t. Traversable t => Foldable t => t (Gen String) -> Gen String
spf s = (sequence s) >>= pure <<< fold

genArgument :: AST.Argument -> Gen String
genArgument (AST.Argument a) = spf [ pure a.name, ignorable, pure ":", ignorable, genValue a.value ]

genDirective :: AST.Directive -> Gen String
genDirective (AST.Directive d) = spf [ pure "@", pure d.name, ignorable, maybe (pure "") genArguments d.arguments ]

-- directives are not bounded by any bracketing, thus the empty strings
genDirectives :: AST.Directives -> Gen String
genDirectives (AST.Directives d) = genListish' "" "" genDirective d

--  = { alias ∷ (Maybe String), name ∷ String, arguments ∷ (Maybe Arguments), directives ∷ (Maybe Directives), selectionSet ∷ (Maybe SelectionSet) }
genArguments :: AST.Arguments -> Gen String
genArguments (AST.Arguments l) = genListish' "(" ")" genArgument l

genAlias :: String -> Gen String
genAlias s = (sequence [ pure s, ignorable, pure ":" ]) >>= pure <<< fold

genField :: AST.Field -> Gen String
genField (AST.Field s) = pure "" -- replace me

genSelection :: AST.Selection -> Gen String
genSelection (AST.Selection_Field s) = genField s -- replace me

genSelection (AST.Selection_FragmentSpread s) = pure "" -- replace me

genSelection (AST.Selection_InlineFragment s) = pure "" -- replace me

genSelectionSet :: AST.SelectionSet -> Gen String
genSelectionSet (AST.SelectionSet s) = genListish' "{" "}" genSelection s

genT_OperationDefinition_OperationType :: AST.T_OperationDefinition_OperationType -> Gen String
genT_OperationDefinition_OperationType _ = pure "" -- replace me
