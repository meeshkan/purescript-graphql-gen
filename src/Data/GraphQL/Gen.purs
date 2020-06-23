module Data.GraphQL.Gen where

import Prelude
import Data.Array (fromFoldable)
import Data.Foldable (class Foldable, fold, intercalate)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser as GP
import Data.List (List(..), (:), singleton)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (class Traversable, sequence)
import Test.QuickCheck.Gen (Gen, arrayOf, arrayOf1, elements)

whitespace ∷ Gen String
whitespace = (arrayOf1 $ pure ' ') >>= pure <<< fromCharArray <<< fromFoldable

strNoNewline ∷ Gen String
strNoNewline =
  ( arrayOf
      $ elements (NonEmpty '_' (GP.lower <> GP.upper <> GP.digits))
  )
    >>= pure
    <<< fromCharArray

comment ∷ Gen String
comment = spf [ pure "#", strNoNewline, pure "\n" ]

comma ∷ Gen String
comma = pure ","

lineTerminator ∷ Gen String
lineTerminator = pure "\n"

mandatoryIgnorable :: Gen String
mandatoryIgnorable = pure " " -- oneOf $ NonEmpty whitespace [ comment, comma, lineTerminator ]

ignorable :: Gen String
ignorable = pure " " -- oneOf $ NonEmpty whitespace [ pure "", comment, comma, lineTerminator ]

_genListish :: List String -> Gen (List String)
_genListish =
  sequence
    <<< intercalate (singleton mandatoryIgnorable)
    <<< map (singleton <<< pure)

genListish :: String -> String -> List String -> Gen String
genListish open close lst =
  (_genListish lst)
    >>= \l ->
        ( sequence
            $ ( (pure open : ignorable : (map pure l)) <> (ignorable : pure close : Nil)
              )
        )
          >>= pure
          <<< fold

genListish' :: forall a. String -> String -> (a -> Gen String) -> List a -> Gen String
genListish' open close g lst = (sequence $ (map g lst)) >>= (genListish open close)

genShowable :: forall n a. Newtype n a => Show a => n -> Gen String
genShowable = pure <<< show <<< unwrap

genListValue :: AST.ListValue -> Gen String
genListValue (AST.ListValue l) = genListish' "[" "]" genValue l

genObjectValue :: AST.ObjectValue -> Gen String
genObjectValue (AST.ObjectValue l) = genListish' "{" "}" genArgument l

genValue :: AST.Value -> Gen String
genValue (AST.Value_BooleanValue b) = genShowable b

genValue (AST.Value_EnumValue b) = genEnum b

genValue (AST.Value_FloatValue b) = genShowable b

genValue (AST.Value_IntValue b) = genShowable b

genValue (AST.Value_ListValue b) = genListValue b

genValue (AST.Value_NullValue b) = pure "null"

genValue (AST.Value_ObjectValue b) = genObjectValue b

genValue (AST.Value_StringValue b) = genShowable b

genValue (AST.Value_Variable b) = genVariable b

genVariable :: AST.Variable -> Gen String
genVariable (AST.Variable b) = pure ("$" <> b)

genEnum :: AST.EnumValue -> Gen String
genEnum (AST.EnumValue b) = pure b

spf :: forall t. Traversable t => Foldable t => t (Gen String) -> Gen String
spf s = (sequence s) >>= pure <<< fold

mpg :: forall a. (a -> Gen String) -> Maybe a -> Gen String
mpg = maybe (pure "")

genArgument :: AST.Argument -> Gen String
genArgument (AST.Argument a) =
  spf
    [ pure a.name, ignorable, pure ":", ignorable, genValue a.value
    ]

genDirective :: AST.Directive -> Gen String
genDirective (AST.Directive d) =
  spf
    [ pure "@", pure d.name, ignorable, mpg genArguments d.arguments
    ]

-- directives are not bounded by any bracketing, thus the empty strings
genDirectives :: AST.Directives -> Gen String
genDirectives (AST.Directives d) = genListish' "" "" genDirective d

genArguments :: AST.Arguments -> Gen String
genArguments (AST.Arguments l) = genListish' "(" ")" genArgument l

genAlias :: String -> Gen String
genAlias s = spf [ pure s, ignorable, pure ":" ]

genField :: AST.Field -> Gen String
genField (AST.Field s) =
  spf
    [ mpg genAlias s.alias
    , ignorable
    , pure s.name
    , ignorable
    , mpg genArguments s.arguments
    , ignorable
    , mpg genDirectives s.directives
    , ignorable
    , mpg genSelectionSet s.selectionSet
    ]

genFragmentSpread :: AST.FragmentSpread -> Gen String
genFragmentSpread (AST.FragmentSpread fs) =
  spf
    [ pure "..."
    , ignorable
    , pure fs.fragmentName
    , ignorable
    , mpg genDirectives fs.directives
    ]

genTypeCondition :: AST.TypeCondition -> Gen String
genTypeCondition (AST.TypeCondition (AST.NamedType t)) = spf [ pure "on", mandatoryIgnorable, pure t ]

genInlineFragment :: AST.InlineFragment -> Gen String
genInlineFragment (AST.InlineFragment f) =
  spf
    [ pure "..."
    , ignorable
    , mpg genTypeCondition f.typeCondition
    , ignorable
    , mpg genDirectives f.directives
    , ignorable
    , genSelectionSet f.selectionSet
    ]

genSelection :: AST.Selection -> Gen String
genSelection (AST.Selection_Field s) = genField s

genSelection (AST.Selection_FragmentSpread s) = genFragmentSpread s

genSelection (AST.Selection_InlineFragment s) = genInlineFragment s

genSelectionSet :: AST.SelectionSet -> Gen String
genSelectionSet (AST.SelectionSet s) = genListish' "{" "}" genSelection s

genType :: AST.Type -> Gen String
genType (AST.Type_NamedType (AST.NamedType nt)) = pure nt

genType (AST.Type_ListType (AST.ListType lt)) = spf [ pure "[", genType lt, pure "]" ]

genType (AST.Type_NonNullType (AST.NonNullType_ListType l)) =
  genType (AST.Type_ListType l)
    >>= pure
    <<< (flip append "!")

genType (AST.Type_NonNullType (AST.NonNullType_NamedType n)) =
  genType (AST.Type_NamedType n)
    >>= pure
    <<< (flip append "!")

genDefaultValue :: AST.DefaultValue -> Gen String
genDefaultValue (AST.DefaultValue dv) = spf [ pure "=", ignorable, genValue dv ]

genVariableDefinition :: AST.VariableDefinition -> Gen String
genVariableDefinition (AST.VariableDefinition vd) =
  spf
    [ genVariable vd.variable
    , ignorable
    , pure ":"
    , ignorable
    , genType vd.type
    , ignorable
    , mpg genDefaultValue vd.defaultValue
    ]

genVariableDefinitions :: AST.VariableDefinitions -> Gen String
genVariableDefinitions (AST.VariableDefinitions vd) =
  genListish'
    "("
    ")"
    genVariableDefinition
    vd

genT_OperationDefinition_OperationType ::
  AST.T_OperationDefinition_OperationType -> Gen String
genT_OperationDefinition_OperationType o =
  spf
    [ pure
        $ case o.operationType of
            AST.Query -> "query"
            AST.Mutation -> "mutation"
            AST.Subscription -> "subscription"
    , ignorable
    , mpg pure o.name
    , ignorable
    , mpg genVariableDefinitions o.variableDefinitions
    , ignorable
    , mpg genDirectives o.directives
    , ignorable
    , genSelectionSet o.selectionSet
    ]

--------------------------------------------
--------------------------------------------
--------------------------------------------
--- the main entry point -------------------
--------------------------------------------
--------------------------------------------
genOperationDefinition :: AST.OperationDefinition -> Gen String
genOperationDefinition (AST.OperationDefinition_SelectionSet x) = genSelectionSet x

genOperationDefinition (AST.OperationDefinition_OperationType x) =
  genT_OperationDefinition_OperationType
    x
