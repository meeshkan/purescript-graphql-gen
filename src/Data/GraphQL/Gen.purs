module Data.GraphQL.Gen where

import Prelude
import Data.Array (fromFoldable)
import Data.Foldable (fold, intercalate)
import Data.GraphQL.AST as AST
import Data.GraphQL.Parser as GP
import Data.List (List(..), (:), singleton, length)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty (NonEmpty(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
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
comment = fold <$> sequence [ pure "#", strNoNewline, pure "\n" ]

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

mpg :: forall a. (a -> Gen String) -> Maybe a -> Gen String
mpg = maybe (pure "")

genArgument :: AST.Argument -> Gen String
genArgument (AST.Argument a) =
  intercalate
    <$> ignorable
    <*> sequence
        [ pure a.name, pure ":", genValue a.value
        ]

genDirective :: AST.Directive -> Gen String
genDirective (AST.Directive d) =
  fold
    <$> sequence
        [ pure "@", pure d.name, ignorable, mpg genArguments d.arguments
        ]

-- directives are not bounded by any bracketing, thus the empty strings
genDirectives :: AST.Directives -> Gen String
genDirectives (AST.Directives d) = genListish' "" "" genDirective d

genArguments :: AST.Arguments -> Gen String
genArguments (AST.Arguments l) = if length l == 0 then pure "" else genListish' "(" ")" genArgument l

genAlias :: String -> Gen String
genAlias s = fold <$> sequence [ pure s, ignorable, pure ":" ]

genField :: AST.Field -> Gen String
genField (AST.Field s) =
  intercalate <$> ignorable
    <*> sequence
        [ mpg genAlias s.alias
        , pure s.name
        , mpg genArguments s.arguments
        , mpg genDirectives s.directives
        , mpg genSelectionSet s.selectionSet
        ]

genFragmentSpread :: AST.FragmentSpread -> Gen String
genFragmentSpread (AST.FragmentSpread fs) =
  intercalate <$> ignorable
    <*> sequence
        [ pure "..."
        , pure fs.fragmentName
        , mpg genDirectives fs.directives
        ]

genTypeCondition :: AST.TypeCondition -> Gen String
genTypeCondition (AST.TypeCondition (AST.NamedType t)) = fold <$> sequence [ pure "on", mandatoryIgnorable, pure t ]

genInlineFragment :: AST.InlineFragment -> Gen String
genInlineFragment (AST.InlineFragment f) =
  intercalate <$> ignorable
    <*> sequence
        [ pure "..."
        , mpg genTypeCondition f.typeCondition
        , mpg genDirectives f.directives
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

genType (AST.Type_ListType (AST.ListType lt)) = fold <$> sequence [ pure "[", genType lt, pure "]" ]

genType (AST.Type_NonNullType (AST.NonNullType_ListType l)) =
  genType (AST.Type_ListType l)
    >>= pure
    <<< (flip append "!")

genType (AST.Type_NonNullType (AST.NonNullType_NamedType n)) =
  genType (AST.Type_NamedType n)
    >>= pure
    <<< (flip append "!")

genDefaultValue :: AST.DefaultValue -> Gen String
genDefaultValue (AST.DefaultValue dv) = fold <$> sequence [ pure "=", ignorable, genValue dv ]

genVariableDefinition :: AST.VariableDefinition -> Gen String
genVariableDefinition (AST.VariableDefinition vd) =
  intercalate
    <$> ignorable
    <*> sequence
        [ genVariable vd.variable
        , pure ":"
        , genType vd.type
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
  intercalate
    <$> ignorable
    <*> sequence
        [ pure
            $ case o.operationType of
                AST.Query -> "query"
                AST.Mutation -> "mutation"
                AST.Subscription -> "subscription"
        , mpg pure o.name
        , mpg genVariableDefinitions o.variableDefinitions
        , mpg genDirectives o.directives
        , genSelectionSet o.selectionSet
        ]

genOperationDefinition :: AST.OperationDefinition -> Gen String
genOperationDefinition (AST.OperationDefinition_SelectionSet x) = genSelectionSet x

genOperationDefinition (AST.OperationDefinition_OperationType x) =
  genT_OperationDefinition_OperationType
    x

genFragmentDefinition :: AST.FragmentDefinition -> Gen String
genFragmentDefinition (AST.FragmentDefinition { fragmentName, typeCondition: (AST.TypeCondition (AST.NamedType nt)), directives, selectionSet }) =
  intercalate <$> ignorable
    <*> sequence
        ( [ pure "fragment"
          , pure fragmentName
          , pure "on"
          , pure nt
          ]
            <> maybe [] (fromFoldable <<< (map genDirective) <<< unwrap) directives
            <> [ genSelectionSet selectionSet ]
        )

genExecutableDefinition :: AST.ExecutableDefinition -> Gen String
genExecutableDefinition (AST.ExecutableDefinition_OperationDefinition o) = genOperationDefinition o

genExecutableDefinition (AST.ExecutableDefinition_FragmentDefinition f) = genFragmentDefinition f

genInputValueDefinition :: AST.InputValueDefinition -> Gen String
genInputValueDefinition (AST.InputValueDefinition { description, name, type: _type, defaultValue, directives }) =
  intercalate <$> ignorable
    <*> sequence
        ( maybe
            []
            (pure <<< pure <<< show)
            description
            <> [ pure name, pure ":", genType _type ]
            <> maybe [] (\(AST.DefaultValue v) -> [ pure "=", genValue v ]) defaultValue
            <> maybe [] (pure <<< genDirectives)
                directives
        )

genArgumentsDefinition :: AST.ArgumentsDefinition -> Gen String
genArgumentsDefinition (AST.ArgumentsDefinition l) = genListish' "(" ")" genInputValueDefinition l

genTypeSystemDirectiveLocation :: AST.TypeSystemDirectiveLocation -> Gen String
genTypeSystemDirectiveLocation d =
  pure
    ( case d of
        AST.SCHEMA -> "SCHEMA"
        AST.SCALAR -> "SCALAR"
        AST.OBJECT -> "OBJECT"
        AST.FIELD_DEFINITION -> "FIELD_DEFINITION"
        AST.ARGUMENT_DEFINITION -> "ARGUMENT_DEFINITION"
        AST.INTERFACE -> "INTERFACE"
        AST.UNION -> "UNION"
        AST.ENUM -> "ENUM"
        AST.ENUM_VALUE -> "ENUM_VALUE"
        AST.INPUT_OBJECT -> "INPUT_OBJECT"
        AST.INPUT_FIELD_DEFINITION -> "INPUT_FIELD_DEFINITION"
    )

genExecutableDirectiveLocation :: AST.ExecutableDirectiveLocation -> Gen String
genExecutableDirectiveLocation d =
  pure
    ( case d of
        AST.QUERY -> "QUERY"
        AST.MUTATION -> "MUTATION"
        AST.SUBSCRIPTION -> "SUBSCRIPTION"
        AST.FIELD -> "FIELD"
        AST.FRAGMENT_DEFINITION -> "FRAGMENT_DEFINITION"
        AST.FRAGMENT_SPREAD -> "FRAGMENT_SPREAD"
        AST.INLINE_FRAGMENT -> "INLINE_FRAGMENT"
    )

genDirectiveLocation :: AST.DirectiveLocation -> Gen String
genDirectiveLocation (AST.DirectiveLocation_ExecutableDirectiveLocation e) = genExecutableDirectiveLocation e

genDirectiveLocation (AST.DirectiveLocation_TypeSystemDirectiveLocation t) = genTypeSystemDirectiveLocation t

genDirectiveDefinition :: AST.DirectiveDefinition -> Gen String
genDirectiveDefinition (AST.DirectiveDefinition { description, name, argumentsDefinition, directiveLocations: AST.DirectiveLocations dl }) =
  intercalate <$> ignorable
    <*> sequence
        ( maybe
            []
            (pure <<< pure <<< show)
            description
            <> [ pure "directive"
              , pure ("@" <> name)
              ]
            <> maybe [] (pure <<< genArgumentsDefinition) argumentsDefinition
            <> [ pure "on"
              , intercalate <$> (fold <$> sequence [ ignorable, pure "|", ignorable ])
                  <*> sequence (map genDirectiveLocation dl)
              ]
        )

genSchemaDefinition :: AST.SchemaDefinition -> Gen String
genSchemaDefinition (AST.SchemaDefinition { directives, rootOperationTypeDefinition: otd }) =
  intercalate <$> ignorable
    <*> sequence
        ( [ pure "schema" ] <> maybe [] (pure <<< genDirectives) directives
            <> [ genListish' "{" "}" genT_OperationTypeDefinition (map unwrap otd)
              ]
        )

genTypeSystemDefinition :: AST.TypeSystemDefinition -> Gen String
genTypeSystemDefinition (AST.TypeSystemDefinition_SchemaDefinition s) = genSchemaDefinition s

genTypeSystemDefinition (AST.TypeSystemDefinition_TypeDefinition t) = genTypeDefinition t

genTypeSystemDefinition (AST.TypeSystemDefinition_DirectiveDefinition d) = genDirectiveDefinition d

genOperationType :: AST.OperationType -> Gen String
genOperationType t =
  pure
    ( case t of
        AST.Query -> "query"
        AST.Mutation -> "mutation"
        AST.Subscription -> "subscription"
    )

genNamedType :: AST.NamedType -> Gen String
genNamedType (AST.NamedType s) = pure s

genT_OperationTypeDefinition :: AST.T_OperationTypeDefinition -> Gen String
genT_OperationTypeDefinition ({ operationType, namedType }) =
  intercalate <$> ignorable
    <*> sequence
        [ genOperationType operationType, pure ":", genNamedType namedType ]

genOperationTypeDefinition :: AST.OperationTypeDefinition -> Gen String
genOperationTypeDefinition (AST.OperationTypeDefinition o) = genT_OperationTypeDefinition o

genSchemaExtension :: AST.SchemaExtension -> Gen String
genSchemaExtension (AST.SchemaExtension_With_Directives { directives }) = genDirectives directives

genSchemaExtension (AST.SchemaExtension_With_OperationTypeDefinition { directives, operationTypesDefinition }) =
  intercalate <$> ignorable
    <*> sequence
        ( maybe [] (pure <<< genDirectives) directives
            <> [ genListish' "{" "}" genOperationTypeDefinition operationTypesDefinition ]
        )

genEnumValueDefinition :: AST.EnumValueDefinition -> Gen String
genEnumValueDefinition (AST.EnumValueDefinition { description, enumValue: AST.EnumValue e, directives }) =
  intercalate <$> ignorable
    <*> sequence
        ( maybe [] (pure <<< pure <<< show) description
            <> [ pure e ]
            <> maybe [] (pure <<< genDirectives) directives
        )

genEnumTypeDefinition :: AST.EnumTypeDefinition -> Gen String
genEnumTypeDefinition (AST.EnumTypeDefinition { description, name, directives, enumValuesDefinition }) =
  intercalate <$> ignorable
    <*> sequence
        ( maybe [] (pure <<< pure <<< show) description
            <> [ pure "enum", pure name ]
            <> maybe [] (pure <<< genDirectives) directives
            <> maybe [] (pure <<< genListish' "{" "}" genEnumValueDefinition <<< unwrap) enumValuesDefinition
        )

genEnumTypeExtension :: AST.EnumTypeExtension -> Gen String
genEnumTypeExtension (AST.EnumTypeExtension_With_EnumValuesDefinition { name, directives, enumValuesDefinition: AST.EnumValuesDefinition evd }) =
  intercalate <$> ignorable
    <*> sequence
        ( [ pure "extend", pure "enum", pure name ]
            <> maybe [] (pure <<< genDirectives) directives
            <> [ genListish' "{" "}" genEnumValueDefinition evd
              ]
        )

genEnumTypeExtension (AST.EnumTypeExtension_With_Directives { name, directives }) =
  intercalate <$> ignorable
    <*> sequence [ pure "extend", pure "enum", pure name, genDirectives directives ]

genInputObjectTypeExtension :: AST.InputObjectTypeExtension -> Gen String
genInputObjectTypeExtension (AST.InputObjectTypeExtension_With_InputFieldsDefinition { name, directives, inputFieldsDefinition: AST.InputFieldsDefinition ifd }) =
  intercalate <$> ignorable
    <*> sequence
        ( [ pure "extend", pure "input", pure name ]
            <> maybe [] (pure <<< genDirectives) directives
            <> [ genListish' "{" "}" genInputValueDefinition ifd
              ]
        )

genInputObjectTypeExtension (AST.InputObjectTypeExtension_With_Directives { name, directives }) =
  intercalate <$> ignorable
    <*> sequence [ pure "extend", pure "input", pure name, genDirectives directives ]

genInputObjectTypeDefinition :: AST.InputObjectTypeDefinition -> Gen String
genInputObjectTypeDefinition (AST.InputObjectTypeDefinition { description, name, directives, inputFieldsDefinition }) =
  intercalate <$> ignorable
    <*> sequence
        ( maybe [] (pure <<< pure <<< show) description
            <> [ pure "input", pure name ]
            <> maybe [] (pure <<< genDirectives) directives
            <> maybe []
                ( pure
                    <<< genListish' "{" "}" genInputValueDefinition
                    <<< unwrap
                )
                inputFieldsDefinition
        )

genFieldDefinition :: AST.FieldDefinition -> Gen String
genFieldDefinition (AST.FieldDefinition { description, name, argumentsDefinition, type: _type, directives }) =
  intercalate <$> ignorable
    <*> sequence
        ( maybe []
            (pure <<< pure <<< show)
            description
            <> [ pure name ]
            <> maybe [] (pure <<< genArgumentsDefinition) argumentsDefinition
            <> [ pure ":", genType _type ]
            <> maybe [] (pure <<< genDirectives) directives
        )

genInterfaceTypeDefinition :: AST.InterfaceTypeDefinition -> Gen String
genInterfaceTypeDefinition (AST.InterfaceTypeDefinition { description, name, directives, fieldsDefinition }) =
  intercalate <$> ignorable
    <*> sequence
        ( maybe []
            (pure <<< pure <<< show)
            description
            <> [ pure "interface", pure name ]
            <> maybe [] (pure <<< genDirectives) directives
            <> maybe []
                ( pure
                    <<< genListish' "{" "}" genFieldDefinition
                    <<< unwrap
                )
                fieldsDefinition
        )

genInterfaceTypeExtension :: AST.InterfaceTypeExtension -> Gen String
genInterfaceTypeExtension (AST.InterfaceTypeExtension_With_FieldsDefinition { name, directives, fieldsDefinition: AST.FieldsDefinition fd }) =
  intercalate <$> ignorable
    <*> sequence
        ( [ pure "extend", pure "interface", pure name ] <> maybe [] (pure <<< genDirectives) directives
            <> [ genListish' "{" "}" genFieldDefinition fd
              ]
        )

genInterfaceTypeExtension (AST.InterfaceTypeExtension_With_Directives { name, directives }) =
  intercalate <$> ignorable
    <*> sequence [ pure "extend", pure "interface", pure name, genDirectives directives ]

genUnionTypeDefinition :: AST.UnionTypeDefinition -> Gen String
genUnionTypeDefinition (AST.UnionTypeDefinition { description, name, directives, unionMemberTypes }) =
  intercalate <$> ignorable
    <*> sequence
        ( maybe []
            (pure <<< pure <<< show)
            description
            <> [ pure "union", pure name ]
            <> maybe [] (pure <<< genDirectives) directives
            <> maybe []
                ( \(AST.UnionMemberTypes umt) ->
                    [ pure "="
                    , intercalate
                        <$> (fold <$> sequence [ ignorable, pure "|", ignorable ])
                        <*> sequence (map genNamedType umt)
                    ]
                )
                unionMemberTypes
        )

genUnionTypeExtension :: AST.UnionTypeExtension -> Gen String
genUnionTypeExtension (AST.UnionTypeExtension_With_UnionMemberTypes { name, directives, unionMemberTypes: AST.UnionMemberTypes umt }) =
  intercalate <$> ignorable
    <*> sequence
        ( [ pure "extend", pure "union", pure name ] <> maybe [] (pure <<< genDirectives) directives
            <> [ pure "="
              , intercalate
                  <$> (fold <$> sequence [ ignorable, pure "|", ignorable ])
                  <*> sequence (map genNamedType umt)
              ]
        )

genUnionTypeExtension (AST.UnionTypeExtension_With_Directives { name, directives }) =
  intercalate <$> ignorable
    <*> sequence [ pure "extend", pure "union", pure name, genDirectives directives ]

genObjectTypeDefinition :: AST.ObjectTypeDefinition -> Gen String
genObjectTypeDefinition (AST.ObjectTypeDefinition { description, name, implementsInterfaces, directives, fieldsDefinition }) =
  intercalate <$> ignorable
    <*> sequence
        ( maybe []
            (pure <<< pure <<< show)
            description
            <> [ pure "type", pure name ]
            <> maybe []
                ( \(AST.ImplementsInterfaces ii) ->
                    [ pure "implements"
                    , intercalate
                        <$> (fold <$> sequence [ ignorable, pure "&", ignorable ])
                        <*> sequence (map genNamedType ii)
                    ]
                )
                implementsInterfaces
            <> maybe [] (pure <<< genDirectives) directives
            <> maybe [] (pure <<< genListish' "{" "}" genFieldDefinition <<< unwrap) fieldsDefinition
        )

genObjectTypeExtension :: AST.ObjectTypeExtension -> Gen String
genObjectTypeExtension (AST.ObjectTypeExtension_With_FieldsDefinition { name, directives, fieldsDefinition: AST.FieldsDefinition fd }) =
  intercalate <$> ignorable
    <*> sequence
        ( [ pure "extend", pure "type", pure name ] <> maybe [] (pure <<< genDirectives) directives
            <> [ genListish' "{" "}" genFieldDefinition fd
              ]
        )

genObjectTypeExtension (AST.ObjectTypeExtension_With_Directives { name, directives }) =
  intercalate <$> ignorable
    <*> sequence [ pure "extend", pure "type", pure name, genDirectives directives ]

genObjectTypeExtension (AST.ObjectTypeExtension_With_ImplementsInterfaces { name, implementsInterfaces: AST.ImplementsInterfaces ii }) =
  intercalate <$> ignorable
    <*> sequence
        [ pure "extend"
        , pure "type"
        , pure name
        , pure "implements"
        , intercalate
            <$> (fold <$> sequence [ ignorable, pure "&", ignorable ])
            <*> sequence (map genNamedType ii)
        ]

genTypeExtension :: AST.TypeExtension -> Gen String
genTypeExtension (AST.TypeExtension_EnumTypeExtension e) = genEnumTypeExtension e

genTypeExtension (AST.TypeExtension_InputObjectTypeExtension i) = genInputObjectTypeExtension i

genTypeExtension (AST.TypeExtension_InterfaceTypeExtension i) = genInterfaceTypeExtension i

genTypeExtension (AST.TypeExtension_ObjectTypeExtension o) = genObjectTypeExtension o

genTypeExtension (AST.TypeExtension_UnionTypeExtension u) = genUnionTypeExtension u

genTypeExtension (AST.TypeExtension_ScalarTypeExtension (AST.ScalarTypeExtension { name, directives })) =
  intercalate <$> ignorable
    <*> sequence [ pure "extend", pure "scalar", pure name, genDirectives directives ]

genTypeDefinition :: AST.TypeDefinition -> Gen String
genTypeDefinition (AST.TypeDefinition_EnumTypeDefinition e) = genEnumTypeDefinition e

genTypeDefinition (AST.TypeDefinition_InputObjectTypeDefinition i) = genInputObjectTypeDefinition i

genTypeDefinition (AST.TypeDefinition_InterfaceTypeDefinition i) = genInterfaceTypeDefinition i

genTypeDefinition (AST.TypeDefinition_ObjectTypeDefinition o) = genObjectTypeDefinition o

genTypeDefinition (AST.TypeDefinition_UnionTypeDefinition u) = genUnionTypeDefinition u

genTypeDefinition (AST.TypeDefinition_ScalarTypeDefinition (AST.ScalarTypeDefinition { description, name, directives })) =
  intercalate <$> ignorable
    <*> sequence
        ( maybe []
            (pure <<< pure <<< show)
            description
            <> [ pure "scalar", pure name ]
            <> maybe [] (pure <<< genDirectives) directives
        )

genTypeSystemExtension :: AST.TypeSystemExtension -> Gen String
genTypeSystemExtension (AST.TypeSystemExtension_SchemaExtension s) = genSchemaExtension s

genTypeSystemExtension (AST.TypeSystemExtension_TypeExtension t) = genTypeExtension t

genDefinition :: AST.Definition -> Gen String
genDefinition (AST.Definition_ExecutableDefinition e) = genExecutableDefinition e

genDefinition (AST.Definition_TypeSystemDefinition t) = genTypeSystemDefinition t

genDefinition (AST.Definition_TypeSystemExtension t) = genTypeSystemExtension t

genDocument :: AST.Document -> Gen String
genDocument (AST.Document l) = intercalate <$> ignorable <*> sequence (map genDefinition l)
