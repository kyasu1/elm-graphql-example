-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Country.Object.Country exposing (code, continent, currency, emoji, emojiU, languages, name, native, phone)

import Country.InputObject
import Country.Interface
import Country.Object
import Country.Scalar
import Country.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode


{-| -}
code : SelectionSet (Maybe String) Country.Object.Country
code =
    Object.selectionForField "code" [] (Decode.string |> Decode.nullable)


{-| -}
name : SelectionSet (Maybe String) Country.Object.Country
name =
    Object.selectionForField "name" [] (Decode.string |> Decode.nullable)


{-| -}
native : SelectionSet (Maybe String) Country.Object.Country
native =
    Object.selectionForField "native" [] (Decode.string |> Decode.nullable)


{-| -}
phone : SelectionSet (Maybe String) Country.Object.Country
phone =
    Object.selectionForField "phone" [] (Decode.string |> Decode.nullable)


{-| -}
continent : SelectionSet decodesTo Country.Object.Continent -> SelectionSet (Maybe decodesTo) Country.Object.Country
continent object_ =
    Object.selectionForCompositeField "continent" [] object_ (identity >> Decode.nullable)


{-| -}
currency : SelectionSet (Maybe String) Country.Object.Country
currency =
    Object.selectionForField "currency" [] (Decode.string |> Decode.nullable)


{-| -}
languages : SelectionSet decodesTo Country.Object.Language -> SelectionSet (Maybe (List (Maybe decodesTo))) Country.Object.Country
languages object_ =
    Object.selectionForCompositeField "languages" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


{-| -}
emoji : SelectionSet (Maybe String) Country.Object.Country
emoji =
    Object.selectionForField "emoji" [] (Decode.string |> Decode.nullable)


{-| -}
emojiU : SelectionSet (Maybe String) Country.Object.Country
emojiU =
    Object.selectionForField "emojiU" [] (Decode.string |> Decode.nullable)
