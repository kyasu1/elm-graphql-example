module Main exposing (main)

import Browser
import Country.Object
import Country.Object.Continent as Continent
import Country.Object.Country as Country
import Country.Object.Language as Language
import Country.Query as Query
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, succeed, with)
import Html exposing (..)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (on)
import Json.Decode as Decode
import RemoteData exposing (RemoteData)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- View


view : Model -> Html Msg
view model =
    div [ class "container mx-auto px-6" ]
        [ div [ class "flex h-screen flex-col" ]
            [ header [ class "h-16 mb-2 bg-grey-light text-center p-2" ] [ text "Sample by Elm with Parcel and Tailwind CSS" ]
            , div [ class "flex flex-1" ]
                [ nav [ class "w-64 p-2 bg-orange-lightest" ] [ text "SIDE" ]
                , main_ [ class "flex-1 w-full p-2" ] [ viewMain model ]
                , aside [ class "w-64 p-2 bg-orange-lightest" ] [ text "ASIDE" ]
                ]
            , footer [ class "h-16 bg-orange-lightest text-center p-2" ] [ text "THIS IS FOOTER" ]
            ]
        ]


viewMain : Model -> Html Msg
viewMain { state, data } =
    case data of
        RemoteData.NotAsked ->
            div [] [ text "Not Asked" ]

        RemoteData.Loading ->
            div [] [ text "Loading" ]

        RemoteData.Failure e ->
            div [] [ text <| "Failure" ]

        RemoteData.Success response ->
            div [] [ viewSelector state response ]


viewSelector : State -> Response -> Html Msg
viewSelector state response =
    case state of
        NotSelected ->
            div []
                [ div []
                    [ viewContinentSelector Nothing response.continents
                    ]
                ]

        ContinentSelected continent ->
            div []
                [ div []
                    [ viewContinentSelector (Just continent) response.continents
                    ]
                , div []
                    [ viewCountriesSelector continent Nothing response.countries ]
                ]

        CountrySelected continentCode countryCode ->
            div []
                [ div []
                    [ viewContinentSelector (Just continentCode) response.continents
                    ]
                , div []
                    [ viewCountriesSelector continentCode (Just countryCode) response.countries ]
                , div []
                    [ viewCountry (List.filter (\country -> country.code == countryCode) response.countries |> List.head) ]
                ]


viewContinentSelector : Maybe String -> List Continent -> Html Msg
viewContinentSelector continentMaybe continentList =
    let
        options =
            case continentMaybe of
                Just selectedContinent ->
                    List.map (\continent -> option [ value continent.code, selected (selectedContinent == continent.code) ] [ text continent.name ]) continentList

                Nothing ->
                    List.map (\continent -> option [ value continent.code ] [ text continent.name ]) continentList
    in
    viewRow "Continent : " <| formSelect ContinentChosen options


viewCountriesSelector : String -> Maybe String -> List Country -> Html Msg
viewCountriesSelector continent countryMaybe countryList =
    let
        list =
            List.filter (\country -> country.continent.code == continent) countryList

        options =
            case countryMaybe of
                Just selectedCountry ->
                    List.map (\coutnry -> option [ value coutnry.code, selected (selectedCountry == coutnry.code) ] [ text coutnry.name ]) list

                Nothing ->
                    List.map (\country -> option [ value country.code ] [ text country.name ]) list
    in
    viewRow "Country : " <| formSelect (CountryChosen continent) options


viewCountry : Maybe Country -> Html Msg
viewCountry coutnryMaybe =
    case coutnryMaybe of
        Just country ->
            div [ class "p-2 border" ]
                [ viewRow "code" (text country.code)
                , viewRow "name" (text country.name)
                , viewRow "native" (text (Maybe.withDefault "" country.native))
                , viewRow "phone" (text (Maybe.withDefault "" country.phone))
                , viewRow "continentCode" (text country.continent.code)
                , viewRow "currency" (text (Maybe.withDefault "" country.currency))
                , viewRow "languages" (text (country.languages |> List.map .code |> List.intersperse ", " |> String.concat))
                , viewRow "emoji" (text (Maybe.withDefault "" country.emoji))
                , viewRow "emojiU" (text country.emojiU)
                ]

        Nothing ->
            div [] [ text "Something wrong !" ]


viewRow : String -> Html Msg -> Html Msg
viewRow labelString body =
    div [ class "flex py-2" ] [ div [ class "w-32" ] [ text labelString ], div [] [ body ] ]


formSelect : (String -> Msg) -> List (Html Msg) -> Html Msg
formSelect toMsg options =
    div [ class "inline-block relative w-64" ]
        [ select
            [ onChange toMsg
            , class "block appearance-none w-full bg-white border border-grey-light hover:border-grey px-4 py-2 pr-8 rounded shadow leading-tight focus:outline-none focus:shadow-outline"
            ]
            options
        ]


onChange : (String -> msg) -> Html.Attribute msg
onChange handler =
    on "change" (Decode.map handler Html.Events.targetValue)



-- update


init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = RemoteData.Loading, state = NotSelected }, execQuery )


type Msg
    = GotRespponse (RemoteData (Graphql.Http.Error Response) Response)
    | ContinentChosen String
    | CountryChosen String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRespponse response ->
            ( { model | data = response }, Cmd.none )

        ContinentChosen code ->
            ( { model | state = ContinentSelected code }, Cmd.none )

        CountryChosen continentCode countryCode ->
            ( { model | state = CountrySelected continentCode countryCode }, Cmd.none )


api : String
api =
    "https://countries.trevorblades.com/"


execQuery : Cmd Msg
execQuery =
    query
        |> Graphql.Http.queryRequest api
        |> Graphql.Http.send (RemoteData.fromResult >> GotRespponse)



-- SelectionSet


query : SelectionSet Response RootQuery
query =
    succeed Response
        |> with
            (Query.continents continentSelection
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.nonNullElementsOrFail
            )
        |> with
            (Query.countries countrySelection
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.nonNullElementsOrFail
            )


continentSelection : SelectionSet Continent Country.Object.Continent
continentSelection =
    succeed Continent
        |> with (Continent.code |> SelectionSet.nonNullOrFail)
        |> with (Continent.name |> SelectionSet.nonNullOrFail)


countrySelection : SelectionSet Country Country.Object.Country
countrySelection =
    succeed Country
        |> with
            (Country.code
                |> SelectionSet.nonNullOrFail
            )
        |> with
            (Country.name
                |> SelectionSet.nonNullOrFail
            )
        |> with Country.native
        |> with Country.phone
        |> with
            (Country.continent continentSelection
                |> SelectionSet.nonNullOrFail
            )
        |> with Country.currency
        |> with
            (Country.languages languageSelection
                |> SelectionSet.nonNullOrFail
                |> SelectionSet.nonNullElementsOrFail
            )
        |> with Country.emoji
        |> with (Country.emoji |> SelectionSet.withDefault "")


languageSelection : SelectionSet Language Country.Object.Language
languageSelection =
    succeed Language
        |> with
            (Language.code
                |> SelectionSet.nonNullOrFail
            )
        |> with Language.name
        |> with Language.native
        |> with Language.rtl



-- Model


type alias Model =
    { data : RemoteData (Graphql.Http.Error Response) Response
    , state : State
    }


type State
    = NotSelected
    | ContinentSelected String
    | CountrySelected String String


type alias Response =
    { continents : List Continent
    , countries : List Country
    }


type alias Continent =
    { code : String
    , name : String
    }


type alias Country =
    { code : String
    , name : String
    , native : Maybe String
    , phone : Maybe String
    , continent : Continent
    , currency : Maybe String
    , languages : List Language
    , emoji : Maybe String
    , emojiU : String
    }


type alias Language =
    { code : String
    , name : Maybe String
    , native : Maybe String
    , rtl : Maybe Int
    }
