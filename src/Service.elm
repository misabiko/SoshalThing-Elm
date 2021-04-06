module Service exposing
  ( Service
  , Endpoint, isReady, newEndpoint, unwrapEndpoint, updateEndpointRateLimit
  , Payload(..)
  , RateLimitInfo, initRateLimit
  , viewServiceSettings
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Dict exposing (Dict)
import Url.Builder as UrlB

import Article exposing (Article)
import Maybe.Extra exposing (unwrap)
import Parser exposing (end)


type alias Service a =
  { name: String
  , endpoints: Dict String Endpoint
  , articles: Article.Collection a
  }


type Endpoint
  = Ready EndpointData
--| OverRateLimit EndpointData Time.Posix
  | Problem EndpointData


type alias EndpointData =
  { name: String
  , baseUrl: String
  , path: List String
  , options: List UrlB.QueryParameter
  , rateLimit: Maybe RateLimitInfo
  }


type Payload a
  = FreePayload
      (List (Article a))
      (List Article.Id)
  | RateLimitedPayload
      (List (Article a))
      (List Article.Id)
      RateLimitInfo


type alias RateLimitInfo =
  { remaining: Int
  , limit: Int
  , reset: Int
  }


initRateLimit : Int -> Int -> RateLimitInfo
initRateLimit limit reset =
  { remaining = limit
  , limit = limit
  , reset = reset
  }


newEndpoint : EndpointData -> Endpoint
newEndpoint data =
  Ready data


isReady : Endpoint -> Bool
isReady endpoint =
  case (unwrapEndpoint endpoint).rateLimit of
    Just rateLimit ->
      rateLimit.remaining > 0

    Nothing -> True


unwrapEndpoint : Endpoint -> EndpointData
unwrapEndpoint endpoint =
  case endpoint of
    Ready data -> data
    Problem data -> data


updateEndpointRateLimit : Dict String Endpoint -> Endpoint -> RateLimitInfo -> Dict String Endpoint
updateEndpointRateLimit endpoints endpoint rateLimit =
  case endpoint of
    Ready data ->
      Dict.insert data.name (Ready { data | rateLimit = Just rateLimit}) endpoints

    Problem data ->
      Dict.insert data.name (Ready { data | rateLimit = Just rateLimit}) endpoints


-- VIEW


viewServiceSettings : Service a -> Html msg
viewServiceSettings service =
  div [ class "box" ]
    ( (text service.name) ::
      (List.map (lazy viewEndpointStatus) (Dict.values service.endpoints))
    )


viewEndpointStatus : Endpoint -> Html msg
viewEndpointStatus endpoint =
  let
    data = unwrapEndpoint endpoint
  in
  case data.rateLimit of
    Just rateLimit ->
      div []
        [ p [] [ text data.name ]
        , progress
            [ class "progress"
            , value (String.fromInt rateLimit.remaining)
            , Html.Attributes.max (String.fromInt rateLimit.limit)
            ]
            
            [ text ((String.fromInt rateLimit.remaining) ++ "/" ++ (String.fromInt rateLimit.limit)) ]
        , p [] [ text ("Reset: " ++ (String.fromInt rateLimit.reset))]
        ]

    Nothing ->
      div [] [ p [] [ text data.name ] ]