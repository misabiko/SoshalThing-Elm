module Service exposing
  ( Service, Endpoint, isReady
  , Payload(..)
  , RateLimitInfo, initRateLimit
  , viewServiceSettings
  )

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Dict exposing (Dict)
import Url.Builder as UrlB

import Article exposing (Article)


type alias Service =
  { name: String
  , endpoints: Dict String Endpoint
  , articles: Article.Collection
  }


type alias Endpoint =
  { name: String
  , baseUrl: String
  , path: List String
  , options: List UrlB.QueryParameter
  , rateLimit: Maybe RateLimitInfo
  }


type Payload
  = FreePayload
      (List Article)
      (List Article.Id)
  | RateLimitedPayload
      (List Article)
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


isReady : Endpoint -> Bool
isReady endpoint =
  case endpoint.rateLimit of
    Just rateLimit ->
      rateLimit.remaining > 0

    Nothing -> True


-- VIEW


viewServiceSettings : Service -> Html msg
viewServiceSettings service =
  div [ class "box" ]
    ( (text service.name) ::
      (List.map (lazy viewEndpointStatus) (Dict.values service.endpoints))
    )


viewEndpointStatus : Endpoint -> Html msg
viewEndpointStatus endpoint =
  case endpoint.rateLimit of
    Just rateLimit ->
      div []
        [ p [] [ text endpoint.name ]
        , progress
            [ class "progress"
            , value (String.fromInt rateLimit.remaining)
            , Html.Attributes.max (String.fromInt rateLimit.limit)
            ]
            
            [ text ((String.fromInt rateLimit.remaining) ++ "/" ++ (String.fromInt rateLimit.limit)) ]
        , p [] [ text ("Reset: " ++ (String.fromInt rateLimit.reset))]
        ]

    Nothing ->
      div [] [ p [] [ text endpoint.name ] ]