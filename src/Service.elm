module Service exposing
  ( Service(..), unwrap
  , Endpoint, isReady, newEndpoint, unwrapEndpoint, updateEndpointRateLimit
  , getTimelineArticles, getArticlesList
  , Payload(..)
  , RateLimitInfo, initRateLimit
  , viewServiceSettings, Msg
  )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Dict exposing (Dict)
import Url.Builder as UrlB
import Maybe.Extra as MaybeE

import Article exposing (Article)
import TimeParser


type Service ext vExt
  = Service (ServiceData ext vExt)


type alias ServiceData ext vExt =
  { name: String
  , endpoints: Dict String Endpoint
  , articles: Article.Collection ext
  , viewArticle: (Service ext vExt -> TimeParser.TimeModel -> Bool -> Article vExt -> Html Msg)
  , toViewArticle: (Article.Collection ext -> Article ext -> Maybe (Article vExt))
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


type Payload ext
  = FreePayload
      (List (Article ext))
      (List Article.Id)
  | RateLimitedPayload
      (List (Article ext))
      (List Article.Id)
      RateLimitInfo


type alias RateLimitInfo =
  { remaining: Int
  , limit: Int
  , reset: Int
  }


type Msg
  = Bleh


unwrap : Service ext vExt -> ServiceData ext vExt
unwrap (Service service) = service


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


getTimelineArticles : ServiceData ext vExt -> List Article.Id -> List (Article ext)
getTimelineArticles service ids =
  List.filterMap (Article.get service.articles) ids


getArticlesList : ServiceData ext vExt -> List (Article ext)
getArticlesList service =
  Dict.values service.articles


-- VIEW


viewServiceSettings : Service ext vExt -> Html msg
viewServiceSettings service =
  let
    serviceData = unwrap service
  in
  div [ class "box" ]
    ( (text serviceData.name) ::
      (List.map (lazy viewEndpointStatus) (Dict.values serviceData.endpoints))
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