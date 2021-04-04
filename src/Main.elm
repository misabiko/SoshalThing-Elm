module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Html.Keyed
import Dict exposing (Dict)
import Http
import Time
import Task
import Url.Builder as UrlB

import Article exposing (Article, ShareableArticle)
import TimeParser
import Tweet


-- MAIN


main : Program () Model Msg
main =
  Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Service =
  { name: String
  , endpoints: Dict String Endpoint
  , articles: Article.Collection
  }


type alias Endpoint =
  { name: String
  , baseUrl: String
  , path: List String
  }


type alias Timeline =
  { title: String
  , serviceName: String
  , endpointName: String
  , articleIds: List String
  , options: Dict String String
  }


type alias TimeModel =
  { zone : Time.Zone
  , lastNow : Time.Posix
  }


type alias Model =
  { services: Dict String Service
  , timelines: List Timeline
  , time : TimeModel
  }


init : () -> ( Model, Cmd Msg )
init _ =
  ( { services =  Dict.fromList
        [ initTwitter ]
    , timelines =
      [ initTimeline "Twitter" "Home Timeline" "Home" Dict.empty
      , initTimeline "Twitter" "List" "Art" (Dict.fromList [ ("slug", "Art"), ("owner_screen_name", "misabiko") ])
      , initTimeline "Twitter" "Search" "1draw" (Dict.fromList [ ("q", "-filter:retweets #深夜の真剣お絵描き60分一本勝負 OR #東方の90分お絵描き"), ("result_type", "recent") ])
      , initTimeline "Twitter" "User Timeline" "User" Dict.empty
      ]
    , time =
        { zone = Time.utc
        , lastNow = Time.millisToPosix 0
        }
    }
  , Cmd.batch
      [ Task.perform AdjustTimeZone Time.here
      , Task.perform NewTime Time.now
      ]
  )


initTimeline : String -> String -> String -> Dict String String -> Timeline
initTimeline serviceName endpointName title options =
  { title = title
  , serviceName = serviceName
  , endpointName = endpointName
  , articleIds = []
  , options = options
  }


initTwitter : (String, Service)
initTwitter =
  ( "Twitter"
  , { name = "Twitter"
    , endpoints = Dict.fromList
        [ ( "Home Timeline", { name = "Home Timeline", baseUrl = "http://localhost:5000", path = [ "twitter", "home_timeline" ] } )
        , ( "User Timeline", { name = "User Timeline", baseUrl = "http://localhost:5000", path = [ "twitter", "user_timeline" ] } )
        , ( "Search", { name = "Search", baseUrl = "http://localhost:5000", path = [ "twitter", "search" ] } )
        , ( "List", { name = "List", baseUrl = "http://localhost:5000", path = [ "twitter", "list" ] } )
        ]
    , articles = Dict.empty
    }
  )


-- UPDATE


type alias EndpointPayloadResult =
  Result (List (String, Int)) EndpointPayload


type Msg
  = GotEndpointPayload Service Timeline (Result Http.Error EndpointPayloadResult)
  | GotServicePayload Service (Result Http.Error EndpointPayloadResult)
  | Refresh Service Endpoint Timeline
  | AdjustTimeZone Time.Zone
  | NewTime Time.Posix
  | Like Service Article
  | Repost Service Article


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotEndpointPayload service timeline result ->
      case result of
        Ok payloadResult ->
          case payloadResult of
            Ok endpointPayload ->
              ( { model
                  | services = Dict.insert service.name (updateServiceArticles endpointPayload.articles service) model.services
                  , timelines = updateTimelineArticles endpointPayload.timelineArticles timeline.title model.timelines
                }
              , Task.perform NewTime Time.now
              )

            Err errors ->
              let
                _ = List.map (Debug.log "Payload Error") errors
              in
                ( model, Cmd.none )
        Err reason ->
          let
            _ = Debug.log "Error" reason
          in
            (model, Cmd.none)

    GotServicePayload service result ->
      case result of
        Ok payloadResult ->
          case payloadResult of
            Ok endpointPayload ->
              ( { model | services = Dict.insert service.name (updateServiceArticles endpointPayload.articles service) model.services }
              , Task.perform NewTime Time.now
              )

            Err errors ->
              let
                _ = List.map (Debug.log "Payload Error") errors
              in
                ( model, Cmd.none )
        Err reason ->
          let
            _ = Debug.log "Error" reason
          in
            (model, Cmd.none)

    Refresh service endpoint timeline ->
      (model, getEndpoint service endpoint timeline)

    AdjustTimeZone newZone ->
      ( { model | time = (\t -> { t | zone = newZone }) model.time }
      , Cmd.none
      )

    NewTime now ->
      ( { model | time = (\t -> { t | lastNow = now }) model.time }
      , Cmd.none
      )
    
    Like service article ->
      (model, postLike service article)
    
    Repost service article ->
      (model, postRetweet service article)

updateArticles : Service -> Timeline -> EndpointPayload -> Model -> ( Model, Cmd Msg )
updateArticles service timeline payload model =
  ( { model | services = Dict.insert service.name (updateServiceArticles payload.articles service) model.services
            , timelines = updateTimelineArticles payload.timelineArticles timeline.title model.timelines
    }
  , Task.perform NewTime Time.now
  )


updateServiceArticles : List Article -> Service -> Service
updateServiceArticles articles service =
  { service | articles = Dict.union (listToDict articles) service.articles }


listToDict : List Article -> Article.Collection
listToDict articles =
  Dict.fromList
    <| List.map (\article -> (article.id, article)) articles


updateTimelineArticles : List String -> String -> List Timeline -> List Timeline
updateTimelineArticles articleIds timelineTitle timelines =
  List.map (\timeline -> 
    if timeline.title == timelineTitle then
      { timeline | articleIds = timelineSortArticles (appendNonMembers timeline.articleIds articleIds) }
    else
      timeline)
    timelines


appendNonMembers : List a -> List a -> List a
appendNonMembers existingValues newValues =
  existingValues ++ (List.filter (\val -> not (List.member val existingValues)) newValues)


timelineSortArticles : List String -> List String
timelineSortArticles articleIds =
  articleIds
    |> List.sortBy (\articleId -> Maybe.withDefault 0 (String.toInt articleId))
    |> List.reverse


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "SoshalThing"
  , body =
      [ viewSidebar
      , lazy viewTimelineContainer model ]
  }


viewTimelineContainer : Model -> Html Msg
viewTimelineContainer model =
  (div [ id "timelineContainer" ] (List.map (lazy2 viewTimeline model) model.timelines))


viewSidebar : Html Msg
viewSidebar =
  nav [ id "sidebar" ]
    [ div [ id "sidebarButtons" ]
      [ button [] [ viewIcon "fa-angle-double-right" "fas" "fa-2x" ] ]
    ]


viewIcon : String -> String -> String -> Html Msg
viewIcon icon iconType size =
  span [ class "icon" ]
    [ i [ class iconType, class icon, class size ] [] ]


viewTimeline : Model -> Timeline -> Html Msg
viewTimeline model timeline =
  case (Dict.get timeline.serviceName model.services) of
    Nothing ->
      text (Debug.log (timeline.title ++ " Error") "Couldn't find service.")
    
    Just service ->
      case (Dict.get timeline.endpointName service.endpoints) of
        Nothing ->
          text (Debug.log (timeline.title ++ " Error") "Couldn't find endpoint.")
        
        Just endpoint ->
          div [ class "timeline" ]
            [ div [ class "timelineHeader" ]
              [ strong [] [ text timeline.title ]
              , div [ class "timelineButtons" ]
                  [ button [ onClick (Refresh service endpoint timeline) ] [ viewIcon "fa-sync-alt" "fas" "fa-lg" ] ]
              ]
            , viewContainer model.time service (Article.getShareableArticles service.articles timeline.articleIds)
            ]


viewContainer : TimeModel -> Service -> List ShareableArticle -> Html Msg
viewContainer timeModel service shareableArticles =
  Html.Keyed.node "div" [ class "timelineArticles" ] (List.map (Tweet.viewKeyedTweet Like Repost timeModel service) shareableArticles)


-- HTTP


type alias EndpointPayload =
  { articles: List Article
  , timelineArticles: List String
  }


postLike : Service -> Article -> Cmd Msg
postLike service article =
  case article.social of
    Just social ->
      Http.post
        { url = "http://localhost:5000/twitter/" ++ (if social.liked then "unlike/" else "like/") ++ article.id
        , body = Http.emptyBody
        , expect = Http.expectJson (GotServicePayload service) Tweet.payloadResponseDecoder
        }

    Nothing -> Cmd.none


postRetweet : Service -> Article -> Cmd Msg
postRetweet service article =
  case article.social of
    Just social ->
      if social.reposted then
        Cmd.none
      else
        Http.post
          { url = "http://localhost:5000/twitter/retweet/" ++ article.id
          , body = Http.emptyBody
          , expect = Http.expectJson (GotServicePayload service) Tweet.payloadResponseDecoder
          }

    Nothing -> Cmd.none


getEndpoint : Service -> Endpoint -> Timeline -> Cmd Msg
getEndpoint service endpoint timeline =
  Http.get
    { url = UrlB.crossOrigin endpoint.baseUrl endpoint.path (dictToQueries timeline.options)
    , expect = Http.expectJson (GotEndpointPayload service timeline) Tweet.payloadResponseDecoder
    }


dictToQueries : Dict String String -> List UrlB.QueryParameter
dictToQueries queries =
  List.map (\option -> UrlB.string (Tuple.first option) (Tuple.second option)) (Dict.toList queries)