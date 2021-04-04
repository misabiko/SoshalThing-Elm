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
import Maybe.Extra

import Article exposing (Article, ShareableArticle)
import Service exposing (Service, Endpoint, Payload(..), RateLimitInfo)
import TimeParser
import Tweet
import Url.Builder exposing (crossOrigin)
import Service
import Service exposing (isReady)


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


type alias Timeline =
  { title: String
  , serviceName: String
  , endpointName: String
  , articleIds: List String
  , options: Dict String String
  , interval: Maybe Int
  }


type alias TimeModel =
  { zone : Time.Zone
  , lastNow : Time.Posix
  }


type SidebarMenu
  = ServiceMenu


type Sidebar
  = Collapsed
  | Expanded SidebarMenu


type alias Model =
  { services: Dict String Service
  , timelines: List Timeline
  , time : TimeModel
  , sidebar : Sidebar
  }


init : () -> ( Model, Cmd Msg )
init _ =
  ( { services =  Dict.fromList
        [ initTwitter ]
    , timelines =
      [ initTimeline "Twitter" "Home Timeline" "Home" Dict.empty (Just 64285)
      , initTimeline "Twitter" "List" "Art" (Dict.fromList [ ("slug", "Art"), ("owner_screen_name", "misabiko") ]) (Just 9000)
      , initTimeline "Twitter" "Search" "1draw" (Dict.fromList [ ("q", "-filter:retweets #深夜の真剣お絵描き60分一本勝負 OR #東方の90分お絵描き"), ("result_type", "recent") ])  (Just 9000)
      , initTimeline "Twitter" "User Timeline" "User" Dict.empty (Just 9000)
      ]
    , time =
        { zone = Time.utc
        , lastNow = Time.millisToPosix 0
        }
    , sidebar = Collapsed
    }
  , Cmd.batch
      [ Task.perform AdjustTimeZone Time.here
      , Task.perform NewTime Time.now
      , Task.perform identity (Task.succeed RefreshEverything)
      ]
  )


initTimeline : String -> String -> String -> Dict String String -> Maybe Int -> Timeline
initTimeline serviceName endpointName title options interval =
  { title = title
  , serviceName = serviceName
  , endpointName = endpointName
  , articleIds = []
  , options = options
  , interval = interval
  }


initTwitter : (String, Service)
initTwitter =
  ( "Twitter"
  , { name = "Twitter"
    , endpoints = Dict.fromList
        [ initTwitterEndpoint "Home Timeline" [ "statuses", "home_timeline" ] (Just (Service.initRateLimit 15 (15*60*1000)))
        , initTwitterEndpoint "User Timeline" [ "statuses", "user_timeline" ] (Just (Service.initRateLimit 900 (15*60*1000)))
        , initTwitterEndpoint "Search" [ "search", "tweets" ] (Just (Service.initRateLimit 180 (15*60*1000)))
        , initTwitterEndpoint "List" [ "lists", "statuses" ] (Just (Service.initRateLimit 900 (15*60*1000)))
        ]
    , articles = Dict.empty
    }
  )


initTwitterEndpoint : String -> List String -> Maybe RateLimitInfo -> (String, Endpoint)
initTwitterEndpoint name path maybeRateLimit =
  ( name
  , { name = name
    , baseUrl = "http://localhost:5000"
    , path = [ "twitter", "v1" ] ++ path
    , options = [UrlB.string "tweet_mode" "extended"]
    , rateLimit = maybeRateLimit
    }
  )


-- UPDATE


type Msg
  = GotPayload Service Endpoint Timeline (Result Http.Error (Result (List (String, Int)) Payload))
  | GotServicePayload Service (Result Http.Error (Result (List (String, Int)) Payload))
  | Refresh Service Endpoint Timeline
  | RefreshEverything
  | AdjustTimeZone Time.Zone
  | NewTime Time.Posix
  | Like Service Article
  | Repost Service Article
  | HideSidebarMenu
  | ShowSidebarMenu SidebarMenu


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotPayload service endpoint timeline result ->
      case result of
        Ok payloadResult ->
          case payloadResult of
            Ok payload ->
              case payload of
                FreePayload articles timelineArticles ->
                  ( { model
                      | services =
                          Dict.insert service.name
                            { service | articles = Dict.union (Article.listToDict articles) service.articles }
                            model.services
                      , timelines = updateTimelineArticles timelineArticles timeline.title model.timelines
                    }
                  , Task.perform NewTime Time.now
                  )

                RateLimitedPayload articles timelineArticles rateLimit ->
                  ( { model
                      | services =
                          Dict.insert service.name
                            { service
                              | articles = Dict.union (Article.listToDict articles) service.articles
                              , endpoints = Dict.insert endpoint.name { endpoint | rateLimit = Just rateLimit} service.endpoints
                            }
                            model.services
                      , timelines = updateTimelineArticles timelineArticles timeline.title model.timelines
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
            _ = Debug.log "Http Error" reason
          in
            (model, Cmd.none)

    GotServicePayload service result ->
      case result of
        Ok payloadResult ->
          case payloadResult of
            Ok payload ->
              case payload of
                FreePayload articles timelineArticles ->
                  ( { model | services =
                      Dict.insert
                        service.name
                        { service | articles = Dict.union (Article.listToDict articles) service.articles }
                        model.services
                    }
                  , Task.perform NewTime Time.now
                  )

                RateLimitedPayload articles timelineArticles rateLimit ->
                  ( { model | services =
                      Dict.insert
                        service.name
                        { service | articles = Dict.union (Article.listToDict articles) service.articles }
                        model.services
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

    Refresh service endpoint timeline ->
      (model, getEndpoint service endpoint timeline)

    RefreshEverything ->
      (model
      , Cmd.batch
          (List.filterMap (\timeline ->
            case (getTimelineServiceEndpoint model.services timeline) of
              Just (service, endpoint) ->
                Just (getEndpoint service endpoint timeline)

              Nothing -> Nothing
          ) model.timelines)
      )

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

    HideSidebarMenu ->
      ( { model | sidebar = Collapsed}, Cmd.none )

    ShowSidebarMenu menu ->
      ( { model | sidebar = Expanded menu}, Cmd.none )


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


getTimelineServiceEndpoint : Dict String Service -> Timeline -> Maybe (Service, Endpoint)
getTimelineServiceEndpoint services timeline =
  case (Dict.get timeline.serviceName services) of
    Just service ->
      case (Dict.get timeline.endpointName service.endpoints) of
        Just endpoint ->
            Just (service, endpoint)

        Nothing -> Nothing

    Nothing -> Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  List.map (timelineRefreshSub model.services) model.timelines
    |> Maybe.Extra.values
    |> Sub.batch


timelineRefreshSub : Dict String Service -> Timeline -> Maybe (Sub Msg)
timelineRefreshSub services timeline =
  case ((getTimelineServiceEndpoint services timeline), timeline.interval) of
    (Just (service, endpoint), Just interval) ->
      Just (Time.every (toFloat interval) (\_ -> Refresh service endpoint timeline))

    _ -> Nothing


-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "SoshalThing"
  , body =
      [ viewSidebar model
      , viewTimelineContainer model ]
  }


viewSidebar : Model -> Html Msg
viewSidebar model =
  nav [ id "sidebar" ]
    <| case model.sidebar of
        Collapsed ->
          [ div [ id "sidebarButtons" ]
              [ button [ onClick (ShowSidebarMenu ServiceMenu)] [ viewIcon "fa-angle-double-right" "fas" "fa-2x" ] ]
          ]
        
        Expanded menu ->
          [ viewSidebarMenu model menu
          , div [ id "sidebarButtons" ]
              [ button [ onClick HideSidebarMenu ] [ viewIcon "fa-angle-double-left" "fas" "fa-2x" ] ]
          ]


viewSidebarMenu : Model -> SidebarMenu -> Html Msg
viewSidebarMenu model sidebarMenu =
  case sidebarMenu of
    ServiceMenu ->
      lazy viewServiceMenu (Dict.values model.services)


viewServiceMenu : List Service -> Html Msg
viewServiceMenu services =
  div [ class "sidebarMenu" ]
    <| List.map (lazy Service.viewServiceSettings) services


viewTimelineContainer : Model -> Html Msg
viewTimelineContainer model =
  Html.Keyed.node "div" [ id "timelineContainer" ] (List.map (viewKeyedTimeline model) model.timelines)


viewKeyedTimeline : Model -> Timeline -> (String, Html Msg)
viewKeyedTimeline model timeline =
  (timeline.title, viewTimeline model timeline)


viewTimeline : Model -> Timeline -> Html Msg
viewTimeline model timeline =
  case (getTimelineServiceEndpoint model.services timeline) of
    Nothing ->
      text (Debug.log (timeline.title ++ " Error") "Couldn't find service or endpoint.")
    
    Just (service, endpoint) ->
      let
        endpointReady = isReady endpoint
      in
        div [ class "timeline" ]
          [ div [ class "timelineHeader", classList [("timelineInvalid", not endpointReady)] ]
            [ strong [] [ text timeline.title ]
            , div [ class "timelineButtons" ]
                [ button
                    (if endpointReady then [onClick (Refresh service endpoint timeline)] else [])
                    [ viewIcon "fa-sync-alt" "fas" "fa-lg" ] ]
            ]
          , lazy2 (viewContainer service) model.time (Article.getShareableArticles service.articles timeline.articleIds)
          ]


viewContainer : Service -> TimeModel -> List ShareableArticle -> Html Msg
viewContainer service timeModel shareableArticles =
  Html.Keyed.node "div" [ class "timelineArticles" ] (List.map (Tweet.viewKeyedTweet Like Repost timeModel service) shareableArticles)

viewIcon : String -> String -> String -> Html Msg
viewIcon icon iconType size =
  span [ class "icon" ]
    [ i [ class iconType, class icon, class size ] [] ]


viewMaybe : Maybe (Html Msg) -> List (Html Msg)
viewMaybe maybeElement =
  case maybeElement of
    Just element ->
      [element]
    Nothing ->
      []


-- HTTP


postLike : Service -> Article -> Cmd Msg
postLike service article =
  case article.social of
    Just social ->
      Http.post
        { url = UrlB.crossOrigin "http://localhost:5000" ["twitter", "v1", "favorites", if social.liked then "destroy" else "create"] [UrlB.string "id" article.id, UrlB.string "tweet_mode" "extended"]
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
          { url = UrlB.crossOrigin "http://localhost:5000" ["twitter", "v1", "statuses", "retweet"] [UrlB.string "id" article.id, UrlB.string "tweet_mode" "extended"]
          , body = Http.emptyBody
          , expect = Http.expectJson (GotServicePayload service) Tweet.payloadResponseDecoder
          }

    Nothing -> Cmd.none


getEndpoint : Service -> Endpoint -> Timeline -> Cmd Msg
getEndpoint service endpoint timeline =
  Http.get
    { url = UrlB.crossOrigin endpoint.baseUrl endpoint.path (endpoint.options ++ (dictToQueries timeline.options))
    , expect = Http.expectJson (GotPayload service endpoint timeline) Tweet.payloadResponseDecoder
    }


dictToQueries : Dict String String -> List UrlB.QueryParameter
dictToQueries queries =
  List.map (\option -> UrlB.string (Tuple.first option) (Tuple.second option)) (Dict.toList queries)