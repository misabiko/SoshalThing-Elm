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

import Article exposing (Article)
import Service exposing (Service, Endpoint, Payload(..), RateLimitInfo)
import Timeline exposing
    ( Timeline, TimelineArticle, timelineArticlesToIds, isCompact, CompactMode(..)
    , updateTimelineArticles, getTimelineServiceEndpoint
    , timelineRefreshSub
    )
import Tweet exposing (Tweet)
import Filter exposing (..)


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
  { services: Dict String (Service Tweet)
  , timelines: List (Timeline Tweet)
  , time : TimeModel
  , sidebar : Sidebar
  }


init : () -> ( Model, Cmd Msg )
init _ =
  ( { services =  Dict.fromList [ initTwitter ]
    , timelines = initTimelines
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


initTimelines : List (Timeline Tweet)
initTimelines =
  [ { title = "Home"
    , serviceName = "Twitter"
    , endpointName = "Home Timeline"
    , articleIds = []
    , options = Dict.empty
    , interval = Just 64285
    , filters = []
    , compactMode = Compact
    , sort = Timeline.ByCreationDate
    , showOptions = False
    }
  , { title = "Art"
    , serviceName = "Twitter"
    , endpointName = "List"
    , articleIds = []
    , options =
        Dict.fromList
          [ ("slug", "Art")
          , ("owner_screen_name", "misabiko")
          ]
    , interval = Just 9000
    , filters =
        [ (HasMedia, ExcludeIfNot)
        , (IsRepost, ExcludeIf)
        ]
    , compactMode = Expand
    , sort = Timeline.ByCreationDate
    , showOptions = False
    }
  , { title = "1draw"
    , serviceName = "Twitter"
    , endpointName = "Search"
    , articleIds = []
    , options =
        Dict.fromList
          [ ("q", "-filter:retweets #深夜の真剣お絵描き60分一本勝負 OR #東方の90分お絵描き")
          , ("result_type", "recent")
          ]
    , interval = Just 9000
    , filters =
        [ (HasMedia, ExcludeIfNot)
        , (IsRepost, ExcludeIf)
        ]
    , compactMode = Compact
    , sort = Timeline.ByCreationDate
    , showOptions = False
    }
  , { title = "User"
    , serviceName = "Twitter"
    , endpointName = "User Timeline"
    , articleIds = []
    , options = Dict.empty
    , interval = Just 9000
    , filters = []
    , compactMode = Compact
    , sort = Timeline.ByCreationDate
    , showOptions = False
    }
  ]


initTwitter : (String, (Service Tweet))
initTwitter =
  ( "Twitter"
  , { name = "Twitter"
    , endpoints = Dict.fromList
        [ initTwitterEndpoint
            "Home Timeline"
            [ "statuses", "home_timeline" ]
            [UrlB.string "count" "200"]
            (Just (Service.initRateLimit 15 (15*60*1000)))
        , initTwitterEndpoint
            "User Timeline"
            [ "statuses", "user_timeline" ]
            []
            (Just (Service.initRateLimit 900 (15*60*1000)))
        , initTwitterEndpoint
            "Search"
            [ "search", "tweets" ]
            []
            (Just (Service.initRateLimit 180 (15*60*1000)))
        , initTwitterEndpoint
            "List"
            [ "lists", "statuses" ]
            []
            (Just (Service.initRateLimit 900 (15*60*1000)))
        ]
    , articles = Dict.empty
    }
  )


initTwitterEndpoint : String -> List String -> List UrlB.QueryParameter -> Maybe RateLimitInfo -> (String, Endpoint)
initTwitterEndpoint name path options maybeRateLimit =
  ( name
  , Service.newEndpoint
    { name = name
    , baseUrl = "http://localhost:5000"
    , path = [ "twitter", "v1" ] ++ path
    , options = (UrlB.string "tweet_mode" "extended") :: options
    , rateLimit = maybeRateLimit
    }
  )


-- UPDATE


type Msg
  = GotPayload (Service Tweet) Endpoint (Timeline Tweet) (Result Http.Error (Result (List (String, Int)) (Payload Tweet)))
  | GotServicePayload (Service Tweet) (Result Http.Error (Result (List (String, Int)) (Payload Tweet)))
  | Refresh (Service Tweet) Endpoint (Timeline Tweet)
  | RefreshEverything
  | AdjustTimeZone Time.Zone
  | NewTime Time.Posix
  | Like (Service Tweet) (Article Tweet)
  | Repost (Service Tweet) (Article Tweet)
  | HideSidebarMenu
  | ShowSidebarMenu SidebarMenu
  | DebugArticle (Article Tweet)


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
                              , endpoints = Service.updateEndpointRateLimit service.endpoints endpoint rateLimit
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
                FreePayload articles _ ->
                  ( { model | services =
                      Dict.insert
                        service.name
                        { service | articles = Dict.union (Article.listToDict articles) service.articles }
                        model.services
                    }
                  , Task.perform NewTime Time.now
                  )

                RateLimitedPayload articles _ _ ->
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
            getTimelineServiceEndpoint model.services timeline
              |> Maybe.andThen (\(service, endpoint) ->
                Just (getEndpoint service endpoint timeline)
              )
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

    DebugArticle article ->
      let
        _ = Debug.log "Article" article
      in
      (model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  List.map (timelineRefreshSub Refresh model.services) model.timelines
    |> Maybe.Extra.values
    |> Sub.batch


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


viewServiceMenu : List (Service Tweet) -> Html Msg
viewServiceMenu services =
  div [ class "sidebarMenu" ]
    <| List.map (lazy Service.viewServiceSettings) services


viewTimelineContainer : Model -> Html Msg
viewTimelineContainer model =
  Html.Keyed.node "div" [ id "timelineContainer" ] (List.map (viewKeyedTimeline model) model.timelines)


viewKeyedTimeline : Model -> (Timeline Tweet) -> (String, Html Msg)
viewKeyedTimeline model timeline =
  (timeline.title, viewTimeline model timeline)


viewTimeline : Model -> (Timeline Tweet) -> Html Msg
viewTimeline model timeline =
  case (getTimelineServiceEndpoint model.services timeline) of
    Nothing ->
      text (Debug.log (timeline.title ++ " Error") "Couldn't find service or endpoint.")
    
    Just (service, endpoint) ->
      let
        endpointReady = Service.isReady endpoint
      in
        div [ class "timeline" ]
          [ div [ class "timelineHeader", classList [("timelineInvalid", not endpointReady)] ]
            [ strong [] [ text timeline.title ]
            , div [ class "timelineButtons" ]
                [ button
                    (if endpointReady then [onClick (Refresh service endpoint timeline)] else [])
                    [ viewIcon "fa-sync-alt" "fas" "fa-lg" ] ]
            ]
          , lazy4 (viewContainer service) model.time timeline.filters timeline.compactMode timeline.articleIds
          ]


viewContainer : (Service Tweet) -> TimeModel -> List (Filter Tweet) -> CompactMode -> List TimelineArticle -> Html Msg
viewContainer service timeModel filters timelineCompact timelineArticles =
  Html.Keyed.node "div" [ class "timelineArticles" ]
    ( List.map
        (Tweet.viewKeyedTweet timeModel service)
        (Filter.filterArticles filters (Service.getTimelineArticles service (List.map .id timelineArticles)))
    )


viewIcon : String -> String -> String -> Html Msg
viewIcon icon iconType size =
  span [ class "icon" ]
    [ i [ class iconType, class icon, class size ] [] ]


-- HTTP


postLike : (Service Tweet) -> (Article Tweet) -> Cmd Msg
postLike service article =
  case article.social of
    Just social ->
      Http.post
        { url = UrlB.crossOrigin "http://localhost:5000" ["twitter", "v1", "favorites", if social.liked then "destroy" else "create"] [UrlB.string "id" article.id, UrlB.string "tweet_mode" "extended"]
        , body = Http.emptyBody
        , expect = Http.expectJson (GotServicePayload service) Tweet.payloadResponseDecoder
        }

    Nothing -> Cmd.none


postRetweet : (Service Tweet) -> (Article Tweet) -> Cmd Msg
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


getEndpoint : (Service Tweet) -> Endpoint -> (Timeline Tweet) -> Cmd Msg
getEndpoint service endpoint timeline =
  let
    endpointData = Service.unwrapEndpoint endpoint
  in
  Http.get
    { url = UrlB.crossOrigin endpointData.baseUrl endpointData.path (endpointData.options ++ (dictToQueries timeline.options))
    , expect = Http.expectJson (GotPayload service endpoint timeline) Tweet.payloadResponseDecoder
    }


dictToQueries : Dict String String -> List UrlB.QueryParameter
dictToQueries queries =
  List.map (\option -> UrlB.string (Tuple.first option) (Tuple.second option)) (Dict.toList queries)