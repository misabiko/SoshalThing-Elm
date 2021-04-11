module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Html.Keyed
import Dict exposing (Dict)
import Array exposing (Array)
import Http
import Time
import Task
import Url.Builder as UrlB
import Maybe.Extra

import Article exposing (Article)
import Service exposing (Service(..), Endpoint, Payload(..), RateLimitInfo)
import Timeline exposing
    ( Timeline, ViewTimeline, TimelineArticle, timelineArticlesToIds, isCompact, CompactMode(..)
    , updateTimelineArticles
    --, getTimelineServiceEndpoint, timelineRefreshSub
    )
import Tweet exposing (ArticleExt)
import Filter exposing (..)
import Timeline exposing (SortMethod(..))
import TimeParser exposing (TimeModel)
import Extra exposing (viewIcon)
import Twitter.Service exposing (TwitterService)


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


type SidebarMenu
  = ServiceMenu


type Sidebar
  = Collapsed
  | Expanded SidebarMenu


type alias Model =
  { services: Dict String TwitterService
  , timelines: List (Timeline ArticleExt)
  , time : TimeModel
  , sidebar : Sidebar
  }


init : () -> ( Model, Cmd Msg )
init _ =
  ( { services =  Dict.fromList (List.map (\service -> ((Service.unwrap service).name, service)) [ Twitter.Service.init ])
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


initTimelines : List (Timeline ArticleExt)
initTimelines =
  [ { title = "Home"
    , serviceName = "Twitter"
    , endpointName = "Home Timeline"
    , articleIds = []
    , options = Dict.empty
    , interval = Just 64285
    , filters = Array.fromList []
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
        Array.fromList
          [ { enabled = True, method = HasMedia, mode = ExcludeIfNot }
          , { enabled = True, method = IsRepost, mode = ExcludeIf }
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
        Array.fromList
          [ { enabled = True, method = HasMedia, mode = ExcludeIfNot }
          , { enabled = True, method = IsRepost, mode = ExcludeIf }
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
    , filters = Array.fromList []
    , compactMode = Compact
    , sort = Timeline.ByCreationDate
    , showOptions = False
    }
  ]


-- UPDATE


type Msg
  = GotPayload TwitterService Endpoint (Timeline ArticleExt) (Result Http.Error (Result (List (String, Int)) (Payload ArticleExt)))
  | GotServicePayload TwitterService (Result Http.Error (Result (List (String, Int)) (Payload ArticleExt)))
  | Refresh TwitterService Endpoint (Timeline ArticleExt)
  | RefreshEverything
  | AdjustTimeZone Time.Zone
  | NewTime Time.Posix
  | Like TwitterService (Article ArticleExt)
  | Repost TwitterService (Article ArticleExt)
  | HideSidebarMenu
  | ShowSidebarMenu SidebarMenu
  | DebugArticle (Article ArticleExt)
  | GotTimelineMsg (Timeline.Msg ArticleExt)


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
                  let
                    serviceData = Service.unwrap service
                  in
                  ( { model
                      | services =
                          Dict.insert serviceData.name
                            (Service { serviceData | articles = Dict.union (Article.listToDict articles) serviceData.articles })
                            model.services
                      , timelines = updateTimelineArticles serviceData.articles timeline.sort timelineArticles timeline.title model.timelines
                    }
                  , Task.perform NewTime Time.now
                  )

                RateLimitedPayload articles timelineArticles rateLimit ->
                  let
                    serviceData = Service.unwrap service
                  in
                  ( { model
                      | services =
                          Dict.insert serviceData.name
                            (Service { serviceData
                              | articles = Dict.union (Article.listToDict articles) serviceData.articles
                              , endpoints = Service.updateEndpointRateLimit serviceData.endpoints endpoint rateLimit
                            })
                            model.services
                      , timelines = updateTimelineArticles serviceData.articles timeline.sort timelineArticles timeline.title model.timelines
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
                  let
                    serviceData = Service.unwrap service
                  in
                  ( { model | services =
                      Dict.insert
                        serviceData.name
                        (Service { serviceData | articles = Dict.union (Article.listToDict articles) serviceData.articles })
                        model.services
                    }
                  , Task.perform NewTime Time.now
                  )

                RateLimitedPayload articles _ _ ->
                  let
                    serviceData = Service.unwrap service
                  in
                  ( { model | services =
                      Dict.insert
                        serviceData.name
                        (Service { serviceData | articles = Dict.union (Article.listToDict articles) serviceData.articles })
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
      --, Cmd.batch
      --    (List.filterMap (\timeline ->
      --      getTimelineServiceEndpoint model.services timeline
      --        |> Maybe.andThen (\(service, endpoint) ->
      --          Just (getEndpoint service endpoint timeline)
      --        )
      --    ) model.timelines)
      , Cmd.none
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

    GotTimelineMsg subMsg ->
      (model, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  --List.map (timelineRefreshSub Refresh model.services) model.timelines
  --  |> Maybe.Extra.values
  --  |> Sub.batch
  Sub.none


-- VIEW


view : Model -> Browser.Document Msg
view model =
  let
    timelines = List.filterMap identity
      (List.map
        (\timeline ->
          Maybe.andThen
            (\service -> Timeline.toViewTimeline service timeline)
            (Dict.get timeline.serviceName model.services)
        )
        model.timelines
      )
  in
  { title = "SoshalThing"
  , body =
      [ viewSidebar model
      , Html.map GotTimelineMsg (Timeline.viewTimelineContainer model.time timelines)
      ]
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


viewServiceMenu : List TwitterService -> Html Msg
viewServiceMenu services =
  div [ class "sidebarMenu" ]
    <| List.map (lazy Service.viewServiceSettings) services


-- HTTP


postLike : TwitterService -> (Article ArticleExt) -> Cmd Msg
postLike service article =
  let
    social =
      case article.ext of
        Tweet.Tweet tweet -> tweet.social
        Tweet.Retweet retweet -> retweet.social
        Tweet.Quote quote -> quote.social
  in
    Http.post
      { url = UrlB.crossOrigin "http://localhost:5000" ["twitter", "v1", "favorites", if social.liked then "destroy" else "create"] [UrlB.string "id" article.id, UrlB.string "tweet_mode" "extended"]
      , body = Http.emptyBody
      , expect = Http.expectJson (GotServicePayload service) Tweet.payloadResponseDecoder
      }


postRetweet : TwitterService -> (Article ArticleExt) -> Cmd Msg
postRetweet service article =
  let
    social =
      case article.ext of
        Tweet.Tweet tweet -> tweet.social
        Tweet.Retweet retweet -> retweet.social
        Tweet.Quote quote -> quote.social
  in
    if social.reposted then
      Cmd.none
    else
      Http.post
        { url = UrlB.crossOrigin "http://localhost:5000" ["twitter", "v1", "statuses", "retweet"] [UrlB.string "id" article.id, UrlB.string "tweet_mode" "extended"]
        , body = Http.emptyBody
        , expect = Http.expectJson (GotServicePayload service) Tweet.payloadResponseDecoder
        }


getEndpoint : TwitterService -> Endpoint -> (Timeline ArticleExt) -> Cmd Msg
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