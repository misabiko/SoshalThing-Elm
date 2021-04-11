module Timeline exposing
    ( Timeline, TimelineArticle, ViewTimeline, toViewTimeline, timelineArticlesToIds, isCompact, CompactMode(..)
    , updateTimelineArticles--, getTimelineServiceEndpoint, timelineRefreshSub
    , SortMethod(..)
    , viewTimelineContainer, Msg
    )

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Html.Keyed
import Dict exposing (Dict)
import Array exposing (Array)
import Time

import Article exposing (Article)
import Service exposing (Service(..), Endpoint)
import Filter exposing (..)
import TimeParser exposing (TimeModel)
import Extra exposing (viewIcon)


type alias Timeline ext =
  { title: String
  , serviceName: String
  , endpointName: String
  , articleIds: List TimelineArticle
  , options: Dict String String
  , interval: Maybe Int
  , filters: Array (Filter ext)
  , compactMode: CompactMode
  , sort: SortMethod ext
  , showOptions: Bool
  }


type alias ViewTimeline ext vExt =
  { title: String
  , service: Service ext vExt
  , endpoint: Endpoint
  , articleIds: List TimelineArticle
  , options: Dict String String
  , interval: Maybe Int
  , filters: Array (Filter ext)
  , compactMode: CompactMode
  , sort: SortMethod ext
  , showOptions: Bool
  , data: Timeline ext
  }


toViewTimeline : Service ext vExt -> Timeline ext -> Maybe (ViewTimeline ext vExt)
toViewTimeline service timeline =
  Maybe.andThen
    (\endpoint ->
      Just
        { title = timeline.title
        , service = service
        , endpoint = endpoint
        , articleIds = timeline.articleIds
        , options = timeline.options
        , interval = timeline.interval
        , filters = timeline.filters
        , compactMode = timeline.compactMode
        , sort = timeline.sort
        , showOptions = timeline.showOptions
        , data = timeline
        }
    )
    (Dict.get timeline.endpointName (Service.unwrap service).endpoints)



type alias TimelineArticle =
  { id: Article.Id
  , compact: SettingOverride CompactMode
  }


type SettingOverride setting
  = Inherit
  | Overrided setting


type CompactMode
  = Compact
  | Expand


type SortMethod ext
  = Unsorted
  | ById
  | ByCreationDate
  | ByIndex ((Article ext) -> Int)


type Msg ext
  = GotServiceMsg Service.Msg
  | Refresh (Timeline ext)


newTimelineArticle : Article.Id -> TimelineArticle
newTimelineArticle id =
  { id = id
  , compact = Inherit
  }


isCompact : CompactMode -> TimelineArticle -> Bool
isCompact timelineCompactMode timelineShareable =
  case (timelineShareable.compact) of
    Overrided compactMode ->
      case compactMode of
        Compact -> True
        Expand -> False

    Inherit ->
      case timelineCompactMode of
        Compact -> True
        Expand -> False


updateTimelineArticles : Article.Collection ext -> SortMethod ext -> List Article.Id -> String -> List (Timeline ext) -> List (Timeline ext)
updateTimelineArticles articles sortMethod articleIds timelineTitle timelines =
  List.map (\timeline -> 
    if timeline.title == timelineTitle then
      { timeline | articleIds = timelineSortArticles articles sortMethod (appendMissingArticleIds timeline.articleIds articleIds) }
    else
      timeline)
    timelines


appendMissingArticleIds : List TimelineArticle -> List Article.Id -> List TimelineArticle
appendMissingArticleIds timelineArticles newArticles =
  timelineArticles ++ (List.filterMap (\val ->
    if List.member val (timelineArticlesToIds timelineArticles) then
      Nothing
    else
      Just (newTimelineArticle val)
  ) newArticles)


timelineArticlesToIds : List TimelineArticle -> List Article.Id
timelineArticlesToIds timelineArticles =
  List.map (\article -> article.id) timelineArticles


timelineSortArticles : Article.Collection ext -> SortMethod ext -> List TimelineArticle -> List TimelineArticle
timelineSortArticles articles sortMethod articleIds =
  case sortMethod of
    Unsorted -> articleIds

    ByCreationDate ->
      List.sortBy (\timelineArticle ->
        case (Dict.get timelineArticle.id articles) of
          Just article ->
            Time.posixToMillis article.creationDate

          Nothing -> 0
      ) articleIds

    ById ->
      articleIds
        |> List.sortBy (\timelineArticle -> Maybe.withDefault 0 (String.toInt (timelineArticle.id)))
        |> List.reverse

    ByIndex getIndex ->
      List.sortBy (\timelineArticle ->
        case (Dict.get timelineArticle.id articles) of
          Just article ->
            getIndex article

          Nothing -> 0
      ) articleIds


--getTimelineServiceEndpoint : Dict String (Service ext vExt) -> Timeline ext -> Maybe ((Service ext vExt), Endpoint)
--getTimelineServiceEndpoint services timeline =
--  case (Dict.get timeline.serviceName services) of
--    Just (Service service) ->
--      case (Dict.get timeline.endpointName service.endpoints) of
--        Just endpoint ->
--            Just (service, endpoint)

--        Nothing -> Nothing

--    Nothing -> Nothing


--timelineRefreshSub : (Service ext vExt -> Endpoint -> Timeline ext -> msg) -> Dict String (Service ext vExt) -> Timeline ext -> Maybe (Sub msg)
--timelineRefreshSub refreshMsg services timeline =
--  case ((getTimelineServiceEndpoint services timeline), timeline.interval) of
--    (Just (service, endpoint), Just interval) ->
--      Just (Time.every (toFloat interval) (\_ -> refreshMsg service endpoint timeline))

--    _ -> Nothing


-- VIEW


viewTimelineContainer : TimeModel -> List (ViewTimeline ext vExt) -> Html (Msg ext)
viewTimelineContainer time timelines =
  Html.Keyed.node "div" [ id "timelineContainer" ] (List.map (viewKeyedTimeline time) timelines)


viewKeyedTimeline : TimeModel -> ViewTimeline ext vExt -> (String, Html (Msg ext))
viewKeyedTimeline time vTimeline =
  (vTimeline.title, viewTimeline time vTimeline)


viewTimeline : TimeModel -> ViewTimeline ext vExt -> Html (Msg ext)
viewTimeline time vTimeline =
  let
    endpointReady = Service.isReady vTimeline.endpoint
    service = Service.unwrap vTimeline.service
  in
    div [ class "timeline" ]
      [ div [ class "timelineHeader", classList [("timelineInvalid", not endpointReady)] ]
        [ strong [] [ text vTimeline.title ]
        , div [ class "timelineButtons" ]
            [ button
                (if endpointReady then [onClick (Refresh vTimeline.data)] else [])
                [ viewIcon "fa-sync-alt" "fas" "fa-lg" ] ]
        ]
      , lazy4 (viewContainer vTimeline.service) time vTimeline.filters vTimeline.compactMode vTimeline.articleIds
      ]


viewContainer : Service ext vExt -> TimeModel -> Array (Filter ext) -> CompactMode -> List TimelineArticle -> Html (Msg ext)
viewContainer service timeModel filters timelineCompact timelineArticles =
  let
    serviceData = Service.unwrap service
  in
  Html.Keyed.node "div" [ class "timelineArticles" ]
    ( List.map2
        (\compact article ->
          (article.id
          , Html.map GotServiceMsg (serviceData.viewArticle service timeModel compact article)
          )
        )
        (List.map (isCompact timelineCompact) timelineArticles)
        ( List.map .id timelineArticles
          |> Service.getTimelineArticles serviceData
          |> Filter.filterArticles filters
          |> List.map (serviceData.toViewArticle serviceData.articles)
          |> List.filterMap identity
        )
    )