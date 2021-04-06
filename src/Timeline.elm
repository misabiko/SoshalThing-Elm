module Timeline exposing
    ( Timeline, TimelineArticle, timelineArticlesToIds, isCompact, CompactMode(..)
    , updateTimelineArticles, getTimelineServiceEndpoint
    , timelineRefreshSub, SortMethod(..)
    )

import Dict exposing (Dict)
import Time
import List.Extra as ListE

import Article exposing (Article)
import Service exposing (Service, Endpoint)
import Filter exposing (..)


type alias Timeline =
  { title: String
  , serviceName: String
  , endpointName: String
  , articleIds: List TimelineArticle
  , options: Dict String String
  , interval: Maybe Int
  , filters: List Filter
  , compactMode: CompactMode
  , sort: SortMethod
  }


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


type SortMethod
  = Unsorted
  | ById
  | ByCreationDate
  | ByIndex


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


updateTimelineArticles : Article.Collection a -> SortMethod -> List Article.Id -> String -> List Timeline -> List Timeline 
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


timelineSortArticles : Article.Collection a -> SortMethod -> List TimelineArticle -> List TimelineArticle
timelineSortArticles articles sortMethod articleIds =
  case sortMethod of
    ById ->
      articleIds
        |> List.sortBy (\timelineArticle -> Maybe.withDefault 0 (String.toInt (timelineArticle.id)))
        |> List.reverse

    ByIndex ->
      --List.sortBy (\timelineArticle ->
      --    Maybe.withDefault 0
      --      (
      --        case (Dict.get timelineArticle.id articles) of
      --          Just article ->
      --            article.index

      --          Nothing -> Nothing
      --      )
      --)
      articleIds

    _ -> articleIds


getTimelineServiceEndpoint : Dict String (Service a) -> Timeline -> Maybe ((Service a), Endpoint)
getTimelineServiceEndpoint services timeline =
  case (Dict.get timeline.serviceName services) of
    Just service ->
      case (Dict.get timeline.endpointName service.endpoints) of
        Just endpoint ->
            Just (service, endpoint)

        Nothing -> Nothing

    Nothing -> Nothing


timelineRefreshSub : ((Service a) -> Endpoint -> Timeline -> msg) -> Dict String (Service a) -> Timeline -> Maybe (Sub msg)
timelineRefreshSub refreshMsg services timeline =
  case ((getTimelineServiceEndpoint services timeline), timeline.interval) of
    (Just (service, endpoint), Just interval) ->
      Just (Time.every (toFloat interval) (\_ -> refreshMsg service endpoint timeline))

    _ -> Nothing