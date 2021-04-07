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


type alias Timeline a =
  { title: String
  , serviceName: String
  , endpointName: String
  , articleIds: List TimelineArticle
  , options: Dict String String
  , interval: Maybe Int
  , filters: List (Filter a)
  , compactMode: CompactMode
  , sort: SortMethod a
  , showOptions: Bool
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


type SortMethod a
  = Unsorted
  | ById
  | ByCreationDate
  | ByIndex ((Article a) -> Int)


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


updateTimelineArticles : Article.Collection a -> SortMethod a -> List Article.Id -> String -> List (Timeline a) -> List (Timeline a)
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


timelineSortArticles : Article.Collection a -> SortMethod a -> List TimelineArticle -> List TimelineArticle
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


getTimelineServiceEndpoint : Dict String (Service a) -> Timeline a -> Maybe ((Service a), Endpoint)
getTimelineServiceEndpoint services timeline =
  case (Dict.get timeline.serviceName services) of
    Just service ->
      case (Dict.get timeline.endpointName service.endpoints) of
        Just endpoint ->
            Just (service, endpoint)

        Nothing -> Nothing

    Nothing -> Nothing


timelineRefreshSub : (Service a -> Endpoint -> Timeline a -> msg) -> Dict String (Service a) -> Timeline a -> Maybe (Sub msg)
timelineRefreshSub refreshMsg services timeline =
  case ((getTimelineServiceEndpoint services timeline), timeline.interval) of
    (Just (service, endpoint), Just interval) ->
      Just (Time.every (toFloat interval) (\_ -> refreshMsg service endpoint timeline))

    _ -> Nothing