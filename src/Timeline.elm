module Timeline exposing
    ( Timeline, TimelineArticle, TimelineShareable, timelineArticlesToIds, timelineArticlesToShareable, isCompact, CompactMode(..)
    , updateTimelineArticles, timelineSortArticles, getTimelineServiceEndpoint
    , timelineRefreshSub
    )

import Dict exposing (Dict)
import Time

import Article exposing (Article, ShareableArticle, getShareableArticles)
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
  }


type alias TimelineArticle =
    { id: Article.Id
    , compact: SettingOverride CompactMode
    }


type alias TimelineShareable =
    { shareableArticle: ShareableArticle
    , compact: SettingOverride CompactMode
    }


type SettingOverride setting
    = Inherit
    | Overrided setting


type CompactMode
    = Compact
    | Expand


newTimelineArticle : Article.Id -> TimelineArticle
newTimelineArticle id =
  { id = id
  , compact = Inherit
  }


isCompact : CompactMode -> TimelineShareable -> Bool
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


updateTimelineArticles : List Article.Id -> String -> List Timeline -> List Timeline
updateTimelineArticles articleIds timelineTitle timelines =
  List.map (\timeline -> 
    if timeline.title == timelineTitle then
      { timeline | articleIds = timelineSortArticles (appendMissingArticleIds timeline.articleIds articleIds) }
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


timelineArticlesToShareable : List TimelineArticle -> List ShareableArticle -> List TimelineShareable
timelineArticlesToShareable timelineArticles shareableArticles =
  List.map2 (
    \timelineArticle shareableArticle ->
      TimelineShareable shareableArticle timelineArticle.compact
  ) timelineArticles shareableArticles 


timelineSortArticles : List TimelineArticle -> List TimelineArticle
timelineSortArticles articleIds =
  articleIds
    |> List.sortBy (\article -> Maybe.withDefault 0 (String.toInt (article.id)))
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


timelineRefreshSub : (Service -> Endpoint -> Timeline -> msg) -> Dict String Service -> Timeline -> Maybe (Sub msg)
timelineRefreshSub refreshMsg services timeline =
  case ((getTimelineServiceEndpoint services timeline), timeline.interval) of
    (Just (service, endpoint), Just interval) ->
      Just (Time.every (toFloat interval) (\_ -> refreshMsg service endpoint timeline))

    _ -> Nothing