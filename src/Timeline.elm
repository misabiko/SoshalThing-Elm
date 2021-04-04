module Timeline exposing
    ( Timeline
    , updateTimelineArticles, timelineSortArticles, getTimelineServiceEndpoint
    , timelineRefreshSub
    )

import Dict exposing (Dict)
import Time

import Article exposing (Article, ShareableArticle)
import Service exposing (Service, Endpoint)
import Filter exposing (..)


type alias Timeline =
  { title: String
  , serviceName: String
  , endpointName: String
  , articleIds: List String
  , options: Dict String String
  , interval: Maybe Int
  , filters: List Filter
  }

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


timelineRefreshSub : (Service -> Endpoint -> Timeline -> msg) -> Dict String Service -> Timeline -> Maybe (Sub msg)
timelineRefreshSub refreshMsg services timeline =
  case ((getTimelineServiceEndpoint services timeline), timeline.interval) of
    (Just (service, endpoint), Just interval) ->
      Just (Time.every (toFloat interval) (\_ -> refreshMsg service endpoint timeline))

    _ -> Nothing