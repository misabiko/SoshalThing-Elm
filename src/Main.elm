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

import Article exposing (..)
import TimeParser
import Tweet exposing (payloadResponseDecoder, getShareableArticles)


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
  , articles: ArticleCollection
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


type TweetType
  = Tweet
  | Retweet
  | Quote


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
        [ ( "Home Timeline", { name = "Home Timeline", baseUrl = "http://127.0.0.1:5000", path = [ "home_timeline" ] } )
        , ( "User Timeline", { name = "User Timeline", baseUrl = "http://127.0.0.1:5000", path = [ "user_timeline" ] } )
        , ( "Search", { name = "Search", baseUrl = "http://127.0.0.1:5000", path = [ "search" ] } )
        , ( "List", { name = "List", baseUrl = "http://127.0.0.1:5000", path = [ "list" ] } )
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


listToDict : List Article -> ArticleCollection
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
            , viewContainer model.time service (getShareableArticles service.articles timeline.articleIds)
            ]


viewContainer : TimeModel -> Service -> List ShareableArticle -> Html Msg
viewContainer timeModel service shareableArticles =
  Html.Keyed.node "div" [ class "timelineArticles" ] (List.map (viewKeyedTweet timeModel service) shareableArticles)


viewKeyedTweet : TimeModel -> Service -> ShareableArticle -> (String, Html Msg)
viewKeyedTweet timeModel service shareableArticle =
  (getShareableId shareableArticle, lazy3 viewTweet timeModel service shareableArticle)


getShareableId : ShareableArticle -> String
getShareableId shareableArticle =
  case shareableArticle.sharedArticle of
    Just shared -> shareableArticle.article.id ++ shared.id
    Nothing -> shareableArticle.article.id


viewTweetHeader : TimeModel -> Article -> SocialData -> Html Msg
viewTweetHeader timeModel article social =
  div [ class "articleHeader" ]
    [ a [ class "names"
        , href ("https://twitter.com/" ++ social.authorHandle)
        , target "_blank"
        , rel "noopener noreferrer"
        ]
        [ strong [] [ text social.authorName ]
        , small [] [ text ("@" ++ social.authorHandle) ]
        ]
    , span [ class "timestamp" ]
        [ small
          [ title (TimeParser.toFullTimeFormat timeModel article.creationDate) ]
          [ text (TimeParser.relativeTimeFormat timeModel article.creationDate) ]
        ]
    ]


viewTweetButtons : Service -> Article -> SocialData -> Html Msg
viewTweetButtons service article social =
  nav [ class "level", class "is-mobile" ]
    [ div [ class "level-left" ]
        [ a [ class "level-item"
            , class "articleButton"
            , class "repostButton"
            , classList [("repostedPostButton", social.reposted)]
            , onClick (Repost service article)
            ]
            ([ viewIcon "fa-retweet" "fas" "" ]
            ++ (viewMaybe (
              if social.repostCount > 0 then
                Just (span [] [ text (String.fromInt social.repostCount) ])
              else
                Nothing
            )))

        , a [ class "level-item"
            , class "articleButton"
            , class "likeButton"
            , classList [("likedPostButton", social.liked)]
            , onClick (Like service article)
            ]
            ([ viewIcon "fa-heart" (if social.liked then "fas" else "far") "" ]
            ++ (viewMaybe (
              if social.likeCount > 0 then
                Just (span [ ] [ text (String.fromInt social.likeCount) ])
              else
                Nothing
            )))

        , a [ class "level-item", class "articleButton", class "articleMenuButton" ]
            [ viewIcon "fa-ellipsis-h" "fas" "" ]
        ]
    ]


viewMaybe : Maybe (Html Msg) -> List (Html Msg)
viewMaybe maybeElement =
  case maybeElement of
    Just element ->
      [element]
    Nothing ->
      []


type alias TweetSkeletonParts =
  { superHeader: Maybe (Html Msg)
  , extra: Maybe (Html Msg)
  , footer: Maybe (Html Msg)
  }


viewTweetSkeleton : TimeModel -> TweetSkeletonParts -> Service -> Article -> Html Msg
viewTweetSkeleton timeModel parts service article =
  case ((article.text, article.social)) of
    (Just textStr, Just social) ->
      Html.article [ class "article" ]
        ((viewMaybe parts.superHeader)
        ++ [ div [ class "media" ]
          [ figure [ class "media-left" ]
            [ p [ class "image", class "is-64x64" ]
              [ img [ alt (social.authorHandle ++ "'s avatar"), src social.authorAvatar ] [] ]
            ]
          , div [ class "media-content" ]
              ( [ div [ class "content" ]
                  [ (viewTweetHeader timeModel article social)
                  , div [ class "tweet-paragraph" ] [ text textStr ]
                  ]
              ]
              ++ (viewMaybe parts.extra)
              ++ [viewTweetButtons service article social]
              )
          ]
        ]
        ++ (viewMaybe parts.footer)
        )

    _ ->
      Html.article [ class "article" ]
        [ text ("Couldn't find text and social extension for " ++ article.id) ]


getTweetType : ShareableArticle -> TweetType
getTweetType shareableArticle =
  case shareableArticle.sharedArticle of
    Just shared ->
      case shareableArticle.article.text of
        Just _ -> Quote
        Nothing -> Retweet
    Nothing -> Tweet


getActualTweet : ShareableArticle -> Article
getActualTweet shareableArticle =
  case shareableArticle.sharedArticle of
    Just shared ->
      case (getTweetType shareableArticle) of
        Tweet -> shareableArticle.article
        Retweet -> shared
        Quote -> shareableArticle.article
    Nothing -> shareableArticle.article


viewTweet : TimeModel -> Service -> ShareableArticle -> Html Msg
viewTweet timeModel service shareableArticle =
  let
    actualTweet = getActualTweet shareableArticle
    parts =
      { superHeader = getTweetSuperHeader shareableArticle
      , extra = getTweetExtra timeModel shareableArticle
      , footer = getTweetFooter shareableArticle
      }
  in
    viewTweetSkeleton timeModel parts service actualTweet


getRetweetSuperHeader : Article -> Html Msg
getRetweetSuperHeader article =
  case article.social of
    Just social ->
      div [ class "repostLabel" ]
        [ a [ href ("https://twitter.com/" ++ social.authorHandle)
            , target "_blank"
            , rel "noopener noreferrer"
            ]
            [ text (social.authorName ++ " retweeted") ]
        ]
    
    Nothing ->
      div [ class "repostLabel" ] []


getTweetSuperHeader : ShareableArticle -> Maybe (Html Msg)
getTweetSuperHeader shareableArticle =
  case shareableArticle.sharedArticle of
    Just _ ->
      case shareableArticle.article.text of
        Just quoteText ->
          Nothing
        Nothing ->
          Just (getRetweetSuperHeader shareableArticle.article)
    Nothing -> Nothing


getQuoteExtra : TimeModel -> Article -> Html Msg
getQuoteExtra timeModel article =
  case ((article.social, article.text)) of
    (Just social, Just quoteText) ->
      div [ class "quotedPost" ]
        [ viewTweetHeader timeModel article social
        , div [ class "tweet-paragraph" ] [ text quoteText ]
        -- media
        ]

    _ -> div [] []


getTweetExtra : TimeModel -> ShareableArticle -> Maybe (Html Msg)
getTweetExtra timeModel shareableArticle =
  case shareableArticle.sharedArticle of
    Just shared ->
      case shareableArticle.article.text of
        Just quoteText ->
          Just (getQuoteExtra timeModel shared)
        Nothing ->
          Nothing
    Nothing -> Nothing


getMediaFooter : List ImageData -> Html Msg
getMediaFooter images =
  div [ class "postImages", class "postMedia" ]
    (List.map (\imageData ->
      div [ class "mediaHolder" ]
        [ div [ class "is-hidden", class "imgPlaceholder" ] []
        , img [ src imageData.url ] []
        ]
    ) images)


getTweetFooter : ShareableArticle -> Maybe (Html Msg)
getTweetFooter shareableArticle =
  case (getActualTweet shareableArticle).images of
    Just images ->
      Just (getMediaFooter images)

    Nothing -> Nothing


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
        { url = "http://127.0.0.1:5000/" ++ (if social.liked then "unlike/" else "like/") ++ article.id
        , body = Http.emptyBody
        , expect = Http.expectJson (GotServicePayload service) payloadResponseDecoder
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
          { url = "http://127.0.0.1:5000/retweet/" ++ article.id
          , body = Http.emptyBody
          , expect = Http.expectJson (GotServicePayload service) payloadResponseDecoder
          }

    Nothing -> Cmd.none


getEndpoint : Service -> Endpoint -> Timeline -> Cmd Msg
getEndpoint service endpoint timeline =
  Http.get
    { url = UrlB.crossOrigin endpoint.baseUrl endpoint.path (dictToQueries timeline.options)
    , expect = Http.expectJson (GotEndpointPayload service timeline) payloadResponseDecoder
    }


dictToQueries : Dict String String -> List UrlB.QueryParameter
dictToQueries queries =
  List.map (\option -> UrlB.string (Tuple.first option) (Tuple.second option)) (Dict.toList queries)