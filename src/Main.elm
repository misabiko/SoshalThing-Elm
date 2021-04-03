module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Dict exposing (Dict)
import Http
import Time
import Task
import Url.Builder as UrlB
import Json.Decode as Decode exposing (Decoder, field, string, bool, int, maybe)
import Json.Decode.Pipeline as DecodeP

import TimeParser


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


type ArticleExtension
  = Social SocialData
  | Text String
  | Share String


type alias SocialData =
  { authorName: String
  , authorHandle: String
  , authorAvatar: String
  , liked: Bool
  , reposted: Bool
  , likeCount: Int
  , repostCount: Int
  }


type alias ShareableArticle =
  { article: Article
  , sharedArticle: Maybe Article
  }


type alias Article =
  { id: String
  , creationDate: Time.Posix
  , extensions : Dict String ArticleExtension
  }


type alias ArticleCollection =
  Dict String Article


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


type alias Payload =
  Result (List (String, Int)) EndpointPayload


type Msg
  = GotPayload Service Timeline (Result Http.Error Payload)
  | Refresh Service Endpoint Timeline
  | AdjustTimeZone Time.Zone
  | NewTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotPayload service timeline result ->
      case result of
        Ok payloadResult ->
          case payloadResult of
            Ok endpointPayload ->
              updateArticles service timeline endpointPayload model

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
      , div [ id "timelineContainer" ] (List.map (viewTimeline model) model.timelines) ]
  }


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
            , viewContainer model.time (getArticles service.articles timeline.articleIds)
            ]


getArticles : ArticleCollection -> List String -> List ShareableArticle
getArticles articles ids =
  List.filterMap
    (\id -> 
      case (Dict.get id articles) of
        Just article ->
          case (getShareData article) of
            Just sharedId ->
              case (Dict.get sharedId articles) of
                Just sharedArticle ->
                  Just (ShareableArticle article (Just sharedArticle))
                
                Nothing ->
                  Just (ShareableArticle
                  article
                  (Debug.log
                    ("Couldn't find shared article '" ++ sharedId ++ "'")
                    Nothing))

            Nothing ->
              Just (ShareableArticle article Nothing)

        Nothing -> Nothing
    )
    ids


viewContainer : TimeModel -> List ShareableArticle -> Html Msg
viewContainer timeModel shareableArticles =
  div [ class "timelineArticles" ] (List.map (viewTweet timeModel) shareableArticles)


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


viewTweetButtons : Article -> SocialData -> Html Msg
viewTweetButtons article social =
  nav [ class "level", class "is-mobile" ]
    [ div [ class "level-left" ]
        [ a [ class "level-item", class "articleButton", class "repostButton" ]
            [ viewIcon "fa-retweet" "fas" ""
            , span [] [ text (String.fromInt social.repostCount) ]
            ]
        , a [ class "level-item", class "articleButton", class "likeButton" ]
            [ viewIcon "fa-heart" (if social.liked then "fas" else "far") ""
            , span [] [ text (String.fromInt social.likeCount) ]
            ]
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


getTextData : Article -> Maybe String
getTextData article =
  case (Dict.get "Text" article.extensions) of
    Just ext ->
      case ext of
        Text text -> Just text

        _ -> Nothing
    
    Nothing -> Nothing


getSocialData : Article -> Maybe SocialData
getSocialData article =
  case (Dict.get "Social" article.extensions) of
    Just ext ->
      case ext of
        Social data -> Just data

        _ -> Nothing
    
    Nothing -> Nothing


getShareData : Article -> Maybe String
getShareData article =
  case (Dict.get "Share" article.extensions) of
    Just ext ->
      case ext of
        Share data -> Just data

        _ -> Nothing
    
    Nothing -> Nothing


viewTweetSkeleton : TimeModel -> TweetSkeletonParts -> Article -> Html Msg
viewTweetSkeleton timeModel parts article =
  case ((getTextData article, getSocialData article)) of
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
              ++ [viewTweetButtons article social]
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
      case (getTextData shareableArticle.article) of
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


viewTweet : TimeModel -> ShareableArticle -> Html Msg
viewTweet timeModel shareableArticle =
  let
    actualTweet = getActualTweet shareableArticle
    parts =
      { superHeader = getTweetSuperHeader shareableArticle
      , extra = getTweetExtra timeModel shareableArticle
      , footer = Nothing
      }
  in
    viewTweetSkeleton timeModel parts actualTweet


getRetweetSuperHeader : Article -> Html Msg
getRetweetSuperHeader article =
  case (getSocialData article) of
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
      case (getTextData shareableArticle.article) of
        Just quoteText ->
          Nothing
        Nothing ->
          Just (getRetweetSuperHeader shareableArticle.article)
    Nothing -> Nothing


getQuoteExtra : TimeModel -> Article -> Html Msg
getQuoteExtra timeModel article =
  case ((getSocialData article, getTextData article)) of
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
      case (getTextData shareableArticle.article) of
        Just quoteText ->
          Just (getQuoteExtra timeModel shared)
        Nothing ->
          Nothing
    Nothing -> Nothing


-- HTTP


type alias DecodedTweet =
  { tweet: Article
  , quotedTweet: Maybe Article
  }


type alias EndpointPayload =
  { articles: List Article
  , timelineArticles: List String
  }


getEndpoint : Service -> Endpoint -> Timeline -> Cmd Msg
getEndpoint service endpoint timeline =
  Http.get
    { url = UrlB.crossOrigin endpoint.baseUrl endpoint.path (dictToQueries timeline.options)
    , expect = Http.expectJson (GotPayload service timeline) payloadResponseDecoder
    }


dictToQueries : Dict String String -> List UrlB.QueryParameter
dictToQueries queries =
  List.map (\option -> UrlB.string (Tuple.first option) (Tuple.second option)) (Dict.toList queries)


unpackDecodedTweets : List DecodedTweet -> EndpointPayload
unpackDecodedTweets decodedTweets =
  List.map unpackDecodedTweet decodedTweets
  |> List.unzip
  |> Tuple.mapFirst List.concat
  |> (\tuple -> { articles = Tuple.first tuple, timelineArticles = Tuple.second tuple })


unpackDecodedTweet : DecodedTweet -> (List Article, String)
unpackDecodedTweet decodedTweet =
  ( case decodedTweet.quotedTweet of
          Just tweet ->
            [decodedTweet.tweet, tweet]
          Nothing ->
            [decodedTweet.tweet]
  , decodedTweet.tweet.id
  )


payloadDecoder : Decoder EndpointPayload
payloadDecoder =
  Decode.andThen (\maybeStatuses ->
    case maybeStatuses of
      Just statuses ->
        Decode.succeed statuses
      _ ->
        (Decode.map unpackDecodedTweets (Decode.list tweetDecoder)))
    (maybe (field "statuses" (Decode.map unpackDecodedTweets (Decode.list tweetDecoder))))


tweetDecoder : Decoder DecodedTweet
tweetDecoder =
  Decode.map2 DecodedTweet
    topTweetDecoder
    (Decode.maybe
      (Decode.oneOf
        [ field "quoted_status" topTweetDecoder
        , field "retweeted_status" topTweetDecoder
        ]
      ))


topTweetDecoder : Decoder Article
topTweetDecoder =
  Decode.succeed Article
    |> DecodeP.required "id_str" string
    |> DecodeP.custom (Decode.andThen
      TimeParser.tweetTimeDecoder
      (field "created_at" string)
    )
    |> DecodeP.custom extensionsDecoder


extensionsDecoder : Decoder (Dict String ArticleExtension)
extensionsDecoder =
  Decode.map Dict.fromList
    (Decode.map maybeListCollapse (
      (Decode.succeed List.append
        |> DecodeP.custom
            (Decode.succeed List.append
              |> DecodeP.custom (extensionDecoder "Social" socialDecoder)
              |> DecodeP.custom (extensionDecoder "Share" shareDecoder)
            )
        |> DecodeP.custom (extensionDecoder "Text" textDecoder)
      )
    ))


extensionDecoder : String -> Decoder ArticleExtension -> Decoder (List (Maybe (String, ArticleExtension)))
extensionDecoder key decoder =
  Decode.map List.singleton (Decode.maybe (Decode.map2 Tuple.pair (Decode.succeed key) decoder))


maybeListCollapse : List (Maybe a) -> List a
maybeListCollapse maybes =
  List.filterMap (\maybeElement -> maybeElement) maybes


socialDecoder : Decoder ArticleExtension
socialDecoder =
  Decode.succeed SocialData
    |> DecodeP.requiredAt ["user", "name"] string
    |> DecodeP.requiredAt ["user", "screen_name"] string
    |> DecodeP.requiredAt ["user", "profile_image_url_https"] string
    |> DecodeP.required "favorited" bool
    |> DecodeP.required "retweeted" bool
    |> DecodeP.required "favorite_count" int
    |> DecodeP.required "retweet_count" int
    |> Decode.map Social


shareDecoder : Decoder ArticleExtension
shareDecoder =
  Decode.oneOf
    [ Decode.at ["retweeted_status", "id_str"] Decode.string
    , Decode.at ["quoted_status", "id_str"] Decode.string
    ]
    |> Decode.map Share


textDecoder : Decoder ArticleExtension
textDecoder =
  Decode.andThen
    (\maybeShare ->
      case maybeShare of
        Just _ -> Decode.fail ""
        Nothing ->
          (Decode.succeed identity
            |> DecodeP.custom (
              Decode.oneOf
                [ field "text" string
                , field "full_text" string
                ]
            )
            |> Decode.map Text
          )
    )
    (Decode.maybe (Decode.at ["retweeted_status", "id_str"] Decode.string))


payloadErrorsDecoder : Decoder (List (String, Int))
payloadErrorsDecoder =
  Decode.list 
    <| Decode.map2 Tuple.pair
      (field "message" string)
      (field "code" int)


payloadResponseDecoder : Decoder Payload
payloadResponseDecoder =
  Decode.andThen (\maybeErrors ->
        case maybeErrors of
          Just errors ->
            Decode.succeed (Err errors)
          _ ->
            Decode.map Ok payloadDecoder)
        (maybe (field "errors" payloadErrorsDecoder))