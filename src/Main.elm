module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Dict exposing (..)
import Array exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline as DecodeP exposing (..)


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
  , articles: Dict String Article
  }


type alias Endpoint =
  { name: String
  , url: String
  }


type alias Timeline =
  { title: String
  , serviceName: String
  , endpointName: String
  , articleIds: List String
  }

type ArticleExtension
  = Social SocialData


type alias SocialData =
  { authorName: String
  , authorHandle: String
  , authorAvatar: String
  --, images: List PostImageData
  , liked: Bool
  , reposted: Bool
  , likeCount: Int
  , repostCount: Int
  }

type alias Article =
  { id: String
  , creationDate: String
  , text: String
  , extension : ArticleExtension
  }

type alias Model =
  { services: Dict String Service
  , timelines: List Timeline
  }


init : () -> ( Model, Cmd Msg )
init _ =
  ( { services =  Dict.fromList
        [ initTwitter ]
    , timelines =
      [ initTimeline "Twitter" "Home Timeline" "Home"
      , initTimeline "Twitter" "List" "Art"
      , initTimeline "Twitter" "Search" "1draw"
      , initTimeline "Twitter" "User Timeline" "User"
      ]
    }
  , Cmd.none
  )


initTimeline : String -> String -> String -> Timeline
initTimeline serviceName endpointName title =
  { title = title
  , serviceName = serviceName
  , endpointName = endpointName
  , articleIds = []
  }


initTwitter : (String, Service)
initTwitter =
  ( "Twitter"
  , { name = "Twitter"
    , endpoints = Dict.fromList
        [ ( "Home Timeline", { name = "Home Timeline", url = "http://127.0.0.1:5000/home_timeline" } )
        , ( "User Timeline", { name = "User Timeline", url = "http://127.0.0.1:5000/user_timeline" } )
        , ( "Search", { name = "Search", url = "http://127.0.0.1:5000/search" } )
        , ( "List", { name = "List", url = "http://127.0.0.1:5000/list" } )
        ]
    , articles = Dict.empty
    }
  )


-- UPDATE

type alias Payload =
  Result (List (String, Int)) (List Article)

type Msg
  = GotPayload Service Timeline (Result Http.Error Payload)
  | Refresh Service Endpoint Timeline


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GotPayload service timeline result ->
      case result of
        Ok payloadResult ->
          case payloadResult of
            Ok articles ->
              updateArticles service timeline articles model

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


updateArticles : Service -> Timeline -> List Article -> Model -> ( Model, Cmd Msg )
updateArticles service timeline articles model =
  ( { model | services = Dict.insert service.name (updateServiceArticles articles service) model.services
            , timelines = updateTimelineArticles (List.map .id articles) timeline.title model.timelines
    }
  , Cmd.none
  )


updateServiceArticles : List Article -> Service -> Service
updateServiceArticles articles service =
  { service | articles = Dict.union (listToDict articles) service.articles }


listToDict : List Article -> Dict String Article
listToDict articles =
  Dict.fromList
    <| List.map (\article -> (article.id, article)) articles


updateTimelineArticles : List String -> String -> List Timeline -> List Timeline
updateTimelineArticles articleIds timelineTitle timelines =
  List.map (\timeline -> 
    if timeline.title == timelineTitle then
      { timeline | articleIds = timeline.articleIds ++ articleIds }
    else
      timeline)
    timelines

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
            , viewContainer (getValues service.articles timeline.articleIds)
            ]

getValues : Dict comparable v -> List comparable -> List v
getValues dict keys =
  List.filterMap (\id -> Dict.get id dict) keys

viewContainer : List Article -> Html Msg
viewContainer articles =
  div [ class "timelineArticles" ] (List.map viewTweet articles)

viewTweetHeader : Article -> Html Msg
viewTweetHeader article =
  case article.extension of
    Social social ->
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
            [ small [] [ text article.creationDate ] ]
        ]

viewTweetButtons : Article -> Html Msg
viewTweetButtons article =
  case article.extension of
    Social social ->
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

viewTweetSkeleton : Maybe (Html Msg) -> Maybe (Html Msg) -> Maybe (Html Msg) -> Article -> Html Msg
viewTweetSkeleton superHeader extra footer article =
  case article.extension of
    Social social ->
      Html.article [ class "article" ]
        ((viewMaybe superHeader)
        ++ [ div [ class "media" ]
          [ figure [ class "media-left" ]
            [ p [ class "image", class "is-64x64" ]
              [ img [ alt (social.authorHandle ++ "'s avatar"), src social.authorAvatar ] [] ]
            ]
          , div [ class "media-content" ]
              ( [ div [ class "content" ]
                  [ viewTweetHeader article
                  , div [ class "tweet-paragraph" ] [ text article.text ]
                  ]
              ]
              ++ (viewMaybe extra)
              ++ [ viewTweetButtons article ]
              )
          ]
        ]
        ++ (viewMaybe footer)
        )


viewTweet : Article -> Html Msg
viewTweet article =
  viewTweetSkeleton Nothing Nothing Nothing article


-- HTTP


getEndpoint : Service -> Endpoint -> Timeline -> Cmd Msg
getEndpoint service endpoint timeline =
  Http.get
    { url = endpoint.url
    , expect = Http.expectJson (GotPayload service timeline) payloadResponseDecoder
    }

payloadDecoder : Decoder (List Article)
payloadDecoder =
  Decode.list tweetDecoder

tweetDecoder : Decoder Article
tweetDecoder =
  Decode.succeed Article
    |> DecodeP.required "id_str" string
    |> DecodeP.required "created_at" string -- TODO Parse
    |> DecodeP.custom (oneOf
      [ field "text" string
      , field "full_text" string
      ])
    |> DecodeP.custom socialDecoder

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


payloadErrorsDecoder : Decoder (List (String, Int))
payloadErrorsDecoder =
  Decode.list 
    <| map2 Tuple.pair
      (field "message" string)
      (field "code" int)


payloadResponseDecoder : Decoder Payload
payloadResponseDecoder =
  andThen (\maybeErrors ->
       case maybeErrors of
          Just errors ->
            Decode.succeed (Err errors)
          _ ->
            Decode.map Ok payloadDecoder) (maybe (field "errors" payloadErrorsDecoder))