module Tweet exposing (payloadResponseDecoder, viewKeyedTweet)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Time
import Json.Decode as Decode exposing (Decoder, field, string, bool, int, maybe)
import Json.Decode.Pipeline as DecodeP
import Json.Decode.Extra as DecodeE
import Maybe.Extra as MaybeE

import Article exposing (Article, SocialData, ImageData, ShareableArticle)
import Service exposing (Payload(..), RateLimitInfo)
import TimeParser


type alias TweetSkeletonParts msg =
    { superHeader: Maybe (Html msg)
    , extra: Maybe (Html msg)
    , footer: Maybe (Html msg)
    }


type TweetType
  = Tweet
  | Retweet
  | Quote


type alias TimeModel =
  { zone : Time.Zone
  , lastNow : Time.Posix
  }


type alias Like service msg =
  (service -> Article -> msg)


type alias Repost service msg =
  (service -> Article -> msg)


-- VIEW


viewIcon : String -> String -> String -> Html msg
viewIcon icon iconType size =
  span [ class "icon" ]
    [ i [ class iconType, class icon, class size ] [] ]


viewKeyedTweet : Like service msg -> Repost service msg -> TimeModel -> service -> ShareableArticle -> (String, Html msg)
viewKeyedTweet likeMsg repostMsg timeModel service shareableArticle =
  (Article.getShareableId shareableArticle, lazy2 (viewTweet likeMsg repostMsg service) timeModel shareableArticle)


viewTweetHeader : TimeModel -> Article -> SocialData -> Html msg
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


viewTweetButtons : Like service msg -> Repost service msg -> service -> Article -> SocialData -> Html msg
viewTweetButtons likeMsg repostMsg service article social =
  nav [ class "level", class "is-mobile" ]
    [ div [ class "level-left" ]
        [ a [ class "level-item"
            , class "articleButton"
            , class "repostButton"
            , classList [("repostedPostButton", social.reposted)]
            , onClick (repostMsg service article)
            ]
            (maybeJoinR
              (viewIcon "fa-retweet" "fas" "")
              (
                if social.repostCount > 0 then
                  Just (span [] [ text (String.fromInt social.repostCount) ])
                else
                  Nothing
              )
            )

        , a [ class "level-item"
            , class "articleButton"
            , class "likeButton"
            , classList [("likedPostButton", social.liked)]
            , onClick (likeMsg service article)
            ]
            (consr
              [ viewIcon "fa-heart" (if social.liked then "fas" else "far") "" ]
              (
                if social.likeCount > 0 then
                  Just (span [ ] [ text (String.fromInt social.likeCount) ])
                else
                  Nothing
              )
            )

        , a [ class "level-item", class "articleButton", class "articleMenuButton" ]
            [ viewIcon "fa-ellipsis-h" "fas" "" ]
        ]
    ]


consr : List a -> Maybe a -> List a
consr list item =
    case item of
        Just v ->
             list ++ [v]

        Nothing ->
            list


maybeJoin : Maybe a -> a -> List a
maybeJoin item el =
    case item of
        Just v ->
            [v, el]

        Nothing ->
          [el]


maybeJoinR : a -> Maybe a -> List a
maybeJoinR el item =
    case item of
        Just v ->
             [el, v]

        Nothing ->
          [el]


viewTweetSkeleton : Like service msg -> Repost service msg -> TimeModel -> TweetSkeletonParts msg -> service -> Article -> Html msg
viewTweetSkeleton likeMsg repostMsg timeModel parts service article =
  case ((article.text, article.social)) of
    (Just textStr, Just social) ->
      Html.article [ class "article" ]
        (consr
          (MaybeE.cons
            parts.superHeader
            [ div [ class "media" ]
              [ figure [ class "media-left" ]
                [ p [ class "image", class "is-64x64" ]
                  [ img [ alt (social.authorHandle ++ "'s avatar"), src social.authorAvatar ] [] ]
                ]
              , div [ class "media-content" ]
                  ( [ div [ class "content" ]
                      [ (lazy3 viewTweetHeader timeModel article social)
                      , div [ class "tweet-paragraph" ] [ text textStr ]
                      ]
                  ]
                  ++
                  (MaybeE.cons
                    parts.extra
                    [viewTweetButtons likeMsg repostMsg service article social]
                  )
                  )
              ]
            ]
          )
          parts.footer
        )

    _ ->
      Html.article [ class "article" ]
        [ text ("Couldn't find text and social extension for " ++ article.id) ]


getTweetType : ShareableArticle -> TweetType
getTweetType shareableArticle =
  case shareableArticle.sharedArticle of
    Just _ ->
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


viewTweet : Like service msg -> Repost service msg -> service -> TimeModel -> ShareableArticle -> Html msg
viewTweet likeMsg repostMsg service timeModel shareableArticle =
  let
    actualTweet = getActualTweet shareableArticle
    parts =
      { superHeader = getTweetSuperHeader shareableArticle
      , extra = getTweetExtra timeModel shareableArticle
      , footer = getTweetFooter shareableArticle
      }
  in
    viewTweetSkeleton likeMsg repostMsg timeModel parts service actualTweet


getRetweetSuperHeader : Article -> Html msg
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


getTweetSuperHeader : ShareableArticle -> Maybe (Html msg)
getTweetSuperHeader shareableArticle =
  case shareableArticle.sharedArticle of
    Just _ ->
      case shareableArticle.article.text of
        Just _ ->
          Nothing
        Nothing ->
          Just (getRetweetSuperHeader shareableArticle.article)
    Nothing -> Nothing


getQuoteExtra : TimeModel -> Article -> Html msg
getQuoteExtra timeModel article =
  case ((article.social, article.text)) of
    (Just social, Just quoteText) ->
      div [ class "quotedPost" ]
        [ viewTweetHeader timeModel article social
        , div [ class "tweet-paragraph" ] [ text quoteText ]
        -- media
        ]

    _ -> div [] []


getTweetExtra : TimeModel -> ShareableArticle -> Maybe (Html msg)
getTweetExtra timeModel shareableArticle =
  case shareableArticle.sharedArticle of
    Just shared ->
      case shareableArticle.article.text of
        Just _ ->
          Just (getQuoteExtra timeModel shared)
        Nothing ->
          Nothing
    Nothing -> Nothing


getMediaFooter : List ImageData -> Html msg
getMediaFooter images =
  div [ class "postImages", class "postMedia" ]
    (List.map (\imageData ->
      div [ class "mediaHolder" ]
        [ div [ class "is-hidden", class "imgPlaceholder" ] []
        , img [ src imageData.url ] []
        ]
    ) images)


getTweetFooter : ShareableArticle -> Maybe (Html msg)
getTweetFooter shareableArticle =
  case (getActualTweet shareableArticle).images of
    Just images ->
      Just (getMediaFooter images)

    Nothing -> Nothing


-- DECODE


unpackDecodedTweets : Maybe RateLimitInfo -> List ShareableArticle -> Payload
unpackDecodedTweets maybeRateLimit decodedTweets =
  List.map unpackDecodedTweet decodedTweets
  |> List.unzip
  |> Tuple.mapFirst List.concat
  |> (\tuple ->
      case maybeRateLimit of
        Just rateLimit ->
          RateLimitedPayload
            (Tuple.first tuple)
            (Tuple.second tuple)
            rateLimit

        Nothing ->
          FreePayload
            (Tuple.first tuple)
            (Tuple.second tuple)
  )


unpackDecodedTweet : ShareableArticle -> (List Article, String)
unpackDecodedTweet decodedTweet =
  ( case decodedTweet.sharedArticle of
          Just sharedArticle ->
            [decodedTweet.article, sharedArticle]
          Nothing ->
            [decodedTweet.article]
  , decodedTweet.article.id
  )


payloadDecoder : Decoder Payload
payloadDecoder =
  Decode.map2 unpackDecodedTweets
    (Decode.maybe rateLimitDecoder)
    (
      Decode.oneOf
        [ (Decode.list tweetDecoder)
        , (Decode.map List.singleton tweetDecoder)
        ]
        |> field "statuses"
    )



rateLimitDecoder : Decoder RateLimitInfo
rateLimitDecoder =
  Decode.map3 RateLimitInfo
    (Decode.at ["_headers", "x-rate-limit-remaining"] DecodeE.parseInt)
    (Decode.at ["_headers", "x-rate-limit-limit"] DecodeE.parseInt)
    (Decode.at ["_headers", "x-rate-limit-reset"] DecodeE.parseInt)


tweetDecoder : Decoder ShareableArticle
tweetDecoder =
  Decode.andThen (\shareableArticle ->
      Decode.succeed { shareableArticle
        | article = fixTweetText shareableArticle.article
        , sharedArticle = Maybe.map fixTweetText shareableArticle.sharedArticle
      }
    )
    (Decode.map2 ShareableArticle
      topTweetDecoder
      (Decode.maybe
        (Decode.oneOf
          [ field "quoted_status" topTweetDecoder
          , field "retweeted_status" topTweetDecoder
          ]
        )))


fixTweetText : Article -> Article
fixTweetText article =
  case (article.text, article.images) of
    (Just textStr, Just images) ->
      {article | text =
        List.foldl (\image foldedText ->
          String.replace image.compressedUrl "" foldedText
        ) textStr images
        |> String.trimRight
        |> Just
      }

    _ -> article


topTweetDecoder : Decoder Article
topTweetDecoder =
  Decode.succeed Article
    |> DecodeP.required "id_str" string
    |> DecodeP.custom (Decode.andThen
      TimeParser.tweetTimeDecoder
      (field "created_at" string)
    )
    |> DecodeP.custom (Decode.maybe textDecoder)
    |> DecodeP.custom (Decode.maybe socialDecoder)
    |> DecodeP.custom (Decode.maybe shareDecoder)
    |> DecodeP.custom (Decode.maybe imagesDecoder)


socialDecoder : Decoder SocialData
socialDecoder =
  Decode.succeed SocialData
    |> DecodeP.requiredAt ["user", "name"] string
    |> DecodeP.requiredAt ["user", "screen_name"] string
    |> DecodeP.requiredAt ["user", "profile_image_url_https"] string
    |> DecodeP.required "favorited" bool
    |> DecodeP.required "retweeted" bool
    |> DecodeP.required "favorite_count" int
    |> DecodeP.required "retweet_count" int


shareDecoder : Decoder Article.Id
shareDecoder =
  Decode.oneOf
    [ Decode.at ["retweeted_status", "id_str"] Decode.string
    , Decode.at ["quoted_status", "id_str"] Decode.string
    ]


textDecoder : Decoder String
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
          )
    )
    (Decode.maybe (Decode.at ["retweeted_status", "id_str"] Decode.string))


imagesDecoder : Decoder (List ImageData)
imagesDecoder =
  Decode.oneOf
    [ Decode.at ["extended_entities", "media"] tweetMediaDecoder
    , Decode.at ["entities", "media"] tweetMediaDecoder
    ]


tweetMediaDecoder : Decoder (List ImageData)
tweetMediaDecoder =
  (Decode.list
    (Decode.succeed ImageData
      |> DecodeP.required "media_url_https" Decode.string
      |> DecodeP.required "url" Decode.string
    )
  )


payloadErrorsDecoder : Decoder (List (String, Int))
payloadErrorsDecoder =
  Decode.list 
    <| Decode.map2 Tuple.pair
      (field "message" string)
      (field "code" int)


payloadResponseDecoder : Decoder (Result (List (String, Int)) Payload)
payloadResponseDecoder =
  Decode.andThen (\maybeErrors ->
        case maybeErrors of
          Just errors ->
            Decode.succeed (Err errors)
          _ ->
            Decode.map Ok payloadDecoder)
        (maybe (field "errors" payloadErrorsDecoder))