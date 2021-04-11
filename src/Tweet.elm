module Tweet exposing ({-payloadResponseDecoder, -}viewKeyedTweet)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Time
import Json.Decode as Decode exposing (Decoder, field, string, bool, int, maybe)
import Json.Decode.Pipeline as DecodeP
import Json.Decode.Extra as DecodeE
import Maybe.Extra as MaybeE

import Article exposing (Article, Id, MediaContent(..), SocialData, ImageData, VideoData)
import Service exposing (Payload(..), RateLimitInfo)
import Timeline exposing (isCompact, CompactMode(..))
import TimeParser
import Extra exposing (..)


type TweetExt
  = Tweet TweetData
  | Retweet RetweetData
  | Quote QuoteData


type ViewTweetExt
  = ViewTweet TweetData
  | ViewRetweet (RetweetData, Article ViewArticleExt, TweetData)
  | ViewQuote (QuoteData, Article ViewArticleExt, TweetData)


type alias ArticleExt =
  { ext: TweetExt }


type alias ViewArticleExt =
  { ext: ViewTweetExt }


type alias TweetData =
  { text: String
  , social: SocialData
  , media: Maybe MediaContent
  }


type alias RetweetData =
  { social: SocialData
  , retweeted: Id
  }


type alias QuoteData =
  { text: String
  , social: SocialData
  , quoted: Id
  }


type alias TweetSkeletonParts msg =
    { superHeader: Maybe (Html msg)
    , extra: Maybe (Html msg)
    , footer: Maybe (Html msg)
    }


type alias TimeModel =
  { zone : Time.Zone
  , lastNow : Time.Posix
  }


type alias Like service msg =
  (service -> (Article ArticleExt) -> msg)


type alias Repost service msg =
  (service -> (Article ArticleExt) -> msg)


-- VIEW


viewIcon : String -> String -> String -> Html msg
viewIcon icon iconType size =
  span [ class "icon" ]
    [ i [ class iconType, class icon, class size ] [] ]


viewKeyedTweet : TimeModel -> service -> Bool -> (Article ViewArticleExt) -> (String, Html msg)
viewKeyedTweet timeModel service compact vTweet =
  (getViewTweetId vTweet, lazy3 (viewTweet service) timeModel compact vTweet)


viewTweetHeader : TimeModel -> Time.Posix -> SocialData -> Html msg
viewTweetHeader timeModel creationDate social =
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
          [ title (TimeParser.toFullTimeFormat timeModel creationDate) ]
          [ text (TimeParser.relativeTimeFormat timeModel creationDate) ]
        ]
    ]


viewTweetButtons : service -> (Article ViewArticleExt) -> SocialData -> Html msg
viewTweetButtons service article social =
  nav [ class "level", class "is-mobile" ]
    [ div [ class "level-left" ]
        [ a [ class "level-item"
            , class "articleButton"
            , class "repostButton"
            , classList [("repostedPostButton", social.reposted)]
            --, onClick (repostMsg service article)
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
            --, onClick (likeMsg service article)
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

        , a [ class "level-item"
            , class "articleButton"
            , class "articleMenuButton"
            --, onClick (debugMsg article)
            ]
            [ viewIcon "fa-ellipsis-h" "fas" "" ]
        ]
    ]


viewTweetSkeleton : TimeModel -> TweetSkeletonParts msg -> service -> (Article ViewArticleExt) -> String -> SocialData -> Html msg
viewTweetSkeleton timeModel parts service article content social =
  Html.article [ class "article", attribute "articleId" article.id ]
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
                  [ (lazy3 viewTweetHeader timeModel article.creationDate social)
                  , div [ class "tweet-paragraph" ] [ text content ]
                  ]
              ]
              ++
              (MaybeE.cons
                parts.extra
                [viewTweetButtons service article social]
              )
              )
          ]
        ]
      )
      parts.footer
    )


--getActualTweet : (Article ViewArticleExt) -> (Article ArticleExt)
--getActualTweet vTweet =
--  case vTweet.ext of
--    ViewTweet tweet ->
      
--    ViewRetweet (retweet, art, tweet) ->
--      shared
--    ViewQuote (quote, art, tweet) ->
--      vTweet.tweet


viewTweet : service -> TimeModel -> Bool -> (Article ViewArticleExt) -> Html msg
viewTweet service timeModel compact vTweet =
  let
    (article, content, social) =
      case vTweet.ext of
        ViewTweet tweet ->
          (vTweet, tweet.text, tweet.social)

        ViewRetweet (retweet, art, tweet) ->
          (art, tweet.text, tweet.social)

        ViewQuote (quote, art, tweet) ->
          (vTweet, quote.text, quote.social)

    parts =
      { superHeader = getTweetSuperHeader vTweet
      , extra = getTweetExtra timeModel vTweet
      , footer = getTweetFooter compact vTweet
      }
  in
    viewTweetSkeleton timeModel parts service article content social


getRetweetSuperHeader : SocialData -> Html msg
getRetweetSuperHeader social =
  div [ class "repostLabel" ]
    [ a [ href ("https://twitter.com/" ++ social.authorHandle)
        , target "_blank"
        , rel "noopener noreferrer"
        ]
        [ text (social.authorName ++ " retweeted") ]
    ]


getTweetSuperHeader : (Article ViewArticleExt) -> Maybe (Html msg)
getTweetSuperHeader vTweet =
  case vTweet.ext of
    ViewRetweet (retweet, _, _) ->
      Just (getRetweetSuperHeader retweet.social)
      
    _ -> Nothing


getQuoteExtra : TimeModel -> Time.Posix -> TweetData -> Html msg
getQuoteExtra timeModel creationDate tweet =
  div [ class "quotedPost" ]
    [ viewTweetHeader timeModel creationDate tweet.social
    , div [ class "tweet-paragraph" ] [ text tweet.text ]
    -- media
    ]


getTweetExtra : TimeModel -> (Article ViewArticleExt) -> Maybe (Html msg)
getTweetExtra timeModel vTweet =
  case vTweet.ext of
    ViewQuote (quote, art, tweet) ->
      Just (lazy2 (getQuoteExtra timeModel) art.creationDate tweet)

    _ -> Nothing


getMediaFooter : Bool -> MediaContent -> Html msg
getMediaFooter compact media =
  case media of
    Images imageDatas ->
      div [ class "postImages", class "postMedia", classList [("postImagesCompact", compact)] ]
        (List.indexedMap (\index imageData ->
          div [ class "mediaHolder"
              , class (imageFormatClass imageData)
              , classList
                  [ ("mediaHolderCompact", compact)
                  , ("thirdImage", (List.length imageDatas) == 3 && index == 2)
                  ]
              ]
            [ div [ class "is-hidden", class "imgPlaceholder" ] []
            , img [ src imageData.url ] []
            ]
        ) imageDatas)

    Image imageData ->
      div [ class "postImages", class "postMedia", classList [("postImagesCompact", compact)] ]
        [
          div [ class "mediaHolder"
              , class (imageFormatClass imageData)
              , classList
                  [ ("mediaHolderCompact", compact) ]
              ]
            [ div [ class "is-hidden", class "imgPlaceholder" ] []
            , img [ src imageData.url ] []
            ]
        ]

    Video videoData ->
      div [ class "postVideo", class "postMedia" ]
      [ div [ class "mediaHolder" ]
          [ div [ class "is-hidden", class "imgPlaceholder" ] []
          , video [ controls True, autoplay videoData.autoplay, loop videoData.autoplay ]
              [ source [ src videoData.url ] [] ]
          ]
      ]


imageFormatClass : ImageData -> String
imageFormatClass imageData =
  case imageData.size of
    Just size ->
      if size.width > size.height then
        "landscape"
      else
        "portrait"

    Nothing -> "portrait"


getTweetFooter : Bool -> (Article ViewArticleExt) -> Maybe (Html msg)
getTweetFooter compact vTweet =
  Maybe.andThen
    (\media -> Just (lazy2 getMediaFooter compact media))
    ( case vTweet.ext of
        ViewTweet tweet ->
          tweet.media

        _ -> Nothing
    )


getViewTweetId : (Article ViewArticleExt) -> Id
getViewTweetId vTweet =
  case vTweet.ext of
    ViewTweet _ ->
      vTweet.id

    ViewRetweet (retweet, art, tweet) ->
      vTweet.id ++ art.id

    ViewQuote (quote, art, tweet) ->
      vTweet.id ++ art.id


-- DECODE


{-unpackDecodedTweets : Maybe RateLimitInfo -> List (Article ViewArticleExt) -> Payload ArticleExt
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


unpackDecodedTweet : (Article ViewArticleExt) -> (List (Article ArticleExt), String)
unpackDecodedTweet decodedTweet =
  ( case decodedTweet.shared of
          Just shared ->
            [decodedTweet.tweet, shared]
          Nothing ->
            [decodedTweet.tweet]
  , decodedTweet.tweet.id
  )


payloadDecoder : Decoder (Payload ArticleExt)
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


tweetDecoder : Decoder (Article ViewArticleExt)
tweetDecoder =
  Decode.andThen (\vTweet ->
      case (getTweetType vTweet) of
        Quote ->
          Decode.andThen (\maybeQuoteUrl ->
            Decode.succeed { vTweet
              | article = fixTweetText maybeQuoteUrl vTweet.tweet
              , shared = Maybe.map (fixTweetText maybeQuoteUrl) vTweet.shared
            }
          )
          (Decode.maybe (Decode.at ["quoted_status_permalink", "url"] Decode.string))

        _ ->
          Decode.succeed { vTweet
            | article = fixTweetText Nothing vTweet.tweet
            , shared = Maybe.map (fixTweetText Nothing) vTweet.shared
          }
    )
    (Decode.map2 (Article ViewArticleExt)
      topTweetDecoder
      (Decode.maybe
        (Decode.oneOf
          [ field "quoted_status" topTweetDecoder
          , field "retweeted_status" topTweetDecoder
          ]
        )))


fixTweetText : Maybe String -> (Article ArticleExt) -> (Article ArticleExt)
fixTweetText maybeQuoteUrl article =
  case article.text of
    Just textStr ->
      {article | text =
        Just ((case article.media of
          Just media -> fixTweetTextMedia media textStr
          Nothing -> textStr
        )
          |> (\newTextStr -> case maybeQuoteUrl of
                Just quoteUrl -> 
                  String.replace quoteUrl "" newTextStr
                    |> String.trimRight
                Nothing -> newTextStr
             ))
      }
    Nothing -> article


fixTweetTextMedia : MediaContent -> String -> String
fixTweetTextMedia media textStr =
  case media of
    Images imageDatas ->
      List.foldl (\imageData foldedText ->
        String.replace imageData.compressedUrl "" foldedText
      ) textStr imageDatas
      |> String.trimRight

    Video videoData ->
      String.replace videoData.compressedUrl "" textStr
      |> String.trimRight

topTweetDecoder : Decoder (Article ArticleExt)
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
    |> DecodeP.custom (Decode.maybe mediaDecoder)


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


mediaDecoder : Decoder MediaContent
mediaDecoder =
  let
    decoder =
      Decode.andThen
        (\maybeMediaType ->
          case maybeMediaType of
            Just mediaType ->
              case mediaType of
                "photo" -> imageDecoder

                "video" -> videoDecoder False

                "animated_gif" -> videoDecoder True

                _ -> Decode.fail ("Media type '" ++ mediaType ++ "' not handled.")

            Nothing -> Decode.fail "No media found."
        )
      (Decode.map List.head (Decode.list (field "type" Decode.string)))
  in
    Decode.oneOf
      [ Decode.at ["extended_entities", "media"] decoder
      , Decode.at ["entities", "media"] decoder
      ]


imageDecoder : Decoder MediaContent
imageDecoder =
  Decode.map Images
    (Decode.list
      (Decode.map3 ImageData
        (field "media_url_https" Decode.string)
        (field "url" Decode.string)
        (field "sizes" (Decode.maybe (Decode.oneOf
          [ (field "large" sizeDecoder)
          , (field "medium" sizeDecoder)
          , (field "small" sizeDecoder)
          , (field "thumb" sizeDecoder)
          ]
        )))
      )
    )


sizeDecoder : Decoder { width: Int, height: Int }
sizeDecoder =
  Decode.map2 (\w h -> { width = w, height = h })
    (field "w" Decode.int)
    (field "h" Decode.int)

videoDecoder : Bool -> Decoder MediaContent
videoDecoder autoplay =
  Decode.andThen
    (\maybeVideo -> DecodeE.fromMaybe "Didn't find a video"  maybeVideo)
  (Decode.map List.head
    (Decode.list
      (Decode.map Video
        (Decode.map3 VideoData
          (Decode.at ["video_info", "variants"]
            (
              Decode.andThen
                (\maybeVariant ->
                  (DecodeE.fromMaybe "Didn't find a video_info variant" maybeVariant)
                )
                (Decode.map List.head (Decode.list (field "url" Decode.string)))
            )
          )
          (field "url" Decode.string)
          (Decode.succeed autoplay)
    ))))

payloadErrorsDecoder : Decoder (List (String, Int))
payloadErrorsDecoder =
  Decode.list 
    <| Decode.map2 Tuple.pair
      (field "message" string)
      (field "code" int)


payloadResponseDecoder : Decoder (Result (List (String, Int)) (Payload ArticleExt))
payloadResponseDecoder =
  Decode.andThen (\maybeErrors ->
        case maybeErrors of
          Just errors ->
            Decode.succeed (Err errors)
          _ ->
            Decode.map Ok payloadDecoder)
        (maybe (field "errors" payloadErrorsDecoder))-}