module Tweet exposing (Tweet, payloadResponseDecoder, viewKeyedTweet, QuoteData)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (..)
import Html.Lazy exposing (..)
import Time
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, field, string, bool, int, maybe)
import Json.Decode.Pipeline as DecodeP
import Json.Decode.Extra as DecodeE
import Maybe.Extra as MaybeE

import Article exposing (Article, Id, SocialData, MediaContent(..), ImageData, VideoData)
import Service exposing (Payload(..), RateLimitInfo)
import Timeline exposing (isCompact, CompactMode(..))
import TimeParser
import Extra exposing (..)


type Tweet
  = Tweet (Article TweetData)
  | Retweet (Article RetweetData)
  | Quote (Article QuoteData)


type alias TweetData =
  { text: String
  , social: SocialData
  , media: Maybe MediaContent
  }


newTweet : Id -> Time.Posix -> String -> SocialData -> Maybe MediaContent -> Tweet
newTweet id creationDate text social media =
  Tweet
    { id = id
    , creationDate = creationDate
    , text = text
    , social = social
    , media = media
    }


type alias RetweetData =
  { social: SocialData
  , media: Maybe MediaContent
  , share: Id
  }


newRetweet : Id -> Time.Posix -> SocialData -> Maybe MediaContent -> Id -> Tweet
newRetweet id creationDate social media share =
  Retweet
    { id = id
    , creationDate = creationDate
    , social = social
    , media = media
    , share = share
    }


type alias QuoteData =
  { text: String
  , social: SocialData
  , media: Maybe MediaContent
  , share: Id
  , permalink: String
  }


newQuote : Id -> Time.Posix -> String -> SocialData -> Maybe MediaContent -> Id -> String -> Tweet
newQuote id creationDate text social media share permalink =
  Quote
    { id = id
    , creationDate = creationDate
    , text = text
    , social = social
    , media = media
    , share = share
    , permalink = permalink
    }

type ViewArticle
  = ViewTweet (Article TweetData)
  | ViewRetweet (Article RetweetData) (Article TweetData)
  | ViewQuote (Article QuoteData) (Article TweetData)


type alias TweetSkeletonParts msg =
    { superHeader: Maybe (Html msg)
    , extra: Maybe (Html msg)
    , footer: Maybe (Html msg)
    }


type alias TimeModel =
  { zone : Time.Zone
  , lastNow : Time.Posix
  }


{-type alias Like service msg =
  (service -> ViewArticle -> msg)


type alias Repost service msg =
  (service -> ViewArticle -> msg)-}


-- VIEW


viewIcon : String -> String -> String -> Html msg
viewIcon icon iconType size =
  span [ class "icon" ]
    [ i [ class iconType, class icon, class size ] [] ]


viewKeyedTweet : TimeModel -> service -> Bool -> ViewArticle -> (String, Html msg)
viewKeyedTweet timeModel service compact article =
  (getViewId article, lazy3 (viewTweet service) timeModel compact article)


viewTweetHeader : TimeModel -> ViewArticle -> SocialData -> Html msg
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
          [ title (TimeParser.toFullTimeFormat timeModel (getViewCreationDate article)) ]
          [ text (TimeParser.relativeTimeFormat timeModel (getViewCreationDate article)) ]
        ]
    ]


viewTweetButtons : service -> ViewArticle -> SocialData -> Html msg
viewTweetButtons service article social =
  nav [ class "level", class "is-mobile" ]
    [ div [ class "level-left" ]
        [ a [ class "level-item"
            , class "articleButton"
            , class "repostButton"
            , classList [("repostedPostButton", social.reposted)]
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
            ]
            [ viewIcon "fa-ellipsis-h" "fas" "" ]
        ]
    ]


viewTweetSkeleton : TimeModel -> TweetSkeletonParts msg -> service -> ViewArticle -> Html msg
viewTweetSkeleton timeModel parts service article =
  let
    id = getViewId article
    maybeArticle =
      case article of
        ViewTweet tweet -> Just (tweet.text, tweet.social)
        ViewQuote quote _ -> Just (quote.text, quote.social)
        _ -> Nothing
  in


  case maybeArticle of
    Just (textStr, social) ->
      Html.article [ class "article", attribute "articleId" id ]
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
                    [viewTweetButtons service article social]
                  )
                  )
              ]
            ]
          )
          parts.footer
        )

    _ ->
      Html.article [ class "article" ]
        [ text ("Couldn't find text and social extension for " ++ (getViewId article)) ]


viewTweet : service -> TimeModel -> Bool -> ViewArticle -> Html msg
viewTweet service timeModel compact article =
  let
    parts =
      { superHeader = getTweetSuperHeader article
      , extra = getQuoteExtra timeModel article
      , footer = getTweetFooter compact article
      }
  in
    viewTweetSkeleton timeModel parts service article


getTweetSuperHeader : ViewArticle -> Maybe (Html msg)
getTweetSuperHeader article =
  case article of
    ViewRetweet retweet tweet ->
      Just (
        div [ class "repostLabel" ]
          [ a [ href ("https://twitter.com/" ++ retweet.social.authorHandle)
              , target "_blank"
              , rel "noopener noreferrer"
              ]
              [ text (retweet.social.authorName ++ " retweeted") ]
          ]
      )
    
    _ ->
      Nothing


getQuoteExtra : TimeModel -> ViewArticle -> Maybe (Html msg)
getQuoteExtra timeModel article =
  case article of
    ViewQuote quote _ ->
      Just
        ( div [ class "quotedPost" ]
            [ viewTweetHeader timeModel article quote.social
            , div [ class "tweet-paragraph" ] [ text quote.text ]
            -- media
            ]
        )

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
        [ div [ class "mediaHolder"
              , class (imageFormatClass imageData)
              , classList [ ("mediaHolderCompact", compact) ]
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


getMedia : ViewArticle -> Maybe MediaContent
getMedia article =
  case article of
    ViewTweet tweet ->
      tweet.media

    ViewRetweet retweet tweet ->
      tweet.media

    ViewQuote quote tweet ->
      tweet.media


getViewCreationDate : ViewArticle -> Time.Posix
getViewCreationDate article =
  case article of
    ViewTweet tweet ->
      tweet.creationDate

    ViewRetweet retweet _ ->
      retweet.creationDate

    ViewQuote quote _ ->
      quote.creationDate


getViewId : ViewArticle -> String
getViewId article =
  case article of
    ViewTweet tweet ->
      tweet.id

    ViewRetweet retweet _ ->
      retweet.id

    ViewQuote quote _ ->
      quote.id


getCreationDate : Tweet -> Time.Posix
getCreationDate article =
  case article of
    Tweet tweet ->
      tweet.creationDate

    Retweet retweet ->
      retweet.creationDate

    Quote quote ->
      quote.creationDate


getId : Tweet -> String
getId article =
  case article of
    Tweet tweet ->
      tweet.id

    Retweet retweet ->
      retweet.id

    Quote quote ->
      quote.id


listToDict : List Tweet -> (Article.Collection Tweet)
listToDict articles =
  Dict.fromList
    <| List.map (\article -> (getId article, article)) articles


getTweetFooter : Bool -> ViewArticle -> Maybe (Html msg)
getTweetFooter compact article =
  Maybe.andThen
    (\media -> Just (lazy2 getMediaFooter compact media))
    (getMedia article)


-- DECODE


unpackDecodedTweets : Maybe RateLimitInfo -> List (Tweet, Maybe Tweet) -> Payload Tweet
unpackDecodedTweets maybeRateLimit decodedTweets =
  List.map unpackDecodedTweet decodedTweets
  |> List.unzip
  |> Tuple.mapFirst List.concat
  |> (\(tweets, ids) ->
      case maybeRateLimit of
        Just rateLimit ->
          RateLimitedPayload
            tweets
            ids
            rateLimit

        Nothing ->
          FreePayload
            tweets
            ids
  )


unpackDecodedTweet : (Tweet, Maybe Tweet) -> (List Tweet, String)
unpackDecodedTweet decodedTweet =
  ( case (Tuple.second decodedTweet) of
      Just sharedTweet ->
        [Tuple.first decodedTweet, sharedTweet]
      Nothing ->
        [Tuple.first decodedTweet]
  , getId (Tuple.first decodedTweet)
  )


payloadDecoder : Decoder (Payload Tweet)
payloadDecoder =
  Decode.map2 unpackDecodedTweets
    (Decode.maybe rateLimitDecoder)
    ( field "statuses" (Decode.list tweetDecoder) )



rateLimitDecoder : Decoder RateLimitInfo
rateLimitDecoder =
  Decode.map3 RateLimitInfo
    (Decode.at ["_headers", "x-rate-limit-remaining"] DecodeE.parseInt)
    (Decode.at ["_headers", "x-rate-limit-limit"] DecodeE.parseInt)
    (Decode.at ["_headers", "x-rate-limit-reset"] DecodeE.parseInt)


{-exampleTweetArticle : Article TweetExt
exampleTweetArticle =
  { id = "1010"
  , creationDate = Time.millisToPosix 0
  , text = "Boop"
  , social = 
      { authorName = "Blep"
      , authorHandle = "@blerp"
      , authorAvatar = ""
      , liked = False
      , reposted = False
      , likeCount = 0
      , repostCount = 0
      }
  , media = Nothing
  }-}

tweetDecoder : Decoder (Tweet, Maybe Tweet)
tweetDecoder =
  Decode.map2 Tuple.pair
    plainTweetDecoder
    (Decode.succeed Nothing)
  {-Decode.oneOf
    [ Decode.andThen
        (\quoted -> Decode.map2 Tuple.pair quoteDecoder quoted)
        (field "quoted_status" plainTweetDecoder)
    , Decode.andThen
        (\retweeted -> Decode.map2 Tuple.pair retweetDecoder retweeted)
        (field "retweeted_status" plainTweetDecoder)
    , (Decode.map Just plainTweetDecoder, Decode.succeed Nothing)
    ]-}
  {-Decode.andThen (\articles ->
      case articles of
        (Quote quote, Tweet tweet) ->
          ( Decode.succeed
              ( { quote | text = fixTweetText maybeQuoteUrl quote.text }
              , sharedArticle = Maybe.map (fixTweetText maybeQuoteUrl) article.sharedArticle
            }
          )

        _ ->
          Decode.succeed { article
            | article = fixTweetText Nothing article.article
            , sharedArticle = Maybe.map (fixTweetText Nothing) article.sharedArticle
          }
    )-}


fixTweetText : Maybe String -> Tweet -> Tweet
fixTweetText maybeQuoteUrl article =
  case article of
    Tweet tweet ->
      Tweet
        {tweet | text =
          (case tweet.media of
            Just media -> fixTweetTextMedia media tweet.text
            Nothing -> tweet.text
          )
            |> (\newTextStr -> case maybeQuoteUrl of
                  Just quoteUrl -> 
                    String.replace quoteUrl "" newTextStr
                      |> String.trimRight
                  Nothing -> newTextStr
              )
        }

    Quote quote ->
      Quote
        {quote | text =
          (case quote.media of
            Just media -> fixTweetTextMedia media quote.text
            Nothing -> quote.text
          )
            |> (\newTextStr -> case maybeQuoteUrl of
                  Just quoteUrl -> 
                    String.replace quoteUrl "" newTextStr
                      |> String.trimRight
                  Nothing -> newTextStr
              )
        }

    _ -> article


fixTweetTextMedia : MediaContent -> String -> String
fixTweetTextMedia media textStr =
  case media of
    Images imageDatas ->
      List.foldl (\imageData foldedText ->
        String.replace imageData.compressedUrl "" foldedText
      ) textStr imageDatas
      |> String.trimRight

    Image imageData ->
      String.replace imageData.compressedUrl "" textStr
        |> String.trimRight

    Video videoData ->
      String.replace videoData.compressedUrl "" textStr
      |> String.trimRight


plainTweetDecoder : Decoder Tweet
plainTweetDecoder =
  Decode.map5 newTweet
    (field "id_str" string)
    (Decode.andThen
      TimeParser.tweetTimeDecoder
      (field "created_at" string)
    )
    textDecoder
    socialDecoder
    (Decode.maybe mediaDecoder)


retweetDecoder : Decoder Tweet
retweetDecoder =
  Decode.map5 newRetweet
    (field "id_str" string)
    (Decode.andThen
      TimeParser.tweetTimeDecoder
      (field "created_at" string)
    )
    socialDecoder
    (Decode.maybe mediaDecoder)
    (Decode.at ["retweeted_status", "id_str"] string)


quoteDecoder : Decoder Tweet
quoteDecoder =
  Decode.map7 newQuote
    (field "id_str" string)
    (Decode.andThen
      TimeParser.tweetTimeDecoder
      (field "created_at" string)
    )
    textDecoder
    socialDecoder
    (Decode.maybe mediaDecoder)
    (field "quoted_status_id_str" string)
    (field "quoted_status_permalink" string)


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


shareDecoder : Decoder Id
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

                _ -> Decode.fail ("MediaContent type '" ++ mediaType ++ "' not handled.")

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


payloadResponseDecoder : Decoder (Result (List (String, Int)) (Payload Tweet))
payloadResponseDecoder =
  Decode.andThen (\maybeErrors ->
        case maybeErrors of
          Just errors ->
            Decode.succeed (Err errors)
          _ ->
            Decode.map Ok payloadDecoder)
        (maybe (field "errors" payloadErrorsDecoder))