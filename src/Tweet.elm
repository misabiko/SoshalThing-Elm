module Tweet exposing (payloadResponseDecoder, getShareableArticles)

import Dict
import Json.Decode as Decode exposing (Decoder, field, string, bool, int, maybe)
import Json.Decode.Pipeline as DecodeP

import Article exposing (..)
import TimeParser


type alias DecodedTweet =
  { tweet: Article
  , sharedTweet: Maybe Article
  }


type alias EndpointPayload =
  { articles: List Article
  , timelineArticles: List String
  }


type alias EndpointPayloadResult =
  Result (List (String, Int)) EndpointPayload


getShareableArticles : ArticleCollection -> List String -> List ShareableArticle
getShareableArticles articles ids =
  List.filterMap
    (\id -> 
      case (Dict.get id articles) of
        Just article ->
          case article.share of
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


unpackDecodedTweets : List DecodedTweet -> EndpointPayload
unpackDecodedTweets decodedTweets =
  List.map unpackDecodedTweet decodedTweets
  |> List.unzip
  |> Tuple.mapFirst List.concat
  |> (\tuple -> { articles = Tuple.first tuple, timelineArticles = Tuple.second tuple })


unpackDecodedTweet : DecodedTweet -> (List Article, String)
unpackDecodedTweet decodedTweet =
  ( case decodedTweet.sharedTweet of
          Just tweet ->
            [decodedTweet.tweet, tweet]
          Nothing ->
            [decodedTweet.tweet]
  , decodedTweet.tweet.id
  )


payloadDecoder : Decoder EndpointPayload
payloadDecoder =
  Decode.oneOf
    [ field "statuses" (Decode.map unpackDecodedTweets (Decode.list tweetDecoder))
    , (Decode.map unpackDecodedTweets (Decode.list tweetDecoder))
    , (Decode.map unpackDecodedTweets (Decode.map List.singleton tweetDecoder))
    ]


tweetDecoder : Decoder DecodedTweet
tweetDecoder =
  Decode.andThen (\decodedTweet ->
      Decode.succeed { decodedTweet
        | tweet = fixTweetText decodedTweet.tweet
        , sharedTweet = Maybe.map fixTweetText decodedTweet.sharedTweet
      }
    )
    (Decode.map2 DecodedTweet
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


maybeListCollapse : List (Maybe a) -> List a
maybeListCollapse maybes =
  List.filterMap (\maybeElement -> maybeElement) maybes


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


shareDecoder : Decoder ArticleId
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


payloadResponseDecoder : Decoder EndpointPayloadResult
payloadResponseDecoder =
  Decode.andThen (\maybeErrors ->
        case maybeErrors of
          Just errors ->
            Decode.succeed (Err errors)
          _ ->
            Decode.map Ok payloadDecoder)
        (maybe (field "errors" payloadErrorsDecoder))