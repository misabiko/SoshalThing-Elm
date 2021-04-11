module Twitter.Service exposing (TwitterService, init)

import Url.Builder as UrlB
import Dict

import Service exposing (Service(..), Endpoint, RateLimitInfo)
import Tweet


type alias TwitterService =
  Service Tweet.ArticleExt Tweet.ViewArticleExt


init : TwitterService
init =
  Service
    { name = "Twitter"
    , endpoints = Dict.fromList
        [ initTwitterEndpoint
            "Home Timeline"
            [ "statuses", "home_timeline" ]
            [UrlB.string "count" "200"]
            (Just (Service.initRateLimit 15 (15*60*1000)))
        , initTwitterEndpoint
            "User Timeline"
            [ "statuses", "user_timeline" ]
            []
            (Just (Service.initRateLimit 900 (15*60*1000)))
        , initTwitterEndpoint
            "Search"
            [ "search", "tweets" ]
            []
            (Just (Service.initRateLimit 180 (15*60*1000)))
        , initTwitterEndpoint
            "List"
            [ "lists", "statuses" ]
            []
            (Just (Service.initRateLimit 900 (15*60*1000)))
        ]
    , articles = Dict.empty
    , viewArticle = Tweet.view
    , toViewArticle = Tweet.toViewArticle
    }


initTwitterEndpoint : String -> List String -> List UrlB.QueryParameter -> Maybe RateLimitInfo -> (String, Endpoint)
initTwitterEndpoint name path options maybeRateLimit =
  ( name
  , Service.newEndpoint
    { name = name
    , baseUrl = "http://localhost:5000"
    , path = [ "twitter", "v1" ] ++ path
    , options = (UrlB.string "tweet_mode" "extended") :: options
    , rateLimit = maybeRateLimit
    }
  )