module Article exposing
    ( Article, Id
    , Collection, get
    , listToDict
    , SocialData, MediaContent(..), ImageData, VideoData
    )


import Dict exposing (Dict)
import Time


type alias Article a =
  { a
  | id: Id
  , creationDate: Time.Posix
  }


type alias Id = String


type alias Collection a =
  Dict String (Article a)


-- EXTENSIONS

type alias SocialData =
  { authorName: String
  , authorHandle: String
  , authorAvatar: String
  , liked: Bool
  , reposted: Bool
  , likeCount: Int
  , repostCount: Int
  }


type alias ImageData =
  { url: String
  , compressedUrl: String
  , size: Maybe {width: Int, height: Int}
  }


type alias VideoData =
  { url: String
  , compressedUrl: String
  , autoplay: Bool
  }


type MediaContent
  = Images (List ImageData)
  | Image ImageData
  | Video VideoData


-- HELPERS


listToDict : List (Article a) -> (Collection a)
listToDict articles =
  Dict.fromList
    <| List.map (\article -> (article.id, article)) articles


get : Collection a -> Id -> Maybe (Article a)
get articles id =
  Dict.get id articles