module Article exposing
    ( Article, ArticleId, ArticleCollection
    , SocialData, ImageData
    , ShareableArticle
    )


import Dict exposing (Dict)
import Time


type alias Article =
  { id: ArticleId
  , creationDate: Time.Posix
  , text: Maybe String
  , social: Maybe SocialData
  , share: Maybe ArticleId
  , images: Maybe (List ImageData)
  }


type alias ArticleId = String


type alias ArticleCollection =
  Dict String Article


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
  }


type alias ShareableArticle =
  { article: Article
  , sharedArticle: Maybe Article
  }