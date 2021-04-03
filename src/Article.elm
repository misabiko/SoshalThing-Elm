module Article exposing
    ( Article, ArticleId, Collection
    , SocialData, ImageData
    , ShareableArticle, getShareableArticles, getShareableId
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


type alias Collection =
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


getShareableArticles : Collection -> List String -> List ShareableArticle
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


getShareableId : ShareableArticle -> String
getShareableId shareableArticle =
  case shareableArticle.sharedArticle of
    Just shared -> shareableArticle.article.id ++ shared.id
    Nothing -> shareableArticle.article.id