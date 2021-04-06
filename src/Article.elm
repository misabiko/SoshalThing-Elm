module Article exposing
    ( Article, Id, Collection, listToDict
    , SocialData, Media(..), ImageData, VideoData
    , ShareableArticle, getShareableArticles, getShareableId
    )


import Dict exposing (Dict)
import Time
import Html.Attributes exposing (autoplay)


type alias Article =
  { id: Id
  , creationDate: Time.Posix
  , text: Maybe String
  , social: Maybe SocialData
  , share: Maybe Id
  , media: Maybe Media
  , index: Maybe Int
  }


type alias Id = String


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
  , size: Maybe {width: Int, height: Int}
  }


type alias VideoData =
  { url: String
  , compressedUrl: String
  , autoplay: Bool
  }


type Media
  = Images (List ImageData)
  | Video VideoData


type alias ShareableArticle =
  { article: Article
  , sharedArticle: Maybe Article
  }


getShareableArticles : Collection -> List Id -> List ShareableArticle
getShareableArticles articles ids =
  List.filterMap
    (\id ->
      Dict.get id articles
        |> Maybe.andThen (\article ->
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
        )
    )
    ids


getShareableId : ShareableArticle -> String
getShareableId shareableArticle =
  case shareableArticle.sharedArticle of
    Just shared -> shareableArticle.article.id ++ shared.id
    Nothing -> shareableArticle.article.id


listToDict : List Article -> Collection
listToDict articles =
  Dict.fromList
    <| List.map (\article -> (article.id, article)) articles