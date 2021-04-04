module Filter exposing
  ( Filter, FilterMethod(..), FilterMode(..)
  , filterArticles
  )

import Maybe.Extra

import Article exposing (Article, ShareableArticle)


type alias Filter =
  (FilterMethod, FilterMode)


type FilterMethod
  = IsRepost
  | HasMedia


type FilterMode = ExcludeIf | ExcludeIfNot

filterArticles : List Filter -> List ShareableArticle -> List ShareableArticle
filterArticles filters shareableArticles =
  List.filter (passFiltersArticle filters) shareableArticles


passFiltersArticle : List Filter -> ShareableArticle -> Bool
passFiltersArticle filters shareableArticle =
  List.any (\filter ->
    case (Tuple.second filter) of
      ExcludeIf ->
        passFilterArticle (Tuple.first filter) shareableArticle
      ExcludeIfNot ->
        not (passFilterArticle (Tuple.first filter) shareableArticle)
  ) filters
    |> not


passFilterArticle : FilterMethod -> ShareableArticle -> Bool
passFilterArticle filter shareableArticle =
  case filter of
    HasMedia ->
      case shareableArticle.sharedArticle of
        Just sharedArticle ->
          (hasMedia shareableArticle.article) || (hasMedia sharedArticle)
        Nothing ->
          (hasMedia shareableArticle.article)

    IsRepost ->
      case shareableArticle.sharedArticle of
        Just _ ->
          Maybe.Extra.isNothing shareableArticle.article.text
        Nothing ->
          False


hasMedia : Article -> Bool
hasMedia article =
  not (List.isEmpty (Maybe.withDefault [] article.images))