module Filter exposing
  ( Filter, FilterMethod(..), FilterMode(..)
  , filterArticles
  )

import Maybe.Extra as MaybeE

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
          (MaybeE.isJust shareableArticle.article.media) || (MaybeE.isJust sharedArticle.media)
        Nothing ->
          (MaybeE.isJust shareableArticle.article.media)

    IsRepost ->
      case shareableArticle.sharedArticle of
        Just _ ->
          MaybeE.isNothing shareableArticle.article.text
        Nothing ->
          False