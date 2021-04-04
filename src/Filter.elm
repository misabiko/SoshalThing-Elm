module Filter exposing
  ( Filter, FilterMethod(..), FilterMode(..)
  , filterShareableArticles, filterArticles
  )

import Maybe.Extra as MaybeE

import Article exposing (Article, ShareableArticle)


type alias Filter =
  (FilterMethod, FilterMode)


type FilterMethod
  = IsRepost
  | HasMedia


type FilterMode = ExcludeIf | ExcludeIfNot

filterShareableArticles : List Filter -> List ShareableArticle -> List ShareableArticle
filterShareableArticles filters shareableArticles =
  List.filter (passFiltersShareableArticle filters) shareableArticles

filterArticles : List Filter -> List Article -> List Article
filterArticles filters articles =
  List.filter (passFiltersArticle filters) articles


passFiltersShareableArticle : List Filter -> ShareableArticle -> Bool
passFiltersShareableArticle filters shareableArticle =
  List.any (\filter ->
    case (Tuple.second filter) of
      ExcludeIf ->
        passFilterShareableArticle (Tuple.first filter) shareableArticle
      ExcludeIfNot ->
        not (passFilterShareableArticle (Tuple.first filter) shareableArticle)
  ) filters
    |> not


passFiltersArticle : List Filter -> Article -> Bool
passFiltersArticle filters article =
  List.any (\filter ->
    case (Tuple.second filter) of
      ExcludeIf ->
        passFilterArticle (Tuple.first filter) article
      ExcludeIfNot ->
        not (passFilterArticle (Tuple.first filter) article)
  ) filters
    |> not


passFilterArticle : FilterMethod -> Article -> Bool
passFilterArticle filter article =
  case filter of
    HasMedia ->
      (MaybeE.isJust article.media)

    IsRepost ->
      False


passFilterShareableArticle : FilterMethod -> ShareableArticle -> Bool
passFilterShareableArticle filter shareableArticle =
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