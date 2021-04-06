module Filter exposing
  ( Filter, FilterMethod(..), FilterMode(..)
  , filterArticles
  )

import Maybe.Extra as MaybeE

import Article exposing
  (Article
  )


type alias Filter =
  (FilterMethod, FilterMode)


type FilterMethod
  = IsRepost
  | HasMedia


type FilterMode = ExcludeIf | ExcludeIfNot


filterArticles : List Filter -> List (Article a) -> List (Article a)
filterArticles filters articles =
  List.filter (passFiltersArticle filters) articles


passFiltersArticle : List Filter -> (Article a) -> Bool
passFiltersArticle filters article =
  List.any (\filter ->
    case (Tuple.second filter) of
      ExcludeIf ->
        passFilterArticle (Tuple.first filter) article
      ExcludeIfNot ->
        not (passFilterArticle (Tuple.first filter) article)
  ) filters
    |> not


passFilterArticle : FilterMethod -> (Article a) -> Bool
passFilterArticle filter article =
  case filter of
    HasMedia ->
      --(MaybeE.isJust article.media)
      False

    IsRepost ->
      False