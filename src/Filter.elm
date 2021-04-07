module Filter exposing
  ( Filter, FilterMethod(..), FilterMode(..)
  , filterArticles
  )

import Maybe.Extra as MaybeE

import Article exposing
  (Article
  )


type alias Filter a =
  (FilterMethod a, FilterMode)


type FilterMethod a
  = IsRepost
  | HasMedia
  | Custom ((Article a) -> Bool)


type FilterMode = ExcludeIf | ExcludeIfNot


filterArticles : List (Filter a) -> List (Article a) -> List (Article a)
filterArticles filters articles =
  List.filter (passFiltersArticle filters) articles


passFiltersArticle : List (Filter a) -> (Article a) -> Bool
passFiltersArticle filters article =
  List.any (\filter ->
    case (Tuple.second filter) of
      ExcludeIf ->
        passFilterArticle (Tuple.first filter) article
      ExcludeIfNot ->
        not (passFilterArticle (Tuple.first filter) article)
  ) filters
    |> not


passFilterArticle : FilterMethod a -> Article a -> Bool
passFilterArticle filter article =
  case filter of
    HasMedia ->
      --(MaybeE.isJust article.media)
      False

    IsRepost ->
      False

    Custom getter ->
      getter article