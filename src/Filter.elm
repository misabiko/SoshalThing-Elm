module Filter exposing
  ( Filter, FilterMethod(..), FilterMode(..)
  , filterArticles
  , viewFilter, Msg(..)
  )

import Maybe.Extra as MaybeE
import Html exposing (..)
import Html.Events exposing (onCheck)
import Html.Attributes exposing (..)
import Array exposing (Array)

import Article exposing (Article)


type alias Filter a =
  { enabled: Bool
  , method: FilterMethod a
  , mode: FilterMode
  }


type FilterMethod a
  = IsRepost
  | HasMedia
  | Custom ((Article a) -> Bool)


type FilterMode = ExcludeIf | ExcludeIfNot


type Msg
  = SetEnabled Int Bool
  | SetMode Int FilterMode


filterArticles : Array (Filter a) -> List (Article a) -> List (Article a)
filterArticles filters articles =
  List.filter (passFiltersArticle filters) articles


passFiltersArticle : Array (Filter a) -> (Article a) -> Bool
passFiltersArticle filters article =
  List.any (\filter ->
    if filter.enabled then
      case filter.mode of
        ExcludeIf ->
          passFilterArticle filter.method article
        ExcludeIfNot ->
          not (passFilterArticle filter.method article)
    else
      False
  ) (Array.toList filters)
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


-- VIEW


viewFilter : Int -> Filter a -> Html Msg
viewFilter index filter =
  div [ class "field", class "has-addons" ]
    [ p [ class "control" ]
        [ input [ type_ "checkbox", checked filter.enabled, onCheck (SetEnabled index) ] [] ]
    , p [ class "control" ]
        [ button [ class "button", class "is-static" ]
            [ case filter.method of
                IsRepost -> text "IsRepost"
                HasMedia -> text "HasMedia"
                Custom _ -> text "Custom"
            ]
        ]
    , p [ class "control" ]
        [ span
            [ class "select" ]
            [ select []
                ( case filter.mode of
                    ExcludeIf ->
                      [ option [ selected True ] [ text "Exclude if" ]
                      , option [] [ text "Exclude if not" ]
                      ]
                    ExcludeIfNot ->
                      [ option [] [ text "Exclude if" ]
                      , option [ selected True ] [ text "Exclude if not" ]
                      ]
                )
            ]
        ]
    ]