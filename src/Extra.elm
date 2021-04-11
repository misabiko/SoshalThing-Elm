module Extra exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


consr : List a -> Maybe a -> List a
consr list item =
    case item of
        Just v ->
             list ++ [v]

        Nothing ->
            list


maybeJoin : Maybe a -> a -> List a
maybeJoin item el =
    case item of
        Just v ->
            [v, el]

        Nothing ->
          [el]


maybeJoinR : a -> Maybe a -> List a
maybeJoinR el item =
    case item of
        Just v ->
             [el, v]

        Nothing ->
          [el]


viewIcon : String -> String -> String -> Html msg
viewIcon icon iconType size =
  span [ class "icon" ]
    [ i [ class iconType, class icon, class size ] [] ]