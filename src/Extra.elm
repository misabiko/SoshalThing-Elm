module Extra exposing (..)


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