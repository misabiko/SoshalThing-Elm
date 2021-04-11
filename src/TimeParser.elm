module TimeParser exposing (tweetTimeDecoder, relativeTimeFormat, toFullDateFormat, toFullTimeFormat, TimeModel)

import Time exposing (..)
import Time.Extra as TimeE
import Parser exposing (..)
import Json.Decode
import Set


-- PARSING


type alias ParsedTweetTime =
  { month: Month
  , day: Int
  , hour: Int
  , minute: Int
  , second: Int
  , year: Int
  }


tweetTimeDecoder : String -> Json.Decode.Decoder Posix
tweetTimeDecoder timeString =
  case (run tweetTimeParser timeString) of
    Ok parsed ->
      parsedTimeToParts parsed
        |> TimeE.partsToPosix utc
        |> Json.Decode.succeed

    Err deadEnds ->
      Json.Decode.fail ("Failed to parse the 'created_at' field '" ++ timeString ++ "': \n" ++ (deadEndsToString deadEnds))


deadEndsToString : List DeadEnd -> String
deadEndsToString deadEnds =
  List.foldl (++) "" (List.map deadEndToString deadEnds)


deadEndToString : DeadEnd -> String
deadEndToString deadEnd =
  (case deadEnd.problem of
    Expecting str ->
      "Expecting: " ++ str
    ExpectingInt ->
      "ExpectingInt"
    ExpectingHex ->
      "ExpectingHex"
    ExpectingOctal ->
      "ExpectingOctal"
    ExpectingBinary ->
      "ExpectingBinary"
    ExpectingFloat ->
      "ExpectingFloat"
    ExpectingNumber ->
      "ExpectingNumber"
    ExpectingVariable ->
      "ExpectingVariable"
    ExpectingSymbol str ->
      "ExpectingSymbol: " ++ str
    ExpectingKeyword str ->
      "ExpectingKeyword: " ++ str
    ExpectingEnd ->
      "ExpectingEnd"
    UnexpectedChar ->
      "UnexpectedChar"
    Problem str ->
      "Problem: " ++ str
    BadRepeat ->
      "BadRepeat")
  ++ String.concat 
    [ " at ["
    , (String.fromInt deadEnd.row)
    , ", "
    , (String.fromInt deadEnd.col)
    , "]"
    ]


parsedTimeToParts : ParsedTweetTime -> TimeE.Parts
parsedTimeToParts parsed =
  TimeE.Parts parsed.year parsed.month parsed.day parsed.hour parsed.minute parsed.second 0


-- Tue Aug 18 00:00:00 +0000 2020
-- TimeE.Parts 2018 Sep 26 14 30 0 0
tweetTimeParser : Parser ParsedTweetTime
tweetTimeParser =
  succeed ParsedTweetTime
    |. oneOf (List.map keyword [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sun", "Sat" ])
    |. spaces
    |= andThen parseMonth (variable { start = Char.isUpper, inner = Char.isAlpha, reserved = Set.empty })
    |. spaces
    |= paddedInt
    |. spaces
    |= paddedInt
    |. symbol ":"
    |= paddedInt
    |. symbol ":"
    |= paddedInt
    |. spaces
    |. keyword "+0000"
    |. spaces
    |= int
    |. end


paddedInt : Parser Int
paddedInt =
  succeed identity
    |. oneOf
        [ symbol "0"
        , succeed ()
        ]
    |= int


parseMonth : String -> Parser Month
parseMonth str =
  case str of
    "Jan" -> succeed Jan
    "Feb" -> succeed Feb
    "Mar" -> succeed Mar
    "Apr" -> succeed Apr
    "May" -> succeed May
    "Jun" -> succeed Jun
    "Jul" -> succeed Jul
    "Aug" -> succeed Aug
    "Sep" -> succeed Sep
    "Oct" -> succeed Oct
    "Nov" -> succeed Nov
    "Dec" -> succeed Dec
    _ -> problem ("Couldn't parse the month '" ++ str ++ "'" )


-- FORMATTING


type alias TimeModel =
  { zone : Time.Zone
  , lastNow : Time.Posix
  }


-- TimeE.diff only uses the zone for Day, Month and Weekday
relativeTimeFormat : TimeModel -> Posix -> String
relativeTimeFormat timeModel time =
  if (TimeE.diff TimeE.Year timeModel.zone time timeModel.lastNow) > 0 then
    toFullDateFormat timeModel time
  else if (TimeE.diff TimeE.Month timeModel.zone time timeModel.lastNow) > 0 then
    toDayMonthFormat timeModel time
  else
    let
      dayDiff = TimeE.diff TimeE.Day timeModel.zone time timeModel.lastNow
    in
      if dayDiff > 0 then
        (String.fromInt dayDiff) ++ "d"
  else
    let
      hourDiff = TimeE.diff TimeE.Hour timeModel.zone time timeModel.lastNow
    in
      if hourDiff > 0 then
        (String.fromInt hourDiff) ++ "h"
  else
    let
      minDiff = TimeE.diff TimeE.Minute timeModel.zone time timeModel.lastNow
    in
      if minDiff > 0 then
        (String.fromInt minDiff) ++ "m"
  else
    let
      secDiff = TimeE.diff TimeE.Second timeModel.zone time timeModel.lastNow
    in
      if secDiff > 0 then
        (String.fromInt secDiff) ++ "s"
  else
    "just now"


toDayMonthFormat : TimeModel -> Posix -> String
toDayMonthFormat timeModel time =
  String.fromInt (toDay timeModel.zone time)
  ++ " " ++
  toAbbrevMonth (toMonth timeModel.zone time)


toFullDateFormat : TimeModel -> Posix -> String
toFullDateFormat timeModel time =
  (toDayMonthFormat timeModel time)
  ++ " " ++
  String.fromInt (toYear timeModel.zone time)


-- 4:05 PM Mar 30 2021
toFullTimeFormat : TimeModel -> Posix -> String
toFullTimeFormat timeModel time =
  String.fromInt (toHour timeModel.zone time)
  ++ ":" ++
  String.fromInt (toMinute timeModel.zone time)
  ++ " " ++
  (toFullDateFormat timeModel time)


toAbbrevMonth : Month -> String
toAbbrevMonth month =
  case month of
    Jan -> "Jan"
    Feb -> "Feb"
    Mar -> "Mar"
    Apr -> "Apr"
    May -> "May"
    Jun -> "Jun"
    Jul -> "Jul"
    Aug -> "Aug"
    Sep -> "Sep"
    Oct -> "Oct"
    Nov -> "Nov"
    Dec -> "Dec"