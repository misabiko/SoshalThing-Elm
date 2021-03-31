module TimeParser exposing (tweetTimeDecoder, timeFormat)

import Time exposing (Month(..), toDay, toMonth, toYear)
import Time.Extra as TimeE
import Parser exposing (..)
import Json.Decode
import Set


-- PARSING


tweetTimeDecoder : String -> Json.Decode.Decoder Time.Posix
tweetTimeDecoder timeString =
  case (run tweetTimeParser timeString) of
    Ok parsed ->
      parsedTimeToParts parsed
        |> TimeE.partsToPosix Time.utc
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
  ++ " at ["
  ++ (String.fromInt deadEnd.row)
  ++ ", "
  ++ (String.fromInt deadEnd.col)
  ++ "]"


type alias ParsedTweetTime =
  { month: Time.Month
  , day: Int
  , hour: Int
  , minute: Int
  , second: Int
  , year: Int
  }


parsedTimeToParts : ParsedTweetTime -> TimeE.Parts
parsedTimeToParts parsed =
  TimeE.Parts parsed.year parsed.month parsed.day parsed.hour parsed.minute parsed.second 0


-- Tue Aug 18 00:00:00 +0000 2020
-- TimeE.Parts 2018 Time.Sep 26 14 30 0 0
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


parseMonth : String -> Parser Time.Month
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


timeFormat : Time.Posix -> String
timeFormat time =
  String.fromInt (toDay Time.utc time)
  ++ " " ++
  toAbbrevMonth (toMonth Time.utc time)
  ++ " " ++
  String.fromInt (toYear Time.utc time)


toAbbrevMonth : Time.Month -> String
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

