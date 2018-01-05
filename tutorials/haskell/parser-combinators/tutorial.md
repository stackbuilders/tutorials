---
title: Parser Combinators to check business rules
published: 2018-02-01
ghc: 8.0.1
lts: 7.16
tags: haskell
language: haskell
author: Cristhian Motoche
author-name: Cristhian Motoche
twitter-profile: Camm_V222
github-profile: CristhianMotoche
description: Parser Combinators will help you to parse structures easily and save time while checking business rules.
---

Parsing is a common task in software development and consists on looking for
structures into strings of symbols. For instance, getting an object from a JSON,
XML, etc. Some formats are very well known and there are some libraries to deal
with them. However, what would happen with structures that meet specific business
rules? Like license plates of a country, phone numbers, identificators, etc. Perhaps,
it would be hard or impossible to find a library for them. Because of that,
the programmer will have to write a parser to deal with those specific structures.

A parser for a [plate of Ecuador](https://en.wikipedia.org/wiki/Vehicle_registration_plates_of_Ecuador)
(e. g. ABC-1234) will be the example to show the
methods of parsing. The plate will meet the following criteria:

- Start with three letters
- The first letters belongs to a province of the country (e. g. P stands for Pichincha)
- The second letter called the key letter identifies the type of license plate.
	If the key letter is one of [AZQEMW] then it belongs to a special vehicle,
  otherwise it refrest to a particular or private vehicle.
- After the letters there is a hyphen (-)
- At the end there are three of four numbers from 000 to 9999

We can use the parser for two things:
1. Verify that an input text plate is correctly written. Basically, to say "Yes, it’s correct" or
"No, it isn’t".
2. Extract the plate from an input text and use that structure to deal with it in an application.

The plate could be represented with the following structure:

```haskell
data Plate =
	Plate
		{ provinceLetter :: Char
			, keyLetter :: KeyLetter
			, thirdLetter :: Char
			, number :: Int
		}

data KeyLetter = A | Z | Q | E | M | W | Other Char
```

## Methods of parsing
### Using pure functions
It will be necessary to implement a few functions to extract every field of the
defined data type. For instance, let’s try to get the numbers:

```haskell
getNumbers :: String -> Maybe Int
getNumbers plate =
    let nums = dropWhile (not . isDigit) plate
 in readMaybe nums

The following functions will also have to return a

getProvinceLetter :: String -> Maybe Char
getProvinceLetter plate
    | length plate > 1 && head plate `elem` provinceLetters = Just (head plate)
    | otherwise = Nothing

getKeyLetter :: String -> Maybe Char
getKeyLetter plate =
    let second = head (drop 1 plate)
    in
if length plate > 2 && second `elem` keyLetters
then Just (mapKeyLetter second)
    else Nothing

mapKeyLetter :: Char -> KeyLetter
mapKeyLetter ‘A’ = A
mapKeyLetter ‘Z’ = Z
mapKeyLetter ‘Q’ = Q
mapKeyLetter ‘E’ = E
mapKeyLetter ‘M’ = M
mapKeyLetter ‘W’ = W
mapKeyLetter x = Other x

getThirdLetter :: String -> Maybe Char
getThirdLetter plate
    | length plate > 3 && length plate <= 4 = Just (head (drop 2 plate))
    | otherwise = Nothing
```

And then we can use Functor instance of Maybe to do:

```haskell
getPlate :: String -> Maybe Plate
getPlate plate =
  Plate <$> getProvinceLetter plate
    <*> getKeyLetter plate
    <*> getThirdLetter plate
    <*> getNumbers plate
```

So far, so good. However, there are many conditional statements and we depend of
short-circuit evaluation, otherwise the use of `head` is not a good idea. For that
and more, I consider using simple functions not always a good idea to deal with parsing.

### Use regexes
Regular expressions could make things easier.

```haskell
getPlate :: String -> Maybe Plate
getPlate plate =
case matchRegex (mkRegex "([A-Z])([A-Z])([A-Z])-([0-9]{3,4})") plate of
    Just groups ->
      let prov = head (groups !! 0)
          key = mapKeyLetter (head (groups !! 1))
          third = head (groups !! 2)
          number = read (groups !! 3)
          plateStruct = Plate prov key third number
      in Just plateStruct
    Nothing -> Nothing
```

Indeed it makes the code smaller. However, it is unreadable and hard to maintain.
Also, I’m omitting the specific rules for the province letter and the key letter
just to make the regex smaller. The real regex would be something like:

```haskell
regex :: Regex
regex = mkRegex $ “([” ++ provLetters ++ ”])([” ++ keyLetters ++  ”])([A-Z])-([0-9]{3,4})”
```

Sometimes, using regex is a bad idea for dealing with some parsing structures,
because it is hard to maintain and understand.

There are other ways like [recursive-descent](https://en.wikipedia.org/wiki/Recursive_descent_parser)
parsing and parser generators. Of course, there are some implementations of those
parsers in Haskell. For instance [Happy](https://www.haskell.org/happy/) is a parser
generator similar to Yacc. It gets a grammar file from input, then happy generates a
Haskell source which can be used within an application for parsing. Those parsers are
quite complete and based on a formal definition. However, they are quite complex
to deal with simple business rules. For instance, the plate license example.

So, basically:

- Pure functions are easily to write, but not really easy to understand, and also it could be necessary to generate many code depending on the parsing rules.
- Regexes are easy to use, but hard to maintain and understand.
- Use recursive-descent parsing works, but it is tedious, verbose and error-prone as Haoyi says in his blog post.
- Parser generators are flexible, but could be an over engineered for some scenarios.

Parser combinators to the rescue!

What are parser combinators?
First, a parser is a function that accepts strings as input and returning some structure as output.
For instance: `parserInteger :: Parser Integer` is a function that accepts a string of symbols and
returns an integer.

A parser combinator is a higher-order function that accepts several parsers as input
and returns a new parser as its output. For instance: `Parser Integer -> Parser [Integer]` it
takes a parser of integers and returns a parser for a list of integers.

Some examples of parser combinators are defined in the Applicative type class:

```haskell
(<|>) :: Parser a -> Parser a -> Parser a
```

It will try the first parser, in case of failing it will try the following.

```haskell
some :: Parser a -> Parser [a]
```

It will try the given parser for at least one otherwise it will fail.

```haskell
many :: Parser a -> Parser [a]
```

It will try the given parser for at least one otherwise it will fail.

As them, there are many more defined in parser-combinators package. For instance:

```haskell
count :: Int -> Parser a -> Parser [a]
```

It will attempt an amount of times the parser that has as second parameter.

**Note:** I wrote the type annotations of the above functions to see the Parser type
on them, although those functions are polymorphic and work for any instance of
Alternative and Monad.

Parser combinators are flexible and easy to use. Let’s see them in action:

```
parserPlate :: Parser Plate
parserPlate = do
  provinceLetter <- oneOf provinceLetters
  keyLetter <- mapKeyLetter <$> oneOf keyLetters
  thirdLetter <- oneOf ['A'..'Z']
  char '-'
  number <- read <$> (try (count 4 digitChar) <|> count 3 digitChar)
  return Plate{..}
```

And that is all what is necessary for the parser. Now, to tests the parser the following function will be enough:

```
getPlate :: String -> Maybe Plate
getPlate = parseMaybe parserPlate
```

The parser is easy to understand and not very large. In the next section, a more
real example of parser combinators will be described.

Parser combinators in action
Parser combinators can be used to check rules when looking for structures in text.
In order to explain this, a CSV about tickets to owners of cars will be parsed.
The file will have the following structure:

```csv
date,time,owner,plate,reason,amount
2017-11-10,22:00:00,Cristhian Motoche,PAZ-7894,Overspeed,$112.05
2017/12/21,12:00:00 am,Cristhian Motoche,PAZ-7894,Overspeed,$112.05
```

And it will have have to meet the following rules:
- Date:
  - It will start with the year, then the month and finally the day. It could be separated with slashes “/” or dashes “-”.
- Time:
  - It will start be hours, minutes and seconds and it could be in 24-hour or 12-hour clock convention.
- Owner:
  - The owner will have a name and a last name, separated with an space.
- Plate:
  - It will follow the rules of the ecuadorian plate that we have already described.
- Reason:
  - Text with information about the reason of the ticket.
- Amount:
  - Will be the amount of money related to the ticket and it will start with a dollar (‘$’) sign.

In order to parse the CSV, the cassava-megaparsec library will be used, which
uses megaparsec library for parser combinators. The parsing error messages are understandable.

First step, define your data types:

```haskell
data TicketRow =
  TicketRow
    { date :: Day
    , time :: TimeOfDay
    , owner :: String
    , plate :: Plate
    , reason :: String
    , amount :: Amount
    } deriving Show

data Plate =
  Plate
    { provinceLetter :: Char
    , keyLetter :: KeyLetter
    , thirdLetter :: Char
    , number :: Int
    } deriving Show

data KeyLetter = A | Z | Q | E | M | W | Other Char
   deriving Show

data Amount =
  Amount
    { dollars :: Integer
    , cents :: Integer
    } deriving Show

type ParserM = Parsec ConversionError String
```

The first three records will represent data structures related to the ticket, the plate and the
amount of money. The last is a type alias for Parsec.

The next step will be define the FromNamedRecord for `TicketRow` and the FromField instances for
Plate and Amount. The Day and TimeOfDay already have `FromField`, but it is necessary to create a
couple of parsers to accept the rules established and return the type expected. parseDate and
`parseTime’` will work for this case.

```haskell
instance FromNamedRecord TicketRow where
  parseNamedRecord row =
    TicketRow
      <$> (row .: "date" >>= parseDate)
      <*> (row .: "time" >>= parseTime')
      <*> row .: "owner"
      <*> row .: "plate"
      <*> row .: "reason"
      <*> row .: "amount"

instance FromField Plate where
  parseField = withParser parserPlate

instance FromField Amount where
  parseField = withParser parserAmount

parseDate :: ByteString -> Parser Day
parseDate = withParser parserDate

parseTime' :: ByteString -> Parser TimeOfDay
parseTime' = withParser parserTime
```

The withParser function is just a helper that takes a parser combinator, a bytesting to parse and return the data type extracted from the bytesting but in the context of the parser of Data.Csv. The from12HourTo24Hour function will change the format from 12-hour to 24-hour convention. And also, a couple of strings with the valid province and key letters.

```haskell
withParser :: ParserM a -> ByteString -> Parser a
withParser parser bstr =
  case parse parser "" (B8.unpack bstr) of
    Right value -> pure value
    Left err -> fail (parseErrorTextPretty err)

from12HourTo24Hour :: Int -> String -> Maybe Int
from12HourTo24Hour 12 "am" = Just 0
from12HourTo24Hour time "am" =
  if time < 12 && time > 0
     then Just time
     else Nothing
from12HourTo24Hour 12 "pm" = Just 12
from12HourTo24Hour time "pm" =
  if time < 12 && time > 0
     then Just (time + 12)
     else Nothing
from12HourTo24Hour _ _ = Nothing

provinceLetters :: String
provinceLetters = "ABUCXHOEWGILRMVNSPQKTZYJ"

keyLetters :: String
keyLetters = [‘A’..’Z’]

decodeTicketRows
  :: String
  -> Either (ParseError Word8 ConversionError) (Header, Vector TicketRow)
decodeTicketRows = decodeByName "" . BL8.pack
```

The parserDate will take the first 4 digit chars for the year, then the value of the divisor
is held, so then it can be checked that this value is the same to separate the month and day.
Finally, the `fromGregorian` function will take the parsed values to return the Day.

```haskell
parserDate :: ParserM Day
parserDate = do
  year <- read <$> (count 4 digitChar)
  divisor <- char '/' <|> char '-'
  month <- read <$> (count 2 digitChar)
  satisfy (== divisor)
  day <- read <$> (count 2 digitChar)
  return $ fromGregorian year month day
```

To parse the time of the day, the `try` parse combinator will attempt to parse the time in 24 hour
convention otherwise the 12 hour convention. 

```haskell
parserTime :: ParserM TimeOfDay
parserTime =
  try parser24HourTime
    <|> parser12HourTime
```

A parser helper for the gregorian time will get the hours, minutes and seconds.

```haskell
parserGregorianTime :: ParserM (Int, Int, Integer)
parserGregorianTime = do
  hour <- read <$> (try (count 2 digitChar) <|> count 1 digitChar)
  char ':'
  min <- read <$> count 2 digitChar
  char ':'
  sec <- read <$> count 2 digitChar
  return (hour, min, sec)
```

For the time of the day parser, the parser combinator `eof` will stop looking for more input.
Then makeTimeOfDayValid will attempt to create a valid TimeOfDay.

```haskell
parser24HourTime :: ParserM TimeOfDay
parser24HourTime = do
  (hour, min, sec) <- parserGregorianTime
  eof
  let validTime = makeTimeOfDayValid hour min (MkFixed sec)
      failToFormat = fail "Not a valid 24-hour time"
   in maybe failToFormat return validTime
```

For the time of day parser in the 12-hour convention, after getting the time an space char and the period of the date are parsed. Then the helper from12HourTo24Hour will try to transform the time in a valid TimeOfTheDay.

```haskell
parser12HourTime :: ParserM TimeOfDay
parser12HourTime = do
  (tmpHour, min, sec) <- parserGregorianTime
  char ' '
  period <- string "am" <|> string "pm"
  let validTime hour = makeTimeOfDayValid hour min (MkFixed sec)
      failToFormat = fail "Not a valid 12-hour time"
      validHour = from12HourTo24Hour tmpHour period
   in maybe failToFormat return (validHour >>= validTime)
```

I will skip the explanation of the parserPlate, since it was described at the beginning of the reading.

```haskell
parserPlate :: ParserM Plate
parserPlate = do
  provinceLetter <- oneOf provinceLetters
  keyLetter <- oneOf keyLetters
  thirdLetter <- oneOf ['A'..'Z']
  char '-'
  number <- read <$> (try (count 4 digitChar) <|> count 3 digitChar)
  return Plate{..}
```

Finally the parserAmount will try to read an Amount, that starts with a `$` and separates the dollars from the cents by a ‘.’.

```haskell
parserAmount :: ParserM Amount
parserAmount = do
  char '$'
  amountDollar <- read <$> many digitChar
  char '.'
  amountCents <- read <$> count 2 digitChar
  return $ Amount amountDollar amountCents
```

Finally, plug all together in a main function that will read a file called `example.csv` in the current folder and will display the error or print the parsed items.

```haskell
main :: IO ()
main = do
  content <- readFile "./example.csv"
  case decodeTicketRows content of
    Right items -> print items
    Left errors -> putStrLn (parseErrorPretty errors)
```
