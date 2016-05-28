---
title: CSV encoding and decoding in Haskell with Cassava
published: 2016-05-31
ghc: 7.10.3
lts: 5.15
libraries: cassava-0.4.5.0
language: haskell
author-name: Juan Pedro Villa Isaza
---

> “Is that right? Or is it left?”

Comma-separated values (CSV) files are frequently used for exchanging
and converting data between spreadsheet programs and databases. Thus,
CSV encoding and decoding is a common problem to solve in software
development. In Haskell, we can encode and decode CSV files with the
[Cassava][cassava] library, which is easy to use and efficient.

Even though there's no single specification for CSV files,
[RFC 4180][rfc-4180] provides a common definition of the CSV format.
Cassava's implementation is RFC 4180 compliant with a few extensions
such as ignoring empty lines and allowing empty files.

In short, a CSV file is a set of records separated by line breaks, and
a record is a set of fields separated by commas. For example:

```
Aarhus,http://www.odaa.dk/,International Regional
Alabama,http://open.alabama.gov/,US State
```

The first line of a CSV file may be a header record containing the
names of the fields of the following records. For example:

```
Item,Link,Type
Aarhus,http://www.odaa.dk/,International Regional
Alabama,http://open.alabama.gov/,US State
```

These are the first three lines of an [Open Government][open-gov] CSV
file listing countries and regions with open data websites. Let's
download the file and call it `items.csv`:

```
$ curl https://www.data.gov/app/uploads/2016/02/opendatasites.csv -o items.csv
```

In this tutorial, we'll use the Cassava library to read and write
files like this one. As examples, we'll first read the `items.csv`
file and count the number of countries with open data websites, and
then write a new CSV file including only those countries.

In order to understand the data, let's take a closer look at the
`items.csv` file. The first line is the header:

```
Item,Link,Type
```

An item is the name of an open data item, that is, the name of a
country or region. A link is the URL to an item's open data website.
And a type is the type of an open data item, like country or region.
The rest of the lines are records with three such fields. Here are
some examples:

```
United Kingdom,http://data.gov.uk/,International Country
United Nations,http://data.un.org/,International Regional
Uruguay,http://datos.gub.uy/,International Country
Utah,http://www.utah.gov/data/,US State
Vancouver,http://data.vancouver.ca/,International Regional
```

Given such a CSV file, we'd like to turn its contents into useful data
representing open data items that we could use for updating a database
or doing calculations. To do this, we first need custom data types to
represent open data items.

Let's create an `OpenData` module for our solution and add two
extensions:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
```

Next, let's declare the module and add a list of explicit exports:

```haskell
module OpenData
  ( Item(..)
  , ItemType(..)
  , decodeItems
  , decodeItemsFromFile
  , encodeItems
  , encodeItemsToFile
  , filterCountryItems
  , itemHeader
  , japanItem
  , japanRecord
  )
  where
```

And now let's import everything we need. In order to make it easier to
know where things come from, we group imports by package, and use
explicit imports for types and some operators, and qualified imports
otherwise:

```haskell
-- base
import Control.Exception (IOException)
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable

-- bytestring
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

-- cassava
import Data.Csv
  ( DefaultOrdered(headerOrder)
  , FromField(parseField)
  , FromNamedRecord(parseNamedRecord)
  , Header
  , ToField(toField)
  , ToNamedRecord(toNamedRecord)
  , (.:)
  , (.=)
  )
import qualified Data.Csv as Cassava

-- text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

-- vector
import Data.Vector (Vector)
import qualified Data.Vector as Vector
```

The preamble is ready, so we can now define a data type to represent
an open data item:

```haskell
data Item =
  Item
    { itemName :: Text
    , itemLink :: Text
    , itemType :: ItemType
    }
  deriving (Eq, Show)
```

This is a record type with three fields, one for each field in the
records of the CSV file. An item's type is represented by a custom
data type called `ItemType`:

```haskell
data ItemType
  = Country
  | Other Text
  deriving (Eq, Show)
```

Since we're only interested in countries, this type is explicit only
for countries. We could, however, enumerate all possible item types if
we wanted to be more precise.

Using these types, we can now have a better specification of the
problem we're trying to solve. Given a CSV record like the following:

```
Japan,http://www.data.go.jp/,International Country
```

Which we can represent as a `ByteString`:

```haskell
japanRecord :: ByteString
japanRecord =
  "Japan,http://www.data.go.jp/,International Country"
```

We'd like to decode a value equivalent to the following item:

```haskell
japanItem :: Item
japanItem =
  Item
    { itemName = "Japan"
    , itemLink = "http://www.data.go.jp/"
    , itemType = Country
    }
```

(If you're adding code to an `OpenData` module, then don't forget to
add `japanRecord` and `japanItem`. We'll use them as examples below.)

In order to decode one such record into a value of type `Item`, we
need `Item` to be an instance of either `FromRecord` or
`FromNamedRecord`, Cassava's type classes for decoding CSV records. We
use `FromRecord` for CSV files with no header, and `FromNamedRecord`
for CSV files with a header. In this case, the CSV file has a header,
so let's make `Item` an instance of `FromNamedRecord`:

```haskell
instance FromNamedRecord Item where
  parseNamedRecord m =
    Item
      <$> m .: "Item"
      <*> m .: "Link"
      <*> m .: "Type"
```

To declare an instance of `FromNamedRecord`, we need to implement the
`parseNamedRecord` function, which takes a map of names to fields and
returns a parser of `Item`. The `(.:)` operator is a lookup operator,
so `m .: "Item"` means that we look up a field with name `Item` in the
map `m`. If there's such a field in the map, we use it as the first
argument of the `Item` constructor, that is, we assign it to the
`itemName` field.

Cassava has to know how to turn each field (`ByteString`) into types:
`Text` for the names and links, and `ItemType` for types. Just like
`FromNamedRecord` turns a record into a custom data type, the
`FromField` type class determines how to turn a record field into a
type. Cassava knows how to turn a `ByteString` into a `Text`, but we
have to tell it how to turn a `ByteString` into an `ItemType`:

```haskell
instance FromField ItemType where
  parseField "International Country" =
    pure Country

  parseField otherType =
    Other <$> parseField otherType

```

In this case, we need to implement the `parseField` function, which
takes a field (`ByteString`) and returns a parser of `ItemType`. If
the field is `"International Country"`, we return the `Country` value.
In any other case, we return the `Other` constructor applied to a
value of `Text` representing the item type.

At this point, we have enough functions to decode CSV records into
open data items using the `Cassava.decodeByName` function:

```haskell
Cassava.decodeByName
  :: FromNamedRecord a
  => ByteString
  -> Either String (Header, Vector a)
```

For a type `a` which is an instance of `FromNamedRecord`, this
function takes a `ByteString` representing a CSV file as input, and
returns a `String` if decoding fails, or a tuple with a header and a
vector of values of type `a` if decoding succeeds.

We can now test our implementation using this function to decode a
file with the Japan open data item:

```
> :{
| Cassava.decodeByName
|   "Item,Link,Type\n\
|   \Japan,http://www.data.go.jp/,International Country\n"
|   :: Either String (Header, Vector Item)
| :}
Right (["Item","Link","Type"],[Item {itemName = "Japan", itemLink = "http://www.data.go.jp/", itemType = Country}])
```

If you're testing the implementation, then remember to add the
extensions and the imports to GHCi (or to a `.ghci` file):

```
> :set -XOverloadedStrings
> :set -XRecordWildCards
>
> import Control.Exception (IOException)
> import qualified Control.Exception as Exception
> import qualified Data.Foldable as Foldable
>
> ...
```

Note that we have to include a header record because we're telling
Cassava to expect one such line by using the `Cassava.decodeByName`
function. Try removing the header record and see what happens.

We can make decoding easier by defining a wrapper function to specify
the type we want to decode (`Item`):

```haskell
decodeItems
  :: ByteString
  -> Either String (Vector Item)
decodeItems =
  fmap snd . Cassava.decodeByName
```

After decoding, we ignore the header record using `fmap snd`. Let's
decode the file with the Japan open data item again:

```
> :{
| decodeItems
|   "Item,Link,Type\n\
|   \Japan,http://www.data.go.jp/,International Country\n"
| :}
Right [Item {itemName = "Japan", itemLink = "http://www.data.go.jp/", itemType = Country}]
```

Now, we need a wrapper around `decodeItems` to first read a file and
then decode its contents into a vector of items:

```haskell
decodeItemsFromFile
  :: FilePath
  -> IO (Either String (Vector Item))
decodeItemsFromFile filePath =
  catchShowIO (ByteString.readFile filePath)
    >>= return . either Left decodeItems
```

Given a file path, we read its contents as a `ByteString`. This could
fail if the file doesn't exist or if we don't have read permissions.
If reading the file is successful, we decode the items using
`decodeItems`. Otherwise, we return a failure using `Left`.

Here's the `catchShowIO` function, which tries to run an action and
shows an IO exception if one is raised:

```haskell
catchShowIO
  :: IO a
  -> IO (Either String a)
catchShowIO action =
  fmap Right action
    `Exception.catch` handleIOException
  where
    handleIOException
      :: IOException
      -> IO (Either String a)
    handleIOException =
      return . Left . show
```

We have all pieces together to decode the complete `items.csv` file.
Let's try it:

```
> decodeItemsFromFile "items.csv"
Left "parse error (endOfInput) at Baden-W\187rttemberg,http://opendata.service-bw.de/Seiten/default.aspx,International Regional\r\nBah\144a Bl (truncated)"
```

OK, not quite...

It turns out that the `items.csv` file contains names such as
Baden-Württemberg and Bahía Blanca. The `FromField` instance for
`Text` assumes UTF-8, but that's not the encoding of this file.

In this case, we can fix this problem by using a function such as
`Text.decodeLatin1` after looking up the field with name `"Item"`:

```haskell
instance FromNamedRecord Item where
  parseNamedRecord m =
    Item
      <$> fmap Text.decodeLatin1 (m .: "Item")
      <*> m .: "Link"
      <*> m .: "Type"
```

Let's try again:

```
> decodeItemsFromFile "items.csv"
Right ...
```

We successfully decoded the CSV file and turned it into values of type
`Item`. We can use these values to count the number of countries with
open data websites. To do so, let's define a function to filter items:

```haskell
filterCountryItems
  :: Vector Item
  -> Vector Item
filterCountryItems =
  Vector.filter isCountryItem
```

The `isCountryItem` function can be defined as follows:

```haskell
isCountryItem
  :: Item
  -> Bool
isCountryItem =
  (==) Country . itemType
```

Now, let's create a `Main` module to use these functions and count the
number of country items:

```haskell
module Main
  ( main
  )
  where
```

We don't need a lot of imports:

```haskell
-- open-data
import OpenData

-- base
import qualified Control.Monad as Monad
import qualified System.Exit as Exit
```

And we can simply decode the `items.csv` file and then print the
length of the result of filtering the country items:

```haskell
main :: IO ()
main = do
  putStrLn "Open data!"

  eitherCountryItems <-
    fmap filterCountryItems
      <$> decodeItemsFromFile "items.csv"

  case eitherCountryItems of
    Left reason ->
      Exit.die reason

    Right countryItems -> do
      putStr "Number of country items: "
      print (length countryItems)
```

Let's run this:

```
> main
Open data!
Number of country items: 53
```

There are 53 open data countries and we'd now like to write a CSV file
containing those countries.

Let's go back to the `OpenData` module and add more instances. The
process is similar, but we now need instances of `ToNamedRecord` (or
`ToRecord` if we had no header) and `ToField`. Let's make `Item` an
instance of `ToNamedRecord`:

```haskell
instance ToNamedRecord Item where
  toNamedRecord Item{..} =
    Cassava.namedRecord
      [ "Item" .= itemName
      , "Link" .= itemLink
      , "Type" .= itemType
      ]
```

To do so, we need to implement the `toNamedRecord` function, which
takes an `Item` and returns a named record (a map of names to fields).
Here, the `(.=)` operator constructs named fields, that is, pairs of
names and values, and the `Cassava.namedRecord` function turns a list
of named fields into a named record.

In order to make this work, Cassava has to know how to turn each value
into a field. Since we have a custom field (`ItemType`), we have to
tell Cassava how to convert it into a CSV field:

```haskell
instance ToField ItemType where
  toField Country =
    "International Country"

  toField (Other otherType) =
    toField otherType
```

The `toField` function takes an `ItemType` and returns a field
(`ByteString`). If the item's type is `Country`, we return
`"International Country"`. If it's some other type, we return the name
of the type.

We could now use the `Cassava.encodeByName` function to encode a list
of items:

```haskell
Cassava.encodeByName
  :: ToNamedRecord a
  => Header
  -> [a]
  -> ByteString
```

This function takes a header and a list of items, and returns a
`ByteString` representing a CSV file with the items as records. The
header is a vector of names:

```haskell
itemHeader :: Header
itemHeader =
  Vector.fromList
    [ "Item"
    , "Link"
    , "Type"
    ]
```

Let's try encoding the Japan example from above:

```
> Cassava.encodeByName itemHeader [japanItem]
"Item,Link,Type\r\nJapan,http://www.data.go.jp/,\"International Country\"\r\n"
```

Cassava provides one more type class to specify how to order fields.
We can make the `Item` type an instance of `DefaultOrdered`:

```haskell
instance DefaultOrdered Item where
  headerOrder _ =
    Cassava.header
      [ "Item"
      , "Link"
      , "Type"
      ]
```

We only need to implement the `headerOrder` function, which is very
similar to `itemHeader`. Note that we ignore the argument of this
function. If we want to know the default order of fields for a record,
we can check it using `undefined`, as follows:

```
> headerOrder (undefined :: Item)
["Item","Link","Type"]
```

If we have an instance of `DefaultOrdered`, we can use the
`Cassava.encodeDefaultOrderedByName` function to decode a list of
values:

```haskell
encodeDefaultOrderedByName
  :: (ToNamedRecord a, DefaultOrdered a)
  => [a]
  -> ByteString
```

This is almost the same as `Cassava.encodeByName`, but without a
header argument. Let's try encoding the Japan item:

```
> Cassava.encodeDefaultOrderedByName [japanItem]
"Item,Link,Type\r\nJapan,http://www.data.go.jp/,\"International Country\"\r\n"
```

To make it easier to encode items, we can define a wrapper for the
`Cassava.encodeDefaultOrderedByName` function:

```haskell
encodeItems
  :: Vector Item
  -> ByteString
encodeItems =
  Cassava.encodeDefaultOrderedByName . Foldable.toList
```

And we can use this function to define one that first encodes items to
CSV records and then writes the result to a file:

```haskell
encodeItemsToFile
  :: FilePath
  -> Vector Item
  -> IO (Either String ())
encodeItemsToFile filePath =
  catchShowIO . ByteString.writeFile filePath . encodeItems
```

Again, we use the `catchShowIO` function to handle things like not
having write permissions for the given file path.

Let's add one more line to our `Main` module to write the country
items to a file called `countries.csv`:

```haskell
main :: IO ()
main = do
  putStrLn "Open data!"

  eitherCountryItems <-
    fmap filterCountryItems
      <$> decodeItemsFromFile "items.csv"

  case eitherCountryItems of
    Left reason ->
      Exit.die reason

    Right countryItems -> do
      putStr "Number of country items: "
      print (length countryItems)

      Monad.void (encodeItemsToFile "countries.csv" countryItems)
```

Let's run the `main` function again:

```
> main
Open data!
Number of country items: 53
```

The output is the same, but the countries are now in a `countries.csv`
file. Here's the first part of the file:

```
$ head countries.csv
Item,Link,Type
"Argentina ",http://datos.argentina.gob.ar/,"International Country"
Australia,http://data.gov.au/,"International Country"
Austria,http://data.gv.at/,"International Country"
Bahrain,http://www.bahrain.bh/wps/portal/data/,"International Country"
Belgium,http://data.gov.be/,"International Country"
Brazil,http://dados.gov.br/,"International Country"
Canada,http://open.canada.ca/en,"International Country"
Chile,http://datos.gob.cl/,"International Country"
China,http://govinfo.nlc.gov.cn/,"International Country"
```

Finally, we can check that reading the original file and filtering the
countries yields the same result as reading the countries file. Here
are the filtered items:

```
> :{
| eitherCountryItemsA <-
|   fmap filterCountryItems
|     <$> decodeItemsFromFile "items.csv"
| :}
```

And here are the items from the countries file:

```
> eitherCountryItemsB <- decodeItemsFromFile "countries.csv"
```

Let's check that both results are equal:

```
> eitherCountryItemsA == eitherCountryItemsB
True
```

This is the usual workflow when dealing with CSV data in Haskell using
the Cassava library. In some cases, we may need to only encode or
decode data, or we may need different data types for encoding and
decoding, but the process is very similar to the one described in this
tutorial.

[cassava]: http://hackage.haskell.org/package/cassava
[open-gov]: https://www.data.gov/open-gov/
[rfc-4180]: https://tools.ietf.org/html/rfc4180
