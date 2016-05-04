---
title: CSV encoding and decoding in Haskell with Cassava
ghc: 7.10.3
resolver: lts-5.15
lts: 5.15
library: cassava-0.4.5
language: haskell
published: 2016-05-15
updated: 2016-05-15
author: Juan Pedro Villa Isaza (@jpvillaisaza)
---

## overview

what is the problem to solve? csv encoding and decoding. we'll use an
example with twitter's features for generating csv files and then
compose one file.

approach? we'll use the standard haskell library for this, which is
cassava. as of writing, version something. it has good basic
documentation, but it's good to see a complete example.

[cassava][cassava]

## actual tutorial

[RFC][rfc-4180]

```
aaa,bbb,ccc
zzz,yyy,xxx
```

```
field_name,field_name,field_name
aaa,bbb,ccc
zzz,yyy,xxx
```

Open Data Internationally, [open-gov][open-gov]

```
Item,Link,Type
```

```
...
Albuquerque,http://www.cabq.gov/abq-data/,US City or County
...
United Nations,http://data.un.org/,International Regional
Uruguay,http://datos.gub.uy/,International Country
Utah,http://www.utah.gov/data/,US State
...
```

topics covered by documentation:

encoding/decoding standard haskell types, like strings, probably not
what you're going to do

encoding/decoding custom data types. ToRecord or ToNamedRecord and
FromRecord or FromNamedRecord, derived or manually defined

Named based with ...NamedRecord

```haskell
instance FromNamedRecord Item where
  parseNamedRecord =
    undefined
```

Treating data as opaque bytestrings for things like removing the
second row

first is decoding, we have a csv file and we want to parse it to then
update a database for instance. So we can create our custom data type
and appropriate instances. I guess we can assume that we always will
have headers.

then we can encode. For instance, based on several files of the same
type that we decoded, we can compute results for all of them and build
a new CSV with the data we want.

An idea. If we have the tweets files for a user, we can compute the
amount of tweets for 2016, then we can count the number of tweets for
2016 for a different user, and at the end compute the number of tweets
for 2016 for all those people sorted by total number of tweets.

Another idea. The US government has an open data website where you can
download open data in CSV format. This is not limited to the US, they
also include a list of countries and regions that have open data
websites. There are a lot of files there, but we can simply use the
one for world open data and parse it, and then maybe create a new CSV
file only with countries or only South American countries or something
like that. This format is not very complicated, so it's good enough.

## Exercises

Probably better if the exercises are earlier.

## Summary

A summary of the tutorial or maybe short definitions.

## Follow-up resources

Follow-up resources or where to next, or Related tutorials

[cassava]: http://hackage.haskell.org/package/cassava

[rfc-4180]: https://tools.ietf.org/html/rfc4180

[open-gov]: https://www.data.gov/open-gov/
