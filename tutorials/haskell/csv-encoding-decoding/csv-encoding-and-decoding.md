---
title: ...
sd: sd
ghc: ghc-7.10.3
resolver: lts-5.15
library: cassava-0.4.5
language: haskell

published: 2016-05-15 (first published)
revision: 2016-05-15 (substantive revision)

repository: ...

author (maintainer): Juan Pedro Villa Isaza (@jpvillaisaza)

---

# overview

what is the problem to solve? csv encoding and decoding. we'll use an
example with twitter's features for generating csv files and then
compose one file.

approach? we'll use the standard haskell library for this, which is
cassava. as of writing, version something. it has good basic
documentation, but it's good to see a complete example.

topics covered by documentation:

encoding/decoding standard haskell types, like strings, probably not
what you're going to do

encoding/decoding custom data types. ToRecord or ToNamedRecord and
FromRecord or FromNamedRecord, derived or manually defined

Named based with ...NamedRecord

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

# bibliography
# academic tools
# other internet resources
# related entries

follow up resources or where to next
