---
title: Golden testing in Haskell with hspec-golden
published: 2019-08-10
tags: haskell, golden testing, snapshot testing, expect testing
libraries: hspec-golen-0.1.0.0
language: haskell
author: Adrian Vera
author-name: Adrian Vera
twitter-profile: adrian_med
github-profile: medAdrian
description: In this tutorial we will implement golden tests in Haskell using hspec-golden a super set of the known test framework hspec. A golden test is an Input/Output action that records the result of a test into a file known as the "Golden File".
---

Golden testing (also known as Golden master testing or Snapshot testing) is pretty similar to unit testing, except that the test output is stored in a separate file known as the "Golden File".

> ‘A Golden File is your source of truth. You validate your test response against the Golden File. To summarize, the Golden file contains the response you expect from your test’

Source: [Medium](https://medium.com/@jarifibrahim/golden-files-why-you-should-use-them-47087ec994bf) 

This kind of testing will help us verify modifications made to a reference version of our SUT(Subject Under Test) and validate that it has not modified its
behaviour in an incorrect way. Thus giving us some safety when extending and refactoring code that doesn't have proper tests.

When we write golden tests we build up knowledge of what the code is doing, this is very useful when we want to re-write it,
we can run our test to find out immediately whether we've modified the desired behaviour (we start to see our tests in a different way
since the purpose of golden testing is to document your system's behaviour, not to check for the wished one).

Once we adopt this perspective, we have a completely different frame for understanding automated tests in general,
we can see tests as descriptions of what we have rather than statements of correctness.
We can review our tests periodically to tighten up their conditions as we decide what the behavior should be at each level of our systems.

## Changes on test flow
The workflow for running the tests now becomes the following:

* Write some execution for the subject under test, this can be a small test wich will pass as at the first execution time.
* Change the SUT(Subject Under Test) and evaluate the result of the golden file.
* Update the golden file with the new value output from the Subject Under Test.

## How do Golden tests differentiate from unit tests?
* If the output is larger in size, it's not practical to put it in the source code.

* There is no need to escape quotes or binary data in the output.

* When you add a new test, your testing framework can generate the missing golden file from the current output of the function.

* It's best if you can write down the expected output without looking at the actual output, but it is not always possible.

* The expected output can be automatically generated/updated.

* You can tell your test framework to update all golden files from the current outputs, then check git diff to ensure that all changes are valid, and commit.

* If some of your tests suddenly started failing, you can use diff or other such tools to compare the golden file to the actual file and figure out what exactly changed.

## What is hspec-golden?
[hspec-golden](https://github.com/stackbuilders/hspec-golden) is a superset for the popular Haskell B.D.D. testing framework [hspec](https://github.com/hspec/hspec),
that will allow us to create golden test for our Haskell projects,
currently being developed by [Cristhian Motoche](https://github.com/CristhianMotoche) an active Open Source code enthusiast and Stackbuilders developer.

## What do we need?
For this tutorial, we need an environment running Haskell with base 4.6 up to 5,
if we don’t have a Haskell environment we could refer to this [link](https://wiki.haskell.org/Haskell_in_5_steps#Install_Haskell) to get a clean installation using `stack` or `cabal`.

We also need to have added as a dependency to our Haskell project the [hspec](https://github.com/hspec/hspec) testing framework.

With stack:

``$ stack install hspec``

With cabal:

``$ cabal update && cabal install hspec``

## Getting started
This code sample will help us to understand how does basic golden testing works.

## Installing the library:
To install the [hspec-golden](https://github.com/stackbuilders/hspec-golden) superset we need to run this command on the console:

``$ stack install hspec-golden``

## Importing the libraries
In our project on we have created a GoldenSpec.hs and added the following lines.
``` 
module GoldenSpec where

import Test.Hspec
import Test.Hspec.Golden
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Yaml as Y
import Data.Aeson as A
import Control.Exception (evaluate)
```

Here we are telling Haskell to add the libraries that are going to be implemented in this tutorial,
we are adding the test framework [hspec](https://github.com/hspec/hspec) and [hspec-golden](https://github.com/stackbuilders/hspec-golden),
with some additional libs to implement our own Golden type.


### Writing our first golden test
The [hspec-golden](https://github.com/stackbuilders/hspec-golden) library provides us with an easy example to test the functionality of the library with the `defaultGolden` helper:

``` 
describe "id" $
     it "generates the right output with the right params" $
        let output = show $ id (1:: Int)
        in defaultGolden "id" output
```

On this test we are setting up a B.D.D. test with [hspec](https://github.com/hspec/hspec) “describe” and “it”,
but on the inside there is an implementation of the `defaultGolden` helper,
that will create(or update) for us a directory called `.golden` this will contain another directory named after our described test in this case `id` with two files containing the `actual` and `golden` values for our test.

For our example we will create our own Golden type.
We have created a file called test.yml that contains an ansible role on the dir ["test/fixtures/"](https://github.com/MedAdrian/hspec-golden-tutorial/blob/master/test/fixtures/test.yml) 
we will use this file contents and transform them into JSON.

```
{--
Here we define our Golden test as follows:
- We read a .yml file that contains an ansible role
- We decode the string read by Haskell that we read form the .yml file
--}

spec :: Spec
spec = do
    describe "conversion" $ do
      content <- runIO $ B.readFile "test/fixtures/test.yml"
      let eYaml = Y.decodeEither' content :: Either Y.ParseException Y.Value  --Parses to a generic yml type.
          Right json = A.encode <$> eYaml                                     --Parses the data type to a json structure.
      it "transforms a YAML file contents to JSON" $ do
        myGoldenTest "conversion" json                                        --Generates the golden test.
```

To see the full code example access this [repository](https://github.com/MedAdrian/hspec-golden-tutorial).

##### I.e: Directory Structure 
``` 
|_.golden
   |_id
     |_actual
     |_golden
```

For this example we are testing that the output from the function will return the expected golden value.

## Installing the CLI
You can install the [hspec-golden](https://github.com/stackbuilders/hspec-golden) command line interface (CLI) with stack:

``` $ stack install hspec-golden ```

or cabal:

``` $ cabal install hspec-golden ```

Update the golden tests under .golden directory:

``` $ hgold -u ```

Update the golden tests under .myGoldenTest directory:

``` $ hgold -u .myGoldenTest ```

## Conclusions
We can determine that the name “golden file” only refers to the file with the output value, not the input.
On most libraries there is no input values stored in a file,
but in practice is often convenient to store them both in files so that there is an input file for every output file and vice versa.

A golden test can fail when the actual output value does not match the contents of the golden file.
You then need to determine whether this is an intentional code change or a bug.

Using git we can determine whether the change made is the expected one and commit it.  
This workflow lets you use all the powerful ``git diff`` options like ``--color-words``,
or even launch a graphical ``diff tool`` like ``kdiff3`` with git ``difftool``.