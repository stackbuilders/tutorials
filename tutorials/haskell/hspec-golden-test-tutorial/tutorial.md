---
title: Golden Testing with Hspec-golden
published: 2019-10-31
ghc: 8.0.2
lts: 14.10
tags: haskell, golden, tests
libraries: hspec-golden-0.1.0.1
language: haskell
author-name: Jorge Andrés Guerra Landázuri
twitter-profile: Jorge_257
github-profile: Jagl257
description: Software testing is necessary to check if an application behaves in an expected way. Sometimes it is useful to store the expected output in a separate file. This is how golden tests work and in this tutorial we will be using the Hspec-golden haskell library to easily create and update our golden tests.
---


The value of software testing relies on the ability to check if an application behaves properly. Unit tests are the first layer of testing and help developers detect bugs early in the code by testing each component individually.

For example, let's create a function that welcomes someone to this tutorial:

```haskell
sayHi :: String -> String
sayHi name = "Welcome to the Golden Tests tutorial " ++ name
```

Using `hspec` we can easily test our function :

```haskell
import HelloWorld
import Test.Hspec

spec :: Spec
spec = do
    describe "sayHi" $ 
        it "shows a Hello Golden Testers string" $
            (sayHi "John Doe") `shouldBe` "Welcome to the Golden Tests tutorial John Doe"
```

In a nutshell, we are comparing the output of the function with a string that is stored inside the body of our test. We can take a different approach and store this expected output in a separate file. This approach is known as golden testing and the file in which we store the expected output takes the name of "golden file".

So, now we know what golden testing is, but if unit tests and golden tests are so similar, why don't we just keep up with unit tests? Right?


The truth is that unit tests are not useful when evaluating large, complex outputs and that's where the difference lies. Tests are all about maintainability and readability, and when we test large outputs we tend to add visual noise to our code. Golden Tests helpfully fix this issue. Let's check the main advantages:

 - Large outputs are stored in a golden file reducing the visual noise in our code.

 - With some golden tests libraries the Golden Files can be automatically generated.

 - Some golden test libraries also automatically update the golden file. So we don't need to manually update the tests when the expected output changes. 

 - It is useful when working with JSON, HTML, images, etc.

In this tutorial we will use [hspec-golden](http://hackage.haskell.org/package/hspec-golden) library to build golden tests. We will also use the `hspec-golden` CLI to easily update them.

You can either code along with this tutorial or check out the finished [code](https://github.com/stackbuilders/tutorials/tree/tutorials/tutorials/haskell/hspec-golden-test-tutorial/code) in Github. 

## Hspec-golden

`Hspec-golden` is a StackBuilders testing library written in Haskell, that helps users implement golden tests. It is an open source project so feel free to [review](https://github.com/stackbuilders/hspec-golden) it.

## Getting Started

First things first, let's create a new Haskell project to work in:

```bash
stack new "hspec-golden-tests-tutorial"
```

and add the necessary dependencies in order to follow this tutorial without issues.

Project dependencies:

```yaml
dependencies:
- base >= 4.7 && < 5
- aeson
- text
- bytestring
- blaze-html
```

Testing dependencies:

```yaml
dependencies:
- hspec-golden-tests
- hspec
- hspec-golden

```
## A Simple Test

Now that we have all of the dependencies installed, let's get started with the golden tests.

First let's create a `FizzBuzz ` module to be tested and add some code. For convenience, I have created it inside a `FIZZBUZZ` directory. 

```haskell
module FIZZBUZZ.FizzBuzz where 

fizzBuzz :: [Int] -> [String]
fizzBuzz list = map fizzOrBuzz list

fizzOrBuzz ::Int -> String
fizzOrBuzz n | n `mod` 15 == 0  = "FizzBuzz"
             | n `mod` 3  == 0  = "Fizz"
             | n `mod` 5  == 0  = "Buzz"
             | otherwise        = show n

```
For simplicity we will test a simple function `fizzbuzz` that replaces multiples of 3 with Fizz and multiples of 5 with Buzz. In case that a number is a multiple of both it replaces the number with FizzBuzz. Before moving on with testing, let's review some of `hspec-golden` [documentation](http://hackage.haskell.org/package/hspec-golden-0.1.0.1/docs/Test-Hspec-Golden.html).

Hspec-golden provides us with a `defaultGolden` function which creates the golden files and compares it with the Subject Under Test (SUT) output. It takes two parameters:

  - the **name** of the test 
  - the output of our SUT 

**Note** : _It is important to give the tests unique names otherwise `hspec-golden` won't work_.

```haskell
defaultGolden :: String -> String -> Golden String
```

By default this function searches for golden tests inside a `.golden/` directory. This directory is created automatically when the test runs.

Okay, enough reading, let's create our tests. Let's first create a test module named `FizzBuzzGoldenSpec` under `test/FizzBuzz/` directory.

```haskell
module FizzBuzz.FizzBuzzGoldenSpec where
```

We need some imports.

```haskell
import           Test.Hspec                   --Tests
import           Test.Hspec.Golden            --Golden Tests
import           FIZZBUZ.FizzBuzz             -- SUT
```
Finally let's write the test.

```haskell
spec :: Spec
spec = 
    describe "fizzBuzz" $
      it "Turns 3 multiples to fizz and 5 multiples to buzz" $
        defaultGolden "fizzbuzz" (show $ fizzBuzz [1,2,3,4,5,11,12,13,14,15])
```
We are now ready to test our module.

```shell
$ stack test

hspec-golden-tests> test (suite: hspec-golden-tests-test)


FizzBuzz.FizzBuzzGolden
  fizzBuzz
    Turns 3 multiples to fizz and 5 multiples to buzz
      First time execution. Golden file created.

Hello.Hello
  sayHi
    shows a Hello Golden Testers string

Finished in 0.0006 seconds
2 examples, 0 failures

hspec-golden-tests> Test suite hspec-golden-tests-test passed
```

The test is successful. If we check the ".golden" directory we will find two files.

 - golden : Stores the expected output.
 - actual : Stores the real output from the SUT.



```shell
$ tree
.golden
 └── fizzbuzz
     ├── actual
     └── golden
```

The testing framework recognized that this was the first execution, therefore created the fizzbuzz test with the `actual` and `golden` files. The difference between these two files is that the `golden` file will stay the same unless we want to update it while the `actual` file will be overwritten every time we run the test. This is useful when updating the tests, but we will see that later on in this tutorial. 


## A More Real Test Case

The `fizzbuzz` module was used to create a simple and didactic example but it could have been easily tested with unit tests. In practice golden tests are often used when testing JSON, HTML or images which generate large outputs. We also haven't yet tested the entire functionality of the `hspec-golden` library yet. For example the `hgold` CLI for updating the golden files.

Let's create some new modules to demonstrate a real case in which golden tests are needed.

- Html.hs : Renders a HTML template.
- Json.hs : Encodes a data type into a JSON Bytestring.  

For convenience, each module will be placed inside its own directory.

```shell
$ tree src
src
├── FIZZBUZZ
│   └── FizzBuzz.hs
├── HelloWorld.hs
├── HTML
│   └── Html.hs
└── JSON
    └── Json.hs

```

To make things easy and save time, let's add the following code into each module.

###### Html.hs

```haskell
{-# LANGUAGE OverloadedStrings #-}


module HTML.Html  where

import           Text.Blaze.Html5               as H
import           Text.Blaze.Html5.Attributes    as A
import           Text.Blaze.Html.Renderer.Pretty        (renderHtml)


htmlRendered :: Html -> String
htmlRendered page = renderHtml page

somePage :: Html
somePage = html $ do
    H.head $ do
        H.title "StackBuilders Tutorial."
    body $ do
       "Hello Golden Testers."
```


###### Json.hs

```haskell
{-# LANGUAGE DeriveGeneric #-}

module JSON.Json where

import            Data.Aeson                   (ToJSON, encode)
import            GHC.Generics                 (Generic)
import            Data.ByteString.Lazy         (ByteString)

data Country = Country
  {
    cname      :: String,
    continent :: String,
    ctag       :: Int
  } deriving (Generic, Show)

instance ToJSON Country


ecuador = Country "Ecuador" "America" 1
germany = Country "Germany" "Europe" 2
japan = Country "Japan" "Asia" 3

countries :: [Country]
countries = [ecuador,germany,japan]

encodeCountries :: [Country] -> ByteString
encodeCountries = encode
```

Before coding our tests, let's create and organize `HtmlGoldenSpec.hs` and `JsonGoldenSpec.hs` modules into directories as well.

```shell
$ tree tests
test
├── FizzBuzz
│   └── FizzBuzzGoldenSpec.hs
├── Hello
│   ├── HelloGoldenSpec.hs
│   └── HelloSpec.hs
├── Html
│   └── HtmlGoldenSpec.hs
├── Json
│   └── JsonGoldenSpec.hs
└── Spec.hs
```
We are now ready to start coding our golden tests.

#### HTML Golden tests

Let's start with the HTML module importing all the things we need:

```haskell
import           Test.Hspec
import           Test.Hspec.Golden
import           HTML.Html
```
and writing our test.

```haskell
spec :: Spec
spec =
    describe "renderHtml" $
      it "Renders an Html5 file " $
       defaultGolden "html" (htmlRendered somePage)
```

Just like the previous example, after running the test we will have a new directory `.golden/html/` that contains our actual and golden file. But what happens if we would have to change our HTML template?

In practice changes may include several lines and tags but to make a simple example, let's edit the body of our HTML to "Goodbye Golden Testers" and run the tests.    

```shell
test/Html/HtmlGoldenSpec.hs:11:5:
  1) Html.HtmlGolden.renderHtml Renders an Html5 file
      expected: "   
       <body>
               Hello Golden Testers.
       </body>"
      but got: "
       <body>
               Goodbye Golden Testers.
       </body>"


Finished in 0.0013 seconds
3 examples, 1 failure
```

Our HTML test failed but not big deal, we can edit our `golden` file and things will work just fine. Pretty simple right?

Actually it's not. Let's remember this is just a didactic example. In real life golden files store
really large outputs and it would be a waste of time to update them manually. Fortunately, `hspec-golden` provides us with the `hgold` CLI tool that automatically updates golden files. Let's install it.

Using stack:

```shell
$stack install hspec-golden
```

using Cabal:

```shell
$cabal install hspec-golden
```
Once installed we can use the `--help` flag to see how the CLI works.

According to [hspec-golden](http://hackage.haskell.org/package/hspec-golden), when `hgold` is used without flags it updates the `.golden/` directory by default. If we store our golden files in a different directory we should use the `--update` flag and specify the name of the directory as an argument.

Okay, let's update our golden files. 

```shell
$ hgold
Replacing golden with actual...
  Replacing file: .golden/hello/golden with: .golden/hello/actual
  Replacing file: .golden/html/golden with: .golden/html/actual
Finish...
```

Hgold replaces the `golden` file (expected output) with the content of the `actual` file (SUT output) so it is necessary to run the test first (even if they fail) to update our tests.

So now we are ready to test our updated golden files.

```shell 
Hello.HelloGolden
  sayHi
    returns Hello Golden Testers string
      Golden and Actual output hasn't changed
Hello.Hello
  sayHi
    shows a Hello Golden Testers string
Html.HtmlGolden
  renderHtml
    Renders an Html5 file 
      Golden and Actual output hasn't changed

Finished in 0.0034 seconds
3 examples, 0 failures
```

And we can see, everything works fine.

#### JSON Golden Test

Until now, we have managed components that return only strings, but what if our return type is different like a `ByteString` or `Text`? For example, let's analyze the `encodeCountries` function in the `Json.hs` module.

```haskell
encodeCountries :: [Country] -> ByteString
encodeCountries = encode
```

This function returns a ByteString. That means that if we try to assert the output with our `defaultGolden` function,

```haskell
  describe "encodeCountries" $ do
   it "encodes a group of Countries into a JSON String " $
    defaultGolden "json" (encodeCountries countries)
```

tests won't even compile.

```shell
$stack test
• Couldn't match type ‘B.ByteString’ with ‘[Char]’
      Expected type: String
        Actual type: B.ByteString
```

Conveniently, `hspec-golden` exports a `Golden` data type. With this tool, we can configure our Golden test by specifying the functions `hspec-golden` should use for our test to work.

```haskell
data Golden str =
  Golden {
    output       :: str,                      --  Output
    encodePretty :: str -> String,            --  Makes the comparison pretty when the test fails
    writeToFile  :: FilePath -> str -> IO (), --  How to write into the golden file the file
    readFromFile :: FilePath -> IO str,       --  How to read the file,
    testName     :: String,                   --  Test name 
    directory    :: FilePath                  --  Directory where you write your tests
  }
```
For example, we can define a different directory for storing our golden files or how our test will be read and how the output should be written. Also, the `encodePretty` characteristic determines how the prompt should print a readable output when tests fail. With this data-type we are now able to create a new function to assert our output. Let's start by importing some modules.

```haskell
module Json.JsonGoldenSpec where

import           Test.Hspec
import           Test.Hspec.Golden
import           JSON.Json
import qualified Data.ByteString.Lazy as B    
```

Next, based on the `Golden` data type documentation , let's create our assert function.

```haskell
goldenBytestring :: String -> B.ByteString -> Golden B.ByteString
goldenBytestring name actualOutput =
    Golden {
        output = actualOutput,
        encodePretty = show,
        writeToFile = B.writeFile,
        readFromFile = B.readFile,
        testName = name,
        directory = ".otherGolden"
          
    }
```

Let's analyze a little bit our `goldenBytestring` function: 

 - Its type is `Golden ByteString`.
 - To print our failed tests it makes use of haskell's `show` function. 
 - To write the bytestring into a file, as well as reading it,  we have used the `writeFile` and `readFile` functions from the Data.ByteString.Lazy module.
 - We will be saving the output into a different directory called `.otherGolden/`. 

```shell
$ tree 
├── .golden
│   ├── hello
│   │   ├── actual
│   │   └── golden
│   └── html
│       ├── actual
│       └── golden
|
└── .otherGolden
```

We are now ready to create our test,

```haskell
spec :: Spec
spec =

  describe "encodeCountries" $ do
   it "encodes a group of Countries into a JSON bytestring " $
    goldenBytestring "json" (encodeCountries countries)
```
and run it to see if our assert function is working properly.

```shell
  encodeCountries
    encodes a group of Countries into a JSON bytestring 
      First time execution. Golden file created.

Finished in 0.0034 seconds
4 examples, 0 failures
```

Everything works fine.

Let's remember that we stored our golden files in a different directory. So in the case that we make an update we should use the `hgold` CLI with the `--update` flag, followed by the name of our golden tests directory; in this case `.otherGolden/`.

```shell
$ hgold --update ".otherGolden"

Replacing golden with actual...
  Replacing file: .otherGolden/json/golden with: .otherGolden/json/actual
Finish...
```

## Final Words


Golden testing provides us with a practical way to store and test large outputs generated by our components. For example, testing an API that sends huge amounts of data or testing our HTML templates in the front-end. In Haskell, the `hspec-golden` testing tool, makes it easy to implement golden tests in a project. This tool provides a module capable of asserting the outputs from our SUT with our golden files as well as creating them in case they don't exist. It is also useful when updating our tests. With its CLI, we can replace old golden files with the new output of our SUT.

Using this tool, Haskell developers will be able to start using golden tests within applications with very little effort.