---
title: Creating and Updating Golden Test with Hspec-golden
published: 2019-10-31
ghc: 8.0.2
lts: 14.10
tags: haskell, golden, tests
libraries: hspec-golden-0.1.0.1
language: haskell
author-name: Jorge Andrés Guerra Landázuri
twitter-profile: Jagl257
github-profile: Jagl257
description: Software testing is necessary to check if an application behaves in a expected way. Sometimes it is useful to store the expected output in a separate file. This is how golden tests works and in this tutorial we will be using Hspec-golden haskell library to create and update with no problem our golden tests.
---


The importance of software testing relies on the ability to check if a determined application behaves in an appropriate way. Unit tests are the first level of testing and helps developers to detect early bugs in the code by testing each component individually.

For example, lets create a function that outputs the string "Hello Golden Testers":
```haskell
sayHi :: String
sayHi = "Hello Golden Testers"

```

Using Hspec we can easily test our function :

```haskell
import HelloWorld
import Test.Hspec

spec :: Spec
spec = do
    describe "sayHi" $
        it "shows a Hello Golden Testers string" $
            sayHi `shouldBe` "Hello Golden Testers"
```

So, in a nutshell we are comparing the output of the function with a string that is stored inside the code of our test. We can take a different approach and store this output in a separate file. This approach is known as golden testing and the file in which we store the output takes the name of "golden file".

So, now we know what golden testing is, but if unit tests and golden tests are so similar, why don't we just keep up with unit tests right?

The truth is that unit tests are not useful when evaluating complex and large outputs and that's where the difference lies. Here are some advantages of golden tests over unit tests.

 - It wouldn't be practical to store a large output inside the test code.

 - With some golden tests libraries the expected output (Golden File) can be automatically generated .

 - Some golden test libraries also automatically updates the golden file.

 - It is useful when working with JSON, HTML, images, etc.

In this tutorial we will use [hspec-golden](https://github.com/stackbuilders/hspec-golden) library to build some golden-test. We will also use the hspec-golden CLI to easily update our tests.

## Getting Started

First things first, you should create a new Haskell project on which we will be working:

```bash
stack new "hspec-golden-tests"
```

and add the necessary dependencies in order to follow this tutorial without issues.

Project dependencies

``` yaml
dependencies:
- base >= 4.7 && < 5
- aeson
- text
- bytestring
- blaze-html
```

Testing dependencies

``` yaml

dependencies:
- hspec-golden-tests
- hspec
- hspec-golden

```
## A Simple Test

Now that we have all dependencies installed, lets create our golden tests.

First lets create a ```HelloWorld ``` module to be tested and add the "Hello Golden Testers" function described earlier.

```haskell
module HelloWorld where

sayHi :: String
sayHi = "Hello Golden Testers"

```
Now lets check some of the hspec-golden [documentation](http://hackage.haskell.org/package/hspec-golden-0.1.0.1/docs/Test-Hspec-Golden.html) before creating our first test.

```haskell

defaultGolden :: String -> String -> Golden String

```
Hspec-golden provides us a defaultGolden function which creates and compares the SUT output and the golden file. It takes two parameters:

  - the name of the test (Directory where our golden file will be located)
  - the output of our SUT  

By default this function search for golden test inside a /.golden directory, so it would be a great idea if we create one right now.

Ok enough reading, lets create our tests. We will first create a test module named ```HelloGoldenSpec``` . For convenience I have created this module under test/Hello directory.

```Haskell

module Hello.HelloGoldenSpec where

```

We need some imports.

```Haskell

import           Test.Hspec              -- Test
import           Test.Hspec.Golden       -- Golden Tests  
import           HelloWorld              -- SUT

```
Finally lets write the test.

```Haskell

spec :: Spec
spec =
    describe "sayHi" $
    it "returns Hello Golden Testers string" $
    defaultGolden "hello" sayHi

```
We are now ready to test our module.

```shell

user@ubuntu:~/hspec-golden-test$ stack test

hspec-golden-tests> test (suite: hspec-golden-tests-test)


Hello.HelloGolden
  sayHi
    returns Hello Golden Testers string
      First time execution. Golden file created.

Hello.Hello
  sayHi
    shows a Hello Golden Testers string

Finished in 0.0006 seconds
2 examples, 0 failures

hspec-golden-tests> Test suite hspec-golden-tests-test passed

```

We can see that the test is successful. If we check the .golden directory we will find two files.

 - golden : Is where the expected output is located.
 - actual : Stores the real output from the SUT.


```shell

user@ubuntu:~/hspec-golden-test/.golden$ tree
.
└── hello
    ├── actual
    └── golden

1 directory, 2 files

```

The testing framework recognized that this was the first execution therefore created the hello test with the actual and golden files.



## A more complete test

So far we have implemented the hspec-golden defaultGolden tool for testing and automatically creating our golden files. But we haven't really test the whole functionality of hspec-golden nor the hgold CLI from hspec-golden for updating the golden files.

We will now test two haskell modules that really need  golden tests.

- Html.hs : Renders a HTML template.
- Json.hs : Encodes a data type into a JSON Bytestring.  

For convenience each module will be located inside its own directory.

```shell

user@ubuntu:~/hspec-golden-test/src$ tree
.
├── HelloWorld.hs
├── HTML
│   └── Html.hs
├── JSON
│   └── Json.hs
└── Lib.hs

```

To make things easy and save time lets paste the following code into each module.

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

We are ready to start coding our tests. First lets organize our tests into directories. And inside the respective directory create HtmlGoldenSpec.hs and JsonGoldenSpec.hs.

```shell

user@ubuntu:~/hspec-golden-test-tutorial/test$ tree
.
├── Hello
│   ├── HelloGoldenSpec.hs
│   └── HelloSpec.hs
├── Html
│   └── HtmlGoldenSpec.hs
├── Json
│   └── JsonGoldenSpec.hs
└── Spec.hs

```

#### HTML Golden tests

Now that we have our module lets imports all the things we need.

``` haskell

import           Test.Hspec
import           Test.Hspec.Golden
import           HTML.Html

```
And finally we can write our test.

``` haskell

spec :: Spec
spec =
    describe "renderHtml" $
    it "Renders an Html5 file " $
    defaultGolden "html" (htmlRendered somePage)

```

Just like the previous example, after running the test we will have a new directory (/.golden/html/) that contains our actual and golden file. But what would happen if we change the output of our SUT ?

For example let change the html body to "Goodbye Golden Testers" in the Html module and run the tests.   

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

Our html test failed but not big deal we can change our golden file and things will work just fine. Pretty simple right?
Actually its not that simple. Lets imagine that we have a really large golden file and we had to update it by hand, that would be a waste of time. Fortunately hspec-golden provides us with a tool "hgold" that helps updating our golden files using its CLI. Lets install it.

Using stack:

```shell

stack install hspec-golden

```

using Cabal:

```shell

cabal install hspec-golden

```
Once installed we can use the --help flag to see how the CLI works.

```

Update your golden files

Usage: hgold [-u|--update [DIR]] [-v|--version]

Available options:
  -u,--update [DIR]        The testing directory where youre dumping your
                           results. (default: )
  -v,--version             Show version
  -h,--help                Show this help text


```

According to [hspec-golden](https://github.com/stackbuilders/hspec-golden) when hgold is used without flags it updates on default the "./golden" directory. If we store our golden files in a different dierctory we should use the --update flag and specify the name of the directory as an argument.

Ok, lets update our golden files. When using hgold the golden files (Expected output) will be replaced with the actual files (Output).

```shell

user@ubuntu:~/hspec-golden-test$ hgold
Replacing golden with actual...
  Replacing file: .golden/hello/golden with: .golden/hello/actual
  Replacing file: .golden/html/golden with: .golden/html/actual
Finish...

```

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

And we can see that everything works fine.

#### JSON Golden Test

Until now we have managed components that returns strings only, but what if our return type is different like an Integer or Boolean? For example lets analize our Json.hs module encodeCountries function.

```Haskell

encodeCountries :: [Country] -> ByteString
encodeCountries = encode

```

This function returns a bytestring, that means that if we try to assert the output with our defaultGolden function,

``` Haskell

  describe "encodeCountries" $ do
   it "encodes a group of Countries into a JSON String " $
    defaultGolden "json" (encodeCountries countries)

```

test wont even compile.

```shell
• Couldn't match type ‘B.ByteString’ with ‘[Char]’
      Expected type: String
        Actual type: B.ByteString
```

Conviniently hspec-golden export a Golden data type. With this tool we can configure how our golden test works.

```
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

We are now able to create a new function to assert our output. Lets start by importing some modules.

``` Haskell
module Json.JsonGoldenSpec where

import           Test.Hspec
import           Test.Hspec.Golden
import           JSON.Json
import qualified Data.ByteString.Lazy as B    

```

Next, based on the Golden data type documentation , lets create our assert function.

```Haskell

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

If we check our goldenBytestring function, we have specified a new directory to store our golden files,so we will have to create this one before running our tests.

```shell

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

We are now ready to create our test.

```Haskell

spec :: Spec
spec =

  describe "encodeCountries" $ do
   it "encodes a group of Countries into a JSON bytestring " $
    goldenBytestring "json" (encodeCountries countries)

```
And run them to see if our assert function is working fine.

```shell
  encodeCountries
    encodes a group of Countries into a JSON bytestring 
      First time execution. Golden file created.

Finished in 0.0034 seconds
4 examples, 0 failures

```

Lets remember that our golden files are stored in a different directory so in case of making an update we should use the --update flag from out hgold CLI.

```shell

user@ubuntu:~/hspec-golden-test$ hgold --update ".otherGolden"

Replacing golden with actual...
  Replacing file: .otherGolden/json/golden with: .otherGolden/json/actual
Finish...

```


