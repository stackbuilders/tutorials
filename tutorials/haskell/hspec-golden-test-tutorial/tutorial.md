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
## Our First Test

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

By default this function search for golden test inside a .golden directory, so it would be a great idea if we create one right now.

Ok nough reading, lets create our tests. First create a test module named ```HelloGoldenSpec``` . For convenience I have create the this module under test/Hello directory.

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

We can see that the test is successful. Now lets check .golden directory.


```shell

user@ubuntu:~/hspec-golden-test/.golden$ tree
.
└── hello
    ├── actual
    └── golden

1 directory, 2 files

```

The testing framework recognized that this was the first execution therefore created the hello test with the actual and golden files.
