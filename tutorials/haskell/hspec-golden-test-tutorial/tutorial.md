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
```
sayHi :: String
sayHi = "Hello Golden Testers"

```

Using Hspec we can easily test our function :

```
import HelloWorld
import Test.Hspec

spec :: Spec
spec = do
    describe "sayHi" $
        it "shows a Hello Golden Testers string" $
            sayHi `shouldBe` "Hello Golden Testers"
```

So, in a nutshell we are comparing the output of the function with a string that is stored inside the code of our test. We can take a different approach and store this output in a separate file. This approach is known as golden testing and the file in which we store the output takes the name of "golden file".

So, now we know what golden testing is. But if unit tests and golden tests are so similar, why don't we just keep up with unit tests right?

The truth is that unit tests are not useful when evaluating complex and large outputs and that's where the difference lies. Here are some advantages of golden tests over unit tests.

 - It wouldn't be practical to store a large output inside the test code.

 - With some golden tests libraries the expected output (Golden File) can be automatically generated .

 - Some golden test libraries also updates automatically the golden file.


# How does Hspec-golden
