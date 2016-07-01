---
title: Behavior-driven development (BDD) in Haskell with Hspec
published: 2016-06-14
ghc: 7.10.3
lts: 5.18
libraries: hspec-2.2.3 QuickCheck-2.8.1
language: haskell
author-name: Juan Carlos Pazmiño
description: Description.
---

> “Do the simplest thing that could possibly work” - Kent Beck

### But why BDD?

Test Driven Development (TDD) is an iterative approach of software development that
promotes a very simple idea: test before you code. It encourages developers
to write tests first, and then write the minimum necessary amount of code for the
tests to pass, repeating the process again, in small incremental iterations.

Behavior Driven Development (BDD) goes a step further by describing not tests
but the behavior a piece of code should show to an outside observer. It provides
semantics shifted towards specification rather than testing.

Haskell offers a strong type system that guarantees code correctness. So, why
would you ever bother to use BDD in your Haskell projects? Although Haskell
does help us with this, when it comes to ensure that the code does what's
expected, you're the only one responsible and BDD comes in handy.
Common uses of BDD include preventing division by zero cases, or ensuring your own
instances of monoids, functors, monads, etc., follow the expected rules.

### Hspec

[Hspec][hspec] is a Haskell library that provides an embedded domain specific
language (EDSL) for defining BDD specs. A spec is organized in a tree structure
defined in terms of `describe` and `it`.

The `describe` clause shows the name of the function or feature whose
behavior we are going to specify. This clause can contain multiple
`it` clauses that show a textual description of the expected behavior.

And inside the `it` clauses, we place the expectations. Expectations are
implementations of the expectancy of a certain behavior, and commonly
use the word `should`. Examples of this expectation functions are `shouldBe`,
`shouldSatisfy`, etc.

### The Luhn algorithm

While trying to choose the "perfect" sample for a BDD tutorial, I got suddenly
inspired by one of the first exercises I saw in the [CIS 194][cis-194] course.
Personally, I consider it a great Haskell introduction material.

Luhn is an algorithm used to validate credit card numbers. The algorithm is
very simple in its conception. Thus, it's a great option to watch BDD
in action.

Given an integer number, the Luhn algorithm follows these steps:

* Double every second digit beginning from the right. That is, the last digit
is unchanged; the second-to-last digit is doubled; the third-to-last digit is
unchanged; and so on. For example, `[1,3,8,6]` becomes `[2,3,16,6]`.
* Add the digits of the doubled values and the undoubled digits from the
original number. For example, `[2,3,16,6]` becomes `2+3+1+6+6 = 18`.
* Calculate the remainder when the sum is divided by 10. For the above example,
the remainder would be 8. If the result equals 0, then the number is valid.

### OK, let's do this

The fastest way to start an Hspec-enabled project is by creating a new
[Stack][stack] project using the hspec template:

```
$ stack new luhn hspec
$ cd luhn
```

This command will create a new folder with the necessary files and configuration
to create Hspec specifications. The important bits are:

* A `luhn.cabal` file with a test-suite configuration which includes, amongst others,
`hspec` and `QuickCheck` as build dependencies. `QuickCheck` is a library that allows
creating property tests in Haskell, but more on that later:

```
test-suite luhn-test
  type:                exitcode-stdio-1.0
  hs-source-dirs:      test
  main-is:             Spec.hs
  build-depends:       base
                     , luhn
                     , hspec
                     , QuickCheck
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
  default-language:    Haskell2010
```

* A `src` folder with a sample module `Data.String.Strip`.
* A `test` folder with an `Spec.hs` test driver file, containing configuration
options that allow Hspec to find your specs without the need to include
them manually. You shouldn't change this file:

```haskell
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
```

The project has all it needs to run the specs, out of the box. So, why don't you
try the following command and see what happens:

```
$ stack build --test
```

Hspec's output will give you some useful information like the number of specs,
the described features and what is expected from them in color coded text
(green for passing specs and red for failing ones), and a small summary.

You can take a look at the project structure and sample code if you want. For the
purpose of this tutorial there are some files we don't need anymore, so we'll start by
removing the `src/Data/` and `test/Data` folders completely.

This tutorial relies only on Hspec tests. We are not going to create an executable
application so we can also remove the `app` folder.

At the `luhn.cabal` file:

* Remove the `executable luhn-exe` section entirely.
* In the library section, remove `Data.String.Strip` module and add the `Luhn` and
`Luhn.Internal` modules (we'll create these modules next). The `library` section
should end up looking like this:

```
library
  hs-source-dirs:      src
  exposed-modules:     Luhn
                     , Luhn.Internal
  build-depends:       base >= 4.7 && < 5
  default-language:    Haskell2010
```

Lastly we'll add some shell content for the project to compile:

```
$ echo "module Luhn where" > src/Luhn.hs
$ mkdir src/Luhn
$ echo "module Luhn.Internal where" > src/Luhn/Internal.hs
```

Run the specs again and everything should be green. We should have 0 examples now.

### Building the foundation

When programming the BDD way, you start writing the specs first. This is not only
a good practice, it also helps to reason about your code as a series of individual
decoupled units.

For the Luhn algorithm, we'll need a `validate :: Integer -> Bool` function
that receives a number and determines if it is valid or not. Based on the Luhn
algorithm description, we can foresee the need of the following helper functions:

* `toDigits :: Integer -> [Integer]` to convert an integer number to a
list of digits.
* `doubleEveryOther :: [Integer] -> [Integer]` to double every other digit
starting from the right.
* `sumDigits :: [Integer] -> Integer` to sum all digits, ensuring doubled
digits sum its own digits
first.

As good practice, some developers split their code into two source files.
One of the modules will contain the function we want exposed publicly to the
whole project. The other one (commonly suffixed as `.Internal`) will hold
the helper functions.

You should avoid importing the Internal modules in your application,
but having this file around is useful whenever you want to import them in order
to test its functions.

So we are going to edit our `src/Luhn.hs` module file and create a stub function.
The file should look like this:

```haskell
module Luhn
  ( validate
  )
  where

import Luhn.Internal

validate :: Integer -> Bool
validate = undefined
```

Then, we edit our `src/Luhn/Internal.hs` module file and insert the following code:

```haskell
module Luhn.Internal
  ( toDigits
  , doubleEveryOther
  , sumDigits
  )
  where

toDigits :: Integer -> [Integer]
toDigits = undefined

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = undefined

sumDigits :: [Integer] -> Integer
sumDigits = undefined
```

Even if we're supposed to start with the spec, having these undefined definitions at
hand will help us compile the spec and run them.

### Our first spec
Let's create the `test/LuhnSpec.hs` file inside the `test` folder. The name of the
folder is important here. In order for Hspec to find our spec files
you should follow certain rules:

1. Spec files have to be placed into the same directory as the test
driver `Spec.hs` file, or into a subdirectory.
2. The name of a spec file has to end in `Spec.hs`; the module
name has to match the file name.
3. Each spec file has to export a top-level binding “spec” with type
`Spec`.

Now, let's add a little bit of boilerplate code, starting with our imports:

```haskell
module LuhnSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Luhn
import Luhn.Internal
```

We import our spec targets `Luhn` and `Luhn.Internal`.

Now, we create our `main` function that simply calls `hspec` sending a `Spec`
value as parameter:

```haskell
main :: IO ()
main = hspec spec
```

Finally, we create our `Spec` object with the first spec. We need to
ensure `toDigits` behaves as expected:

```haskell
spec :: Spec
spec = do
  describe "toDigits" $ do
    it "converts a number to a list of digits" $ do
      toDigits 1234567 `shouldBe` [1,2,3,4,5,6,7]
```

We'll destructure this code a bit:

* Our `spec` contains one or many `describe` clauses.
* Each `describe` clause receives a `String` with the name of the function or
feature we are going to specify. This clause can contain one or more `it` clauses.
* Each `it` clause receives a `String` describing the expected behavior
for the function or feature. This clause can contain one or more expectations.
* We are using the basic `shouldBe` expectation here, which simply expects its two
operands to be equal. We'll use a couple more later.

The current structure lets us read our spec as "toDigits, converts a number to
a list of digits". This is, at least, the behavior we expect to see.

If we run the spec now, it should fail with an uncaught exception because
`toDigits` is not yet implemented.

Let's add the minimal implementation needed for the spec to pass.
We'll do this in the `Luhn.Internal` module:

```haskell
toDigits :: Integer -> [Integer]
toDigits _ = [1,2,3,4,5,6,7]
```

Run the spec again and voilà! The spec passed. Now, let's add another expectation to
be sure. Add this code just below our previous expectation and run the spec again:

```haskell
      toDigits 2468 `shouldBe` [2,4,6,8]
```

It fails! Easy enough, we could modify the function to return the reversed list this
time. That will make the first expectation fail. As naive as this game seems, it
illustrates what happens in real life, once you have written larger amounts of code.

You can have the task to add new features to your application. So, you start refactoring
your code to make your new feature work but then you unknowingly break some other feature
you wrote days or even months ago. Without specs, you'll be lucky if you catch this error
before they hit production. You would have been warned about this in a very early stage,
with the right specs.

Let's implement this function, in a way that makes more sense, to make both expectations
pass:

```haskell
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = m : toDigits d where (d,m) = divMod n 10
```

Feeling proud about our achievements, we save the file, hit the console, run 'em specs
and ... still didn't pass. Not to worry. Hspec's feedback can help
us out. Just look at the output Hspec displays.
You should see something like this:

```
test/LuhnSpec.hs:16:
  1) Luhn.toDigits "converts a number to a list of digits"
       expected: [1,2,3,4,5,6,7]
        but got: [7,6,5,4,3,2,1]
```

We are getting the inverse of the expected result. So we change our code respectively:

```haskell
toDigits :: Integer -> [Integer]
toDigits = go []
  where go xs n
          | n <= 0    = xs
          | otherwise = go (m:xs) d where (d,m) = divMod n 10
```

Running the specs again everything should go green, and we regain our confidence.
Let's move on to the `doubleEveryOther` function. Think about it. What do we expect?

```haskell
  describe "doubleEveryOther" $ do
    it "doubles every other number starting from the right" $ do
      doubleEveryOther [1,2,3,4,5] `shouldBe` [1,4,3,8,5]
```

The specs shouldn't pass due to an uncaught exception again.

This time it's your turn. Give it a try and create an implementation for this function.
Remember not to start with a fancy solution right away. Just write what it's needed
for the spec to pass.

Now, we'll add another expectation to reassure our code behaves:

```haskell
      doubleEveryOther [1,2,3,4,5,6] `shouldBe` [2,2,6,4,10,6]
```

Specs fail again, of course. As a hint, having even and odd number of elements
in the list might complicate our algorithm. The use of functions like
`reverse` might help. Improve your code in small steps until the specs are
green one more time.

You can always take a look at the sample project accompanying this tutorial and
compare your results. Code doesn't need to match. As long as you get the right
results and specs pass, it's alright. And, remember to run your specs after
each step to gain immediate feedback.

### Contexts
Sometimes you need to write specs for a single function or feature, when used
in just a couple of different environments. You could use `it` clauses
describing the input properties every time. Or, you could use a `context`.
A `context` groups one or many `it` clauses that share the same kind of input or
environment.

In particular, we want to test `sumDigits` behavior when all of the
digit numbers are less than ten. Also, test the same function when some
of the numbers are greater or equal to ten.

We start by creating the context and expectations for the first case:

```haskell
  describe "sumDigits" $ do
    context "when all numbers are less than 10" $ do
      it "sums the list of integers" $ do
        sumDigits [1,2,3,4,5,6] `shouldBe` sum [1,2,3,4,5,6]
```

Why don't you give it a try and implement `sumDigits`? Again, you can look
at the source code to compare your results.

After you implement `sumDigits`, let's add another context to describe the behavior
of the function if the list contains some numbers greater or equal to ten.
This numbers should first sum their digits prior to summing the list itself:

```haskell
    context "when some numbers are greater or equal to 10" $ do
      it "sums their digits first before summing the list" $ do
        -- 2+1+2+4+1+4+6+8 = 28
        sumDigits [2,12,4,14,6,8] `shouldBe` 28
```

Can you modify the `sumDigits` implementation to match the new
requirements?

### QuickCheck
[QuickCheck][quickcheck] is a Haskell library that allows testing properties
rather than expectations. This means that if you have some piece of code
which must hold on a property that should always be true, you can use
QuickCheck to feed that property, with as much arbitrary inputs
as possible, to check if it really holds.

For educative purposes we'll add a `fromDigits` function. This function should
do the opposite of what `toDigits` does. It should take a list of digits and
convert it to a number.

Let's begin with some code as if we don't have QuickCheck around.
We can start by adding this code to the `describe "toDigits"` section
in the `LuhnSpec` file:

```haskell
    it "holds on: x == fromDigits(toDigits x)" $ do
      12345 `shouldBe` fromDigits (toDigits 123456)
```

And then add this simple implementation to the `Luhn.Internal` module.
Remember to expose this function in the `module` clause:

```haskell
fromDigits :: [Integer] -> Integer
fromDigits _ = 12345
```

Spec passes and everyone is happy. Now, let's replace the spec we just
wrote with a QuickCheck property instead:

```haskell
    it "holds on the property x == fromDigits(toDigits x)" $ do
      property $ \x -> x == (fromDigits . toDigits) x
```

Here we are creating a property. The property receives a function that will receive
arbitrary values of the inferred type and must return a `Bool`. In this case, `x` is
going to be of type `Integer` and we use it to assert the property.

Under the hood QuickCheck will feed the property function with many
arbitrary values, and the property checks if, and only if, for all cases,
the function returns `True`.

After running the specs and checking that the property doesn't hold,
we add a suitable implementation:

```haskell
fromDigits :: [Integer] -> Integer
fromDigits = sum . zipWith (*) [10 ^ i | i <- [0..]] . reverse
```

It didn't pass. What's going on? Let's check the Hspec display:

```
test/LuhnSpec.hs:22:
  1) Luhn.toDigits holds on: x == fromDigits(toDigits x)
       Falsifiable (after 3 tests):
       -1
```

Aha! The property doesn't hold for negative numbers. That's alright,
after all, this was the expected behavior. The `toDigits` function returns an
empty list for numbers lower or equal to zero. So we cannot recover the same
number back with the `fromDigits` function.

The solution is to prevent QuickCheck from feeding negative numbers.
QuickCheck's `(==>)` operator offers a way to specify arbitrary prerequisites
for your properties.

We just place the conditions you want the arbitrary values to comply, to the
left of the operator and your property check to the right. With this in mind,
we can rewrite our spec once more:

```haskell
      property $ \x ->
        x >= 0 ==> x == (fromDigits . toDigits) x
```

We are forcing the `x` values to be greater or equal to zero. This time around,
it works like a charm.

### Validating the Luhn algorithm
So, we've implemented all helper functions. However, the main `validate` function
is still missing. The spec should look like this:

```haskell
  describe "validate" $ do
    it "returns True if number is valid, False otherwise" $ do
      1234567889 `shouldSatisfy` validate
      1234567887 `shouldNotSatisfy` validate
```

Wait a minute. We are using different expectation functions, `shouldSatisfy` and
`shouldNotSatisfy` to be more precise. These functions receive a predicate function
of type `Show a => a -> Bool` as its second parameter. So basically, this
predicate must receive a parameter of any showable type and return a `Bool` value.
And given our `validate` function receives an `Integer` and returns a `Bool`,
it fits perfectly.

After some iterations we might come up with an implementation for `validate`
similar to this one, in our `Luhn` module ...

```haskell
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
```

... which is simply a composition of all of our previous helper functions. Just
run the specs one final time and, if everything is green, let's consider it done.

### Wrapping it up
In this tutorial we reviewed some concepts like TDD and BDD. We also gave you
a small taste of, what it is, and what it feels like writing specs the TDD/BDD way.
Additionally, we used a bunch of expectation functions and QuickCheck properties.

There is more to BDD than what we've learned in this tutorial. Here are some other
follow-up resources you can check:

* [Behavior-Driven Development in Haskell][bdd-simon] - Simon Hengel (Author of Hspec)
* [Hspec User's Manual][hspec]
* [The QuickCheck package][quickcheck]
* [BehaviourDrivenDevelopment][bdd-org]
* [CIS-194][cis-194] Haskell course

Happy testing! or should I say ... specification?

[hspec]: http://hspec.github.io/
[stack]: http://www.haskellstack.org/
[quickcheck]: https://hackage.haskell.org/package/QuickCheck
[bdd-simon]: http://www.bioinf.uni-leipzig.de/conference-registration/13haskell/submissions/hal8_submission_16.pdf
[bdd-org]: http://behaviourdriven.org/
[cis-194]: http://www.cis.upenn.edu/~cis194/spring13/
