---
title: GHC optimization and fusion
published: 2016-10-03
ghc: 8.0.1
lts: 7.4
libraries: criterion-1.1.1.0, weigh-0.0.3
language: haskell
author: Mark Karpov
author-name: Mark Karpov
description: You may have seen GHC pragmas with mysterious rules and phase indication in source code of some great Haskell libraries like ‘text’ or ‘vector’. What is this all about? How do you use them in your project? As it turns out it's easier than you may think.
---

The tutorial walks through details of using GHC pragmas such as `INLINE`,
`SPECIALIZE`, and `RULES` to improve performance of your Haskell programs.
Usually you gain this knowledge from
the
[GHC user manual](https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/index.html),
and that's definitely a recommended reading, but we've also noticed that
some bits of important info are scattered across different sites
like [Haskell Wiki](https://wiki.haskell.org/Introduction), not to mention
papers. This tutorial is an attempt to show all important optimization
“know-how” details in one place with practical examples benchmarked to
demonstrate their effects.

The details we are going to discuss are not in Haskell language report, it's
rather a sort of GHC-specific tuning you may want to perform when other
means of optimization are exhausted. Speaking of optimization means, here is
a list of what you may want to attempt to make your program work faster (in
the order you should attempt them):

1. Choose a better algorithm/data structures.
2. Use GHC pragmas (covered in the tutorial).
3. Rewrite critical bits in C.

As it turns out, we may be already using the best algorithm for task in
hand, so using GHC pragmas may be the only way to make considerable
improvements without going to the C land (which sure may make things faster,
but it also takes away the possibility of compiling your code with GHCJS for
example).

This tutorial would be normally considered rather “advanced”, because it has
to do with various GHC trickery, but I think it's quite approachable for
intermediate-beginner level Haskellers, because things it describes are to
some extent isolated from other topics and so they can be mastered by any
motivated individual.

## GHC pragmas

Pragmas are sort of special hints to the compiler. You should be familiar
with `LANGUAGE` pragmas that enable language extensions in GHC, e.g.:

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

The same syntax is used for all GHC pragmas. Technically, everything between
`{-` and `-}` is a comment, but adding hashes makes GHC watch for pragmas it
knows inside the comment.

We will discuss 3 topics:

1. Inlining with `INLINE` and `INLINABLE` pragmas.
2. Specializing with `SPECIALIZE`.
3. Crafting rewrite rules with `RULES`.

### Inlining

When a program is compiled, functions become labels — strings associated
with positions in machine code. To call a function, its arguments must be
put in appropriate places in memory, stack, and registers and then execution
flow jumps to address where the function begins. After function finished,
it's necessary to restore state of stack and registers so they look just
like before calling the function and jump back to continue executing the
program. All these instructions are not free, and for short function they
may actually take longer than execution of function's body itself.

Here *inlining* comes into play. The idea is simple: just take function's
body and insert it at the place it would otherwise be called. Since
functions to inline are usually short, duplication of code is minimal and we
get considerable performance boost. Inlining is perhaps the simplest (well,
for end user, not for compiler developers!) and yet very efficient way to
improve performance. Furthermore, we will see shortly that inlining in GHC
is not just about eliminating calls themselves, it's also a way to let other
optimizations be applied.

#### How GHC does inlining by itself

When GHC decides whether to inline particular function, it looks at its size
and assigns some sort of weight to that function in a given context. That's
right, the decision whether to inline a function or not is made on per-call
basis and a given function may be inlined in one place and called in another
place. We won't go into the details of how function's “weight” (or “cost”)
is calculated, but it should make sense that the lighter the function, the
keener the compiler is to inline it.

It's worth noticing that GHC is careful about avoiding excessive code bloat
and it does not inline blindly. Generally, a function is only inlined when
it makes at least some sense to inline it. When deciding whether to inline,
GHC considers the following:

* **Does it make sense to inline something at particular call site?** The
  GHC user guide shows the following example:

    ```haskell
    map f xs
    ```

    Here, inlining `f` would produce `map (\x -> body) xs`, which is not any
    better than the original, so GHC does not inline it.

    The case shown in the example can be generalized to he following rule:
    **GHC only inlines functions that are applied to as many arguments as
    they have syntactically on left-hand side (LHS) of function
    definition.** This makes sense because otherwise labmda-wrapping would
    be necessary anyway.

    To clarify, let's steal one more example from the GHC user guide:

    ```haskell
    comp1 :: (b -> c) -> (a -> b) -> a -> c
    comp1 f g = \x -> f (g x)

    comp2 :: (b -> c) -> (a -> b) -> a -> c
    comp2 f g x = f (g x)
    ```

    `comp1` has only two arguments on its LHS, while `comp2` has three, so
    call like this

    ```haskell
    map (comp1 not not) xs
    ```

    …optimizes better than a similar call with `comp2`.

* **How much code duplication inlining would cause?** Code bloat is bad as
  it increases compilation time, size of program, and lowers cache hit
  rates.

* **How much work duplication inlining would cause?** Consider the next two
  examples from the paper “Secrets of the Glasgow Haskell Compiler inliner”
  (Simon Peyton Jones, Simon Marlow):

    ```haskell
    let x = foo 1000 in x + x
    ```

    …where `foo` is expensive to compute. Inlining `x` would result in two calls
    to `foo` instead of one.

    Let's see another example:

    ```haskell
    let x = foo 1000
        f = \y -> x * y
    in … (f 3) … (f 4)
    ```

    This example shows that work can be duplicated even if `x` only appears
    once. If we inline `x` it its occurrence site, it will be evaluated every
    time `f` is called. The example shows that inlining inside of lambda may
    be a dangerous business.

Given the examples above, it's not surprising that GHC is quite conservative
about work duplication. However, it makes sense to put up with some
duplication of work because inlining often opens up new transformation
opportunities at the inlining site. To state it clearer, **avoiding the call
itself is not the only** (and actually not the main) **reason to do
iniling**. Inlining puts together pieces of code that were previously
separate thus allowing next passes of optimizer do more wonderful work.

With this in mind, you shouldn't be too surprised to find out that **body of
the inlineable function (or right-hand side, RHS) is not optimized by GHC**.
This is an important point that we'll revisit later. It's not optimized to
allow other machinery to do its work **after** inlining. For that machinery
it's important that function's body is intact because it operates on rather
syntactic level and optimizations, if applied, would leave almost no change
for the machinery to do its trick. For now remember that bodies of functions
that GHC sees as inlineable won't be optimized, they will be inserted “as
is”. (Body of an inlineable function won't be optimized and inlining may not
happen as well, so you may end up with a call to non-optimized function.
Fear not, we will learn how to fix that latter in the tutorial.)

One of simplest optimization techniques GHC can use with inlining is plain
old beta-reduction. But beta-reduction, combined with inlining, is nothing
short of compile-time evaluation of program. Which means that GHC should
somehow ensure that it terminates.

This brings us to two edge cases:

* **Self-recursive functions are never inlined.** This should be quite
  obvious, because if we chose to inline it, we would never finish.

* **With mutually recursive definitions**, **GHC selects** one or more
  **loop breakers**. Loop breakers are just functions that GHC chooses to
  call, not inline, to break the loop it would get into if it started to
  inline everything. For example if we have `a` defined via `b` and `b`
  defined via `a`, we can choose either of them as a loop breaker. GHC tries
  not to select a function that would be very beneficial to inline (but if
  it has no choice, it will).

Finally, before we move on to discussing how one can manually control
inlining, it's important to understand a couple of things about how compiled
Haskell programs are stored and what GHC can do with already compiled
Haskell code and what it cannot do.

Just like with many other languages that compile to native machine code,
after compilation of say, a library, we get `*.o` files,
called [object files](https://en.wikipedia.org/wiki/Object_file). They
contain object code, which is machine code that can be used in an
executable, but cannot usually be executed on its own. In other words, it's
a collection of compiled executable bits of that library. Every module
produces an object file of its own. But it's hard to work with just object
files, because they contain information in not very friendly form: you can
execute it, but you cannot generally reason about it.

To keep additional information about a compiled module, GHC creates also
what is called “interface files”, which contain info like what GHC was used
to compile it, list of modules given module depends on, list of things it
exports and imports, and other stuff; most importantly, interface files
contain bodies of inlineable functions (actual “unfoldings”) from compiled
module, so GHC can use them to do “cross-module” inlining. This is an
important thing to understand: **we cannot inline a function if we don't
have its body verbatim** (remember, GHC inlines functions without any
processing, as they are!), and unless function body in dumped in an
interface file, we only have object code which cannot be used for inlining.

With this knowledge, we are prepared now to learn how to manually control
inlining.

#### How to control inlining

GHC allows certain level of flexibility regarding inlinig, so there are
several ways to tell the compiler that some function should be inlined (and
even **where** it should be inlined). Since we've just spoken about
interface files, it makes sense to first introduce the `INLINEABLE` pragma.

Use of the pragma looks like this:

```haskell
myFunction :: Int -> Int
myFunction = …
{-# INLINEABLE myFunction #-}
```

Syntactically, an `INLINEABLE` pragma can be put anywhere its type signature
can be put, just like almost all other pragmas that works on per-function
basis.

The main effect of the pragma is that GHC will keep in mind that this
function may be inlined, even if it would not consider it inlineable
otherwise. We don't get any guarantees about whether the function will be
inlined or not in any particular case, but now unfolding of the function is
dumped in interface file, which means that it's possible to inline it in
another module, should it be necessary or convenient.

With a function marked `INLINEABLE`, we can use the special built-in
function called `inline`, which will tell GHC to try very hard to inline its
argument at a particular call site, like this:

```haskell
foo = bar (inline myFunction) baz
```

Semantically, `inline` it just an identity function.

Let's see an actual example of `INLINEABLE` in action. We have module `Goaf`
(that stands for “GHC optimizations and fusion”, BTW) with this:

```haskell
module Goaf
  ( inlining0 )
where

inlining0 :: Int -> Int
inlining0 x =
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000]
```

Here I tried hard and convinced GHC that `inlining` doesn't look very
inlinineable right now (well yeah, pretty dumb example, but it shows the
point), if we compile with `-O2` (as we will do in every example from now
on) and dump `Goaf.hi` interface file, we will see no unfolding of
`inlining0`'s body (if you use different version of GHC you may be unable to
reproduce exactly this output):

```
$ ghc --show-iface Goaf.hi

…

142c0e92c650162b33735c798cb20be3
  $winlining0 :: Int# -> Int#
  {- Arity: 1, HasNoCafRefs, Strictness: <S,U>, Inline: [0] -}
e447f016aa264b71f156911b664944d0
  inlining0 :: Int -> Int
  {- Arity: 1, HasNoCafRefs, Strictness: <S(S),1*U(U)>m,
     Inline: INLINE[0],
     Unfolding: InlineRule (1, True, False)
                (\ (w :: Int) ->
                 case w of ww { I# ww1 ->
                 case $winlining0 ww1 of ww2 { DEFAULT -> I# ww2 } }) -}

…
```

Here this `$winlining0` is actually compiled function that works on unboxed
integers `Int#` and it is not inlineable. `inlining0` itself is a thin
wrapper around it that turns result of type `Int#` into normal `Int` by
wrapping it into `Int`'s constructor `I#`. I won't go into detailed
explanations about unboxed data and primitives, but `Int#` is just your
bare-metal, hard-working C `int`, while `Int` is our familiar boxed, lazy
Haskell `Int` (there are links about primitive Haskell at the end of the
tutorial, you can start form there if this looks interesting).

We see two important things here:

* `inlining0` itself (in form of `$winlinig0`) is not dumped into the
  interface file, that means that we have lost the ability to look inside
  it.

* Still, hope dies last even for GHC, so it has turned `inlining0` function
  into a wrapper which itself is inlineable as you can see. The idea is that
  in the case if `inlining0` is called in arithmetic context with some other
  operations on `Int`s, GHC might be able to optimize further and better
  glue things working on `Int#`s (like `$winlining0`) together.

Now let's use the `INLINEABLE` pragma:

```haskell
inlining1 :: Int -> Int
inlining1 x =
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000]
{-# INLINEABLE inlining1 #-}
```

…which results in:

```
…

033f89de148ece86b9e431dfcd7dde8c
  $winlining1 :: Int# -> Int#
  {- Arity: 1, HasNoCafRefs, Strictness: <S,U>, Inline: INLINABLE[0],
     Unfolding: <stable> (\ (ww :: Int#) ->

       … a LOT of stuff…

6a60cad1d71ad9dfde046c97c2b6f2e9
  inlining1 :: Int -> Int
  {- Arity: 1, HasNoCafRefs, Strictness: <S(S),1*U(U)>m,
     Inline: INLINE[0],
     Unfolding: InlineRule (1, True, False)
                (\ (w :: Int) ->
                 case w of ww { I# ww1 ->
                 case $winlining1 ww1 of ww2 { DEFAULT -> I# ww2 } }) -}
```

The result is almost the same, but now we have complete unfolding of
`$winlinig0` in our interface file. It's unlikely that this will improve
performance considerably, because our functions are rather slow, one-shot
beasts and inlining really won't matter much here:

```
benchmarking inlining0
time                 5.653 ms   (5.632 ms .. 5.673 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.614 ms   (5.601 ms .. 5.627 ms)
std dev              39.86 μs   (33.20 μs .. 48.70 μs)

benchmarking inlining1
time                 5.455 ms   (5.442 ms .. 5.471 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.447 ms   (5.432 ms .. 5.458 ms)
std dev              38.08 μs   (28.36 μs .. 58.38 μs)
```

As expected, this gives rather marginal improvement, but in other cases it
may be more useful.

It turns out that not only inlining requires access to original function
body to work, some other optimizations do as well, so the **`INLINEABLE`
pragma**, causing putting function's unfolding into interface file
**effectively removes module boundaries that could otherwise prevent other
optimizations** from being applied. We will see how this works with
specializing in the next section. For that reason it's nothing unusual to
see `INLINEABLE` used on a self-recursive function, because the intention is
not to inline the function, but to dump its definition into interface file.

A more straightforward approach to control inlining is to use the `INLINE`
pragma. When GHC calculates weight of a function, this pragma makes the
function seem very lightweight, to the extent that GHC will always decide to
inline it. So `{-# INLINE myFunction #-}` will cause unconditional inlining
of `myFunction` everywhere (except for edge cases, like when `myFunction` is
self-recursive).

Inlining is always an option for compiler, unless you tell it that
particular function should not be inlined, and sometimes you will want to be
able to do that. In these cases the `NOINLINE` pragma may be helpful.

Let's have an example from a real, practical package called
`http-client-tls` which adds TLS (HTTPS) support to another package
`http-client` for doing HTTP requests. The package has a notion of HTTP
manager that stores information about open connections and stuff like that.
The problem with it is that it's expensive to create and in general you
should have only one such manager for maximal connection sharing. For that
there is a thing called `globalManager` which you can get and set when
you're in `IO` monad (uses `IORef`s under the hood). To get `IORef` of
global manager the following code is used (well not exactly, this is
simplified, but it may get into this form some day):

```haskell
globalManager :: IORef Manager
globalManager =
  unsafePerformIO (newManager tlsManagerSettings >>= newIORef)
{-# NOINLINE globalManager #-}
```

Here we use `unsafePerformIO` which has type `IO a -> a` and is a dangerous
thing to throw into your code, unless you know what you are doing. It
basically just does its dirty `IO` thing in broad daylight but pretends that
it's prudent and pure, not wanting to live in the `IO` cage. We want
`IORef`, not `IO IORef` (the latter is just a recipe how to get `IORef` to
one more such manager!) and we want it to be created just once. The
expression inside `unsafePerformIO` is to be run and after that its result
should be shared for all future use. Well, it will be shared all right,
since the value is named and top-level, but one thing may impede our
success: GHC can just inline it, causing re-creation and problems with
connection sharing that we wanted to avoid in the first place. To fix this
we add the `NOINLINE` pragma, not stressing about consequences of unsafe
attitude of `globalManager` anymore.

Another use-case for `NOINLINE` is more obvious. Remember that GHC won't
optimize body of an inlineable function? If you don't care if some function
`myFunction` will be inlined or not, but you want its body to be optimized,
you may solve the problem like this:

```haskell
myFunction :: Int -> Int
myFunction = …
{-# NOINLINE myFunction #-}
```

Often times, you will also want to **prevent inlining until some other
optimization happens**. This is also done with `NOINLINE`, but to control
order in which optimizations are applied, we will need to master more black
magic than we know now, so let's move on to specializing.

### Specializing

To understand how specializing works (and what it is, for that matter), we
first need to review how ad-hoc polymorphism with type classes is
implemented in GHC. When there is a constraint in signature of a function:

```haskell
foo :: Num a => a -> a
foo = …
```

…it means that the function should work differently for different `a` that
implement the type class (`Num` in our example). This is accomplished by
passing around a dictionary indexed by methods in given type class, so the
example above turns into:

```haskell
foo :: Num a -> a -> a
foo d = …
```

Note the `d` argument of type `Num a`. This is a dictionary that contains
functions that implement methods of `Num` type class. When a method of that
type class needs to be called, the dictionary is indexed by “name” of that
method and the extracted function is used. Not only `foo` accepts the
dictionary as an additional argument, it also passes it to polymorphic
functions inside `foo`, and those functions may pass it to functions in
their bodies:

```haskell
foo :: Num a -> a -> a
foo d = … bar d …
  where
    bar, baz :: Num a -> a -> a
    bar d = … baz d …
    baz d = …
```

It should be obvious by now that all that passing around and indexing is not
free, it does make your program slower. Think about it: **in every place we
actually use a polymorphic function, we have concrete types** (“use” in the
sense of performing actual work, not defining another polymorphic function
in terms of given one). Then it should be possible for GHC to figure out
which implementation in used in every place (we know types at compile time)
and speed up things considerably. When we turn a polymorphic function into
one specialized for concrete type(s), we do specializing.

You may be wondering now why GHC doesn't do this for us automatically? Well,
it tries and does specialize great deal of stuff, but there are cases (and
we run into them pretty often) when it cannot specialize:

* A module exports a polymorphic function. To specialize we need function's
  body, but in this case we only have compiled version of the function, so
  we just use it without specializing. The solution is to use `INLINEABLE`
  on the exported polymorphic function combined with `SPECIALIZE` in the
  module where we wish to specialize the function (see below).

So if you want to specialize, your tool is the `SPECIALIZE` pragma.
Syntactically, a `SPECIALIZE` pragma can be put anywhere its type signature
can be put:

```haskell
foo :: Num a => a -> a
foo = …
{-# SPECIALIZE foo :: Int -> Int #-}
```

The specified type may be any type that is less polymorphic than the type of
the original function, I like this example from GHC user manual, it states
that

```haskell
{-# SPECIALIZE f :: <type> #-}
```

…is valid when

```haskell
f_spec :: <type>
f_spec = f
```

…is valid. It makes sense!

The actual effect of the pragma is to generate a specialized version of
specified function and a rewrite rule (they are described in the section
about rewrite rules below with more details of how `SPECIALIZE` works) which
rewrites calls to the original function to calls to the specialized its
version whenever types match.

There is a way to specialize all methods in a type class for specific
instance of that class. It looks like this (example from GHC user guide):

```haskell
instance (Eq a) => Eq (Foo a) where
  {-# SPECIALIZE instance Eq (Foo [(Int, Bar)]) #-}
  … usual stuff …
```

It's also possible to inline specialized version of a function (vanilla
specialization disables inlinig as will be demonstrated later in the
tutorial) using the `SPECIALIZE INLINE` pragma. It may be surprising, but it
will even work with self-recursive functions. The motivation here is the
fact that a polymorphic function, unlike a function that works with concrete
types, may actually use different instances when it's called in different
contexts, so inlining specialized versions of the function does not
necessarily diverges. An obvious consequence of this is that GHC can also go
into an infinite loop, so be careful. `SPECIALIZE NOINLINE` variant is also
available.

For a practical example let's try to start with this code:

```haskell
special0' :: (Num a, Enum a) => a -> a
special0' x =
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000]

special0 :: Int -> Int
special0 x = special0' x `rem` 10
```

In the interface file we get:

```
…

3d2b7aef38f4af3a87867079a7fb9d7d
  $w$sspecial0' :: Int# -> Int#
  {- Arity: 1, HasNoCafRefs, Strictness: <S,U>, Inline: [0] -}

9aab4f68c56ea324d5b4f1ae96f44304
  special0 :: Int -> Int
  {- Arity: 1, HasNoCafRefs, Strictness: <S(S),1*U(U)>m,
     Unfolding: InlineRule (1, True, False)
                (\ (x :: Int) ->
                 case special0_$sspecial0' x of wild2 { I# x1 ->
                 I# (remInt# x1 10#) }) -}
97c360215ea1cab7acdf5a4928d349e8
  special0' :: (Num a, Enum a) => a -> a
  {- Arity: 3, HasNoCafRefs,
     Strictness: <S(C(C(S))LLLLLL),U(C(C1(U)),A,U,A,A,A,C(U))><L,U(A,A,A,A,A,A,C(C1(U)),A)><L,U> -}
efc0709eeb0afdb2be8cdce06cc54623
  special0_$sspecial0' :: Int -> Int
  {- Arity: 1, HasNoCafRefs, Strictness: <S(S),1*U(U)>m,
     Inline: INLINE[0],
     Unfolding: InlineRule (1, True, False)
                (\ (w :: Int) ->
                 case w of ww { I# ww1 ->
                 case $w$sspecial0' ww1 of ww2 { DEFAULT -> I# ww2 } }) -}
"SPEC special0' @ Int" [ALWAYS] forall ($dNum :: Num Int)
                                       ($dEnum :: Enum Int)
  special0' @ Int $dNum $dEnum = special0_$sspecial0'
```

What can I say, GHC is really good at specializing if polymorphic function
defined and used in the same module, I could not really find a case where
GHC 8.0.1 would fail to specialize on its own, bravo! The specialized
version of `special0'` is called `$w$sspecial0'` here and it works on `Int#`
for maximal speed.

What else do we see? `special0'` is compiled, but not dumped into the
interface file, this means that if we use it from another module we should
get considerably worse performance compared to `special0`, let's try:

```
benchmarking special0
time                 5.457 ms   (5.436 ms .. 5.477 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.481 ms   (5.470 ms .. 5.492 ms)
std dev              35.69 μs   (29.94 μs .. 44.88 μs)

benchmarking special0_alt   <---- defined in a separate module
time                 5.462 ms   (5.436 ms .. 5.496 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.472 ms   (5.458 ms .. 5.485 ms)
std dev              41.42 μs   (33.29 μs .. 55.02 μs)
```

Hmm? What's going on? `special0_alt` was able to take advantage on
specialized function `$w$sspecial0'` as well! But if we remove export of
`special0`, things change as `special0_alt` would not be able to find
appropriate specialization anymore:

```
benchmarking special0_alt
time                 912.0 ms   (866.2 ms .. 947.7 ms)
                     1.000 R²   (NaN R² .. 1.000 R²)
mean                 931.0 ms   (919.8 ms .. 939.9 ms)
std dev              13.88 ms   (0.0 s .. 15.45 ms)
```

Oh hell, ×167 slowdown is not good. Let's try to fix it:

```haskell
special0' :: (Num a, Enum a) => a -> a
special0' x =
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000]
{-# SPECIALIZE special0' :: Int -> Int #-}
```

This brings our specialization back:

```
  special0'_$sspecial0' :: Int -> Int
  {- Arity: 1, HasNoCafRefs, Strictness: <S(S),1*U(U)>m,
     Inline: INLINE[0],
     Unfolding: InlineRule (1, True, False)
                (\ (w :: Int) ->
                 case w of ww { I# ww1 ->
                 case $w$sspecial0' ww1 of ww2 { DEFAULT -> I# ww2 } }) -}
"SPEC special0'" [ALWAYS] forall ($dNum :: Num Int)
                                 ($dEnum :: Enum Int)
  special0' @ Int $dNum $dEnum = special0'_$sspecial0'
```

…and it indeed returns `special0_alt` its ability to perform well:

```
benchmarking special0_alt
time                 5.392 ms   (5.381 ms .. 5.403 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.399 ms   (5.392 ms .. 5.408 ms)
std dev              25.12 μs   (16.60 μs .. 38.90 μs)
```

Let's conclude:

* GHC has no problem specializing for you when polymorphic function is used
  in the same module it's defined: it has its body and it knows what to do.

* Lack of specialization kills your performance completely and very
  reliably.

* If you can guess which specializations to request from GHC when you write
  your module, most of the time you're OK and you can get your speed back.

What about the case when user of a library wants a specialization library's
author hasn't thought about? Let's see. We first remove the `SPECIALIZE`
pragma for `special0'`, so no specializations are generated on compilation
of our “source” module. Then we try to specialize in the “consumer” module:

```haskell
special0_alt :: Int -> Int
special0_alt x = special0' x `rem` 10
{-# SPECIALIZE special0' :: Int -> Int #-}
```

…and GHC tells us in plain English:

```
You cannot SPECIALISE ‘special0'’
  because its definition has no INLINE/INLINABLE pragma
  (or its defining module ‘Goaf’ was compiled without -O)
```

Cool, but we know that already, don't we? Let's add `special0'`'s body to
interface file with `INLINEABLE`:

```haskell
special0' :: (Num a, Enum a) => a -> a
special0' x =
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000]
{-# INLINEABLE special0' #-}
```

…and we win again:

```
benchmarking special0_alt
time                 5.329 ms   (5.313 ms .. 5.348 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.340 ms   (5.326 ms .. 5.356 ms)
std dev              45.16 μs   (36.56 μs .. 55.29 μs)
```

I've also had a different warning from GHC when I used the same combination
of `INLINEABLE`/`SPECIALIZE`:

```
SPECIALIZE pragma probably won't fire on inlined function ‘foo’.
```

…and benchmarks showed that it didn't fire indeed. So well, yeah, take care
and remember to benchmark every time you change somethnig!

### Rewrite rules

Haskell, being a pure language, gives GHC the magic ability to perform wide
range of transformations over Haskell programs without changing their
meanings. And GHC allows programmer to take part in that process. Thank you,
GHC!

#### The `RULES` pragma

The `RULES` pragma allows to write arbitrary rules how to transform certain
combinations of functions. Here is an example of `RULES` in use:

```haskell
{-# RULES
"map/map" forall f g xs. map f (map g xs) = map (f . g) xs
  #-}
```

Let's go though the example explaining its syntax (GHC user guide has a very
good description, so I don't see why not to include it here almost
verbatim):

* There may be zero or more rules in a `RULES` pragma, which you may write
  each on its own line or even several in one line separating them by
  semicolons.

* Closing `#-}` should start in a column to the right of the opening `{-#`.
  Pretty weird requirement, BTW. If you happen to know why it's this way,
  please comment.

* Each rule has a name, enclosed in double quotes. The name itself has no
  significance at all. It is only used when reporting how many times the
  rule fired.

* Each variable mentioned in a rule must either be in scope (e.g. `map`), or
  bound by the `forall` (e.g. `f`, `g`, `xs`). The variables bound by the
  `forall` are called the **pattern variables**. They are separated by
  spaces, just like in a type `forall`.

* A pattern variable may optionally have a type signature. **If the type of
  the pattern variable is polymorphic, it must have a type signature**. For
  example:

    ```haskell
    {-# RULES
    "fold/build"  forall k z (g :: forall b. (a -> b -> b) -> b -> b).
                  foldr k z (build g) = g k z
      #-}
    ```

    Since `g` has a polymorphic type, it must have a type signature.

* The left hand side of a rule must consist of a top-level variable applied
  to arbitrary expressions. For example, this is not OK:

    ```haskell
    {-# RULES
    "wrong1"   forall e1 e2.  case True of { True -> e1; False -> e2 } = e1
    "wrong2"   forall f.      f True = True
      #-}
    ```

    In `"wrong1"`, the LHS is not an application; in `"wrong2"`, the LHS has
    a pattern variable in the head.

* A rule does not need to be in the same module as (any of) the variables it
  mentions, though of course they need to be in scope.

* All rules are implicitly exported from the module, and are therefore in
  force in any module that imports the module that defined the rule,
  directly or indirectly. (That is, if `A` imports `B`, which imports `C`,
  then `C`'s rules are in force when compiling `A`.) The situation is very
  similar to that for instance declarations.

* Inside a rule `forall` is treated as a keyword, regardless of any other
  flag settings. Furthermore, inside a rule, the language extension
  `-XScopedTypeVariables` is automatically enabled.

* Like other pragmas, RULE pragmas are always checked for scope errors, and
  are typechecked. Typechecking means that the LHS and RHS of a rule are
  typechecked, and must have the same type.

Then GHC user guide goes on to explain what rewrite rules actually do (I
have edited it a bit):

> GHC uses a very simple, syntactic, matching algorithm for matching a rule
> LHS with an expression. It seeks a substitution which makes the LHS and
> expression syntactically equal modulo alpha-conversion (that is, a rule
> matches only if types match too). The pattern (rule), but not the
> expression, is eta-expanded if necessary. (Eta-expanding the expression
> can lead to laziness bugs.) But no beta-conversion is performed (that's
> called higher-order matching).

This requirement of verbatim matching modulo alpha conversion in combination
with the fact that a lot is going on during optimization process in GHC
makes working with rules a bit tricky. That is, sometimes rules do not fire.
Some cases of this are covered in the next section, called “Gotchas”.

Another important thing to mention is that when several rules match at once,
GHC will choose one arbitrarily to apply. You might be wondering “why not to
choose the first one for example” — well, given that rules are much like
instance declarations with respect to how they are imported, there is no
order for them, and the only thing GHC can do when several rules match is to
either apply none (probably it's worse than applying at least something) or
pick one randomly and apply that.

Now before we start considering problems you may have with `RULES`, I
promised to show what sort of rules the `SPECIALIZE` pragma generates. Here
they are:

```haskell
foo :: Num a => a -> a
foo = …
{-# SPECIALIZE foo :: Int -> Int #-}

⇒

fooForInts :: Int -> Int -- this is generated by GHC
fooForInts = …
{-# NOINLINE foo #-}
{-# RULES "foo for ints" foo = fooForInts #-}
```

Yes, specializing normally “disables” inlining. Think about it: we have
generated a specialized version of a function and we have a rule that
replaces polymorphic function `foo` with `fooForInts`, we don't want `foo`
to be inlined because then the rules would have no chance to fire!

#### Gotchas

Even though GHC keeps trying to apply the rules as it optimizes the program,
there are way too many opportunities for things to go in an unexpected
direction that may make the whole experience of crafting rewrite rules
rather frustrating (at least at first). This section highlights some of them
(well, I tried to collect here all of them, but there may be more).

**GHC does not attempt to verify whether RHS has the same meaning as LHS**.
It's the programmer's responsibility to ensure that the rules do not wreak
havoc! An example of a tricky rule that may seem obviously correct could be
something like this:

```haskell
{-# RULES
"double reverse" forall xs. reverse (reverse xs) = xs
  #-}
```

At first glance it really makes sense, doesn't it? The `"double reverse"`
rule nevertheless does not preserve meaning of expression it transforms.
`reverse (reverse xs)` applied to an infinite list would diverge, never
yielding any element, while infinite list `xs` can be consumed normally,
given that it's never forced in its entirety.

**GHC does not attempt to ensure that rules are terminating**. For example
(example from GHC user guide):

```haskell
{-# RULES
"loop" forall x y. f x y = f y x
  #-}
```

…will cause the compiler to go into an infinite loop.

To make things more interesting for programmer, not only every
transformation must not introduce any differences in meaning, ability to
terminate, etc., but also in complex combinations of functions, it is
desirable that we get the same result no matter where we start
transformation with the condition that we apply rules until no rules can be
applied anymore — this is called **confluence**. Here is an example that
will hopefully demonstrate what is meant (adapted from an example found on
Haskell Wiki):

```haskell
{-# RULES
"f/f" forall x. f (f x) = f x
"f/g" forall x. f (g x) = fg x
  #-}
```

The `"f\f"` rule states that `f` is a kind of idempotent function, while the
`"f/g"` rule recognizes the particular combitaion of `f` and `g` and
replaces it with ad-hoc implementation `fg`.

Now consider rewriting of `f . f . g`. If we first apply `"f/f"`, then we'll
end up with `fg x`, but if we first apply `"f/g"`, then we'll get `f . fg`.
The system is not confluent. An obvious fix would be to add this rule:

```haskell
{-# RULES
"f/fg" forall x. f (fg x) = fg x
  #-}
```

…which makes the system confluent. **GHC does not attempt to check if your
rules are confluent**, so take some time to check your rule set for
confluence too!

Finally, **writing rules matching on methods of type classes is futile**
because methods may be specialized (that is, replaced by specialized less
polymorphic functions generated “on-the-fly”) by GHC before rewrite rules
have a chance to be applied, so such rules most certainly won't fire because
the types specialized functions won't match types specificed in rewrite
rules.

Finally, while inlining can get into way of rewrite rules, it can also help
glue together different pieces of code acting as a catalyst for the chemical
reaction of rewrite rules. There is a special modifier to `INLINE` pragma
that's called `CONLIKE` that tells GHC “hey, if inlining this (even many
times) helps some rewrite rules fire, go wild and inline, that's cheap
enough for us”. `CONLIKE` stands for “constructor-like”. In fact, GHC
maintains invariant that every constructor application has arguments that
can be duplicated at no cost: variables, literals, and type applications
(you can find more about this in “Secrets of the GHC inliner”, see links for
further reading at the end of the tutorial), hence the name.

#### Phase control

I wouldn't be surprised if it feels now that a lot is happening during
optimization and things really get messy and interfere with each other in
undesirable ways. I, for one, have this feeling. There must be a way to say:
this should happen first, that should happen after. Well, there is a way.

GHC has a concept of simplifier phases. The phases are numbered. First phase
that runs currently has number 4 (maybe there will be more of them in later
versions of GHC), than goes number 3, 2, 1, and finally the last phase has
number 0.

Unfortunately, the phase separation does not give fine-grained control, but
just enough for us to construct something working. In an ideal world, we
would like to be able to specify which optimization procedure depends on
which, etc., instead we have only two options:

1. Specify beginning from which phase given rewrite rule or
   inline/specialize pragma should be enabled.

2. Specify up to which phase (not including) a rule should be enabled.

The syntactic part boils down to adding `[n]` or `[~n]` after pragma name.
GHC user tutorial has a really nice table that we absolutely must have here:

```haskell
                         -- Before phase 2     Phase 2 and later
{-# INLINE   [2]  f #-}  --      No                 Yes
{-# INLINE   [~2] f #-}  --      Yes                No
{-# NOINLINE [2]  f #-}  --      No                 Maybe
{-# NOINLINE [~2] f #-}  --      Maybe              No

{-# INLINE   f #-}       --      Yes                Yes
{-# NOINLINE f #-}       --      No                 No
```

Regarding “maybe”:

> By “Maybe” we mean that the usual heuristic inlining rules apply (if the
> function body is small, or it is applied to interesting-looking arguments
> etc).

The phase control is also available for `SPECIALIZE` and on per-rule basis
in `RULES`. Let's take a look what sort of effect phase indication has with
the `SPECIALIZE` pragma for example.

```haskell
foo :: Num a => a -> a
foo = …
{-# SPECIALIZE [1] foo :: Int -> Int #-}

fooForInts :: Int -> Int -- generated by GHC
fooForInts = …
{-# NOINLINE [1] foo #-}
{-# RULES    [1] foo = forForInts #-}
```

For example here phase indication for `SPECIALIZE` has the effect of
disabling inlining till it's time to activate the “specializing rule”.

As an example of how phase control may be indispensable with rewrite rules,
it's enough to look at `map`-specific rules found in `Prelude` (original
comments are preserved, they should make sense to you now!):

```haskell
map :: (a -> b) -> [a] -> [b]
{-# NOINLINE [0] map #-}
  -- We want the RULEs "map" and "map/coerce" to fire first.
  -- map is recursive, so won't inline anyway,
  -- but saying so is more explicit, and silences warnings
map _ []     = []
map f (x:xs) = f x : map f xs

-- Note eta expanded
mapFB ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
{-# INLINE [0] mapFB #-}
mapFB c f = \x ys -> c (f x) ys

-- The rules for map work like this.
--
-- Up to (but not including) phase 1, we use the "map" rule to
-- rewrite all saturated applications of map with its build/fold
-- form, hoping for fusion to happen.
-- In phase 1 and 0, we switch off that rule, inline build, and
-- switch on the "mapList" rule, which rewrites the foldr/mapFB
-- thing back into plain map.
--
-- It's important that these two rules aren't both active at once
-- (along with build's unfolding) else we'd get an infinite loop
-- in the rules.  Hence the activation control below.
--
-- The "mapFB" rule optimizes compositions of map.
--
-- This same pattern is followed by many other functions:
-- e.g. append, filter, iterate, repeat, etc.

{-# RULES
"map"       [~1] forall f xs.   map f xs                = build (\c n -> foldr (mapFB c f) n xs)
"mapList"   [1]  forall f.      foldr (mapFB (:) f) []  = map f
"mapFB"     forall c f g.       mapFB (mapFB c f) g     = mapFB c (f.g)
  #-}
```

Note two important points here:

1. Without phase control both rules `"map"` and `"mapList"` would be active
   at once and GHC would go into an infinite loop. Phase control is the only
   way to make this set of rules work.

2. We first use the `"map"` rule, and then we use `"mapList"` which
   essentially rewrites the function back into its `map` form. This strategy
   is called “pair rules”. The reasoning here is to try to represent a
   function in fusion-friendly form, but if by the time we hint phase 1
   fusion still did not happen, it's better to rewrite it back.

     It may be not obvious how result of `"map"` is going to match the
     `"mapList"` rules, but if you keep in mind definition of `build g = g
     (:) []` and the fact that it will most certainly be inlined by phase 1,
     then `"mapList"` should make perfect sense.

You might be thinking: “Fusion? Yet another buzzword in never-ending Haskell
dictionary?”. This brings us to the next major topic of this tutorial…

## Fusion

Enough of that hairy stuff! Take a deep breath, let's discuss something
different now. This section will be about fusion, but before we start
talking about it, we need to define what “fusion” is.

For the purposes of this tutorial, **fusion is a technique that allows to
avoid constructing intermediate results** (such as lists, vectors, arrays…)
when chaining operations (functions). Allocating intermediate results may
really suck out power from your program, so fusion is a very nice
optimization technique in certain cases.

To demonstrate benefits of fusion it's enough to start with a simple
composition of functions you may find yourself writing quite often. The only
difference is that we will use our own, homemade functions (functions from
`Prelude` have rewrite rules we are yet to reinvent) implemented as you
would expect:

```haskell
map0 :: (a -> b) -> [a] -> [b]
map0 _ []     = []
map0 f (x:xs) = f x : map0 f xs

foldr0 :: (a -> b -> b) -> b -> [a] -> b
foldr0 _ b []     = b
foldr0 f b (a:as) = foldr0 f (f a b) as

nofusion0 :: [Int] -> Int
nofusion0 = foldr0 (+) 0 . map0 sqr

sqr :: Int -> Int
sqr x = x * x
```

This all looks quite mundane — good ol' pipeline of functions with function
composition, you probably write a lot of such code. Let's see how it
performs:

```
benchmarking nofusion0
time                 155.4 ms   (146.4 ms .. 162.4 ms)
                     0.996 R²   (0.980 R² .. 1.000 R²)
mean                 155.1 ms   (151.3 ms .. 159.0 ms)
std dev              5.522 ms   (3.154 ms .. 7.537 ms)
```

This is the result with `[0..1000000]` passed as argument to `nofusion0`.

With `weigh` (a relatively new library that allows to find out memory
consumption of your code) I'm getting the following:

```
Case                  Bytes  GCs  Check
nofusion0       249,259,656  448  OK
```

In a lazy language like Haskell laziness just changes when parts of
intermediate lists are allocated, but they still must be allocated because
that's what next step in the “pipe” takes as input, and that's the overhead
we want to reduce with fusion.

Can we do better if we rewrite everything as a single function that sums and
multiplies in one pass? Not so sexy, but let's give it a try to see what
sort of power we missing with our “elegant” composition of list processing
functions:

```haskell
manuallyFused :: [Int] -> Int
manuallyFused []     = 0
manuallyFused (x:xs) = x * x + manuallyFused xs
```

Let's benchmark it:

```
benchmarking manuallyFused
time                 17.10 ms   (16.71 ms .. 17.54 ms)
                     0.996 R²   (0.992 R² .. 0.998 R²)
mean                 17.18 ms   (16.87 ms .. 17.62 ms)
std dev              932.8 μs   (673.7 μs .. 1.453 ms)

Case                 Bytes  GCs  Check
manuallyFused   96,646,160  153  OK
```

The improvement is dramatic. We just manually “fused” the two functions and
produced code that runs faster, consumes less memory, and does the same
thing. But how can we give up on composability and elegance — main merits of
functional programming? No way!

What we would like to achieve is the following:

1. Ability to write beautiful, composable programs.
2. Avoid allocating intermediate results where possible, because it sucks.

The point 2 can be (and has been) addressed differently:

1. We can build our vocabulary of little “primitive” operations (that we use
   as building blocks in our programs) in such a way that they do not ever
   produce results immediately. So when such primitives are combined, they
   produce another (wrapped) function that does not produce result
   immediately either. To get “real” result, we need yet another function
   that can “run” the composite action we've constructed. This is also
   fusion and this is how the `repa` package works for example.

2. We want to have our cake and eat it too. We can expose familiar interface
   where every “primitive” produces result immediately, but we also can add
   rewrite rules that will (hopefully) make GHC rewrite things in such a way
   (more on that below) that in the end compiler gets one tight loop without
   intermediate allocations.

I must say that I like the approach 1 more because it's more explicit and
reliable. Let's see it action.

### Fusion without rewrite rules

Returning to the example with `map` and `foldr`, we can re-write the
functions differently using the principle we just discussed (avoiding
generation of intermediate results). It's essential for fusion that we don't
write our functions as transformations of whole lists (or whatever you
have), because then we are back to the problem of creating those lists at
some point.

It's actually may be tricky to have several independent functions that
conceptually work on linked lists without re-creating list structure is some
form. So, we won't start with fusion that works on linked lists, let's start
with a more obvious example instead: arrays.

An array can be represented as a combination of its size and a function that
takes index and returns a value at that index. We can write:

```haskell
data Array a = Array Int (Int -> a)

rangea :: Int -> Array Int
rangea n = Array n id

mapa :: (a -> b) -> Array a -> Array b
mapa f (Array size g) = Array size (f . g)

foldra :: (a -> b -> b) -> b -> Array a -> b
foldra f b (Array size g) = go 0 b
  where
    go n b' | n < size  = go (n + 1) (f (g n) b')
            | otherwise = b'

fuseda :: Int -> Int
fuseda = foldra (+) 0 . mapa sqr . rangea
```

Here, we have what Repa calls “delayed arrays”. Note that the function
`rangea` allows to create arrays which have elements filled with their
indices. This is for simplicity, in a real library to work with arrays we
would want to complement the delayed arrays with real ones that hold all the
data in memory in adjoined addresses and allow for fast indexing, but for
the demonstration of fusion we can do without “real” arrays.

Now if you take a look at `mapa`, it doesn't really do anything but making
the indexing function just a little bit more complex, so we don't create any
intermediate results with it. `foldra` allows to traverse entire array and
get some value computed from all its elements, it plays role of a consumer
in our case. Finally, `fuseda 1000000` is the same as `manuallyFused
[0..1000000]`, but runs much faster.

Of course `fuseda` is not equivalent in power to `manuallyFused`, but the
whole collection and functions shows that it's possible to have
composability and speed at the same time. Note again, we get this by just
changing indexing function without actually doing anything with real array
(which of course can be “rendered” or built given an `Array`).

Now let's try to do something like this with linked lists, although it's
less obvious. We should start with the idea of not touching real list, but
modifying a function that… what? Indexes list? What such a function should
do to a list? If most basic function of array is to be indexed by position
of its elements, then what is the most basic function of list? How linked
list is consumed?

If we have a list `[a]`, then the way it's usually consumed is via
“unconsing”, that is, we take head of the list `a` and also we get the rest
of it `[a]`. There is a function named `uncons` in `Data.List`, let's take a
look at it:

```haskell
uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (a:as) = Just (a, as)
```

So here we can get head of given list and the rest of it, but if the given
list is empty, we can't get its head. This idea is expressed by `Maybe`.
Let's try to represent a “delayed list” as a wrapper around `uncons`-like
function:

```haskell
newtype List a = List ([a] -> Maybe (a, [a]))
```

How about `map` and `foldr`? It looks like they follow from that definition
rather naturally:

```haskell
map1 :: (a -> b) -> List a -> List b
map1 g (List f) = List h
  where
    h s' = case f s' of
      Nothing       -> Nothing
      Just (x, s'') -> Just (g x, s'')
```

Uh oh. This does not type check though:

> Couldn't match type `a` with `b`

What's the problem? Well, remember that we just want to make the inner
function more complex, in this particular case it means that it should
consume list of type `[a]` and produce list of type `[b]`, this means that
the inner function should have type `[a] -> Maybe (b, [a])` (remember, we
produce elements of `[b]` one at a time). Clearly, this type signature
differs from the one we have so far, hence we should adjust it:

```haskell
newtype List a b = List ([a] -> Maybe (b, [a]))
```

So the type `List a b` means “produces list of elements of type `b` from
list of elements of type `a`”. Not a very clear signature to have for a
thing like list, but let's put up with this and go to the end to see if this
at least performs better. Finally, `map1` compiles:

```haskell
map1 :: (a -> b) -> List s a -> List s b
map1 g (List f) = List h
  where
    h s' = case f s' of
      Nothing       -> Nothing
      Just (x, s'') -> Just (g x, s'')
```

The signature literally says: “when you have a list that has `a` elements,
no matter what you consume to get them (in our case this is something
labelled `s`), I'll give you another list that produces `b` elements, still
consuming the same thing `s`”.

Let's go ahead and implement `foldr1` (this is not your `foldr1` from
`Predule`, the numeric suffix just shows which example it belongs too, sorry
for possible confusion). To implement `foldr1` we need something to consume,
because we want to get one single value in the end, real value, not
something “delayed”.

We could pass source of values directly to `foldr1`, but it's not nice for
two reasons:

1. We want signature of `foldr1` stay as close to familiar signature of
   `foldr` as possible.

2. `foldr` is just one primitive that “forces” delayed list, what about
   other ones? Should we add an extra argument to all of them? This is not
   elegant, IMO.

So what can we do here? Well, perhaps we could store the initial list
together with the function we already have `[a] -> Maybe (b, [a])`:

```haskell
data List a b = List ([a] -> Maybe (b, [a])) [a]
```

We should remember though that we want to pass that list around but we
should not touch it until we want to “force” our list:

```haskell
map1 :: (a -> b) -> List s a -> List s b
map1 g (List f s) = List h s
--             ^           ^
--             |  “as is”  |
--             +-----------+
  where
    h s' = case f s' of
      Nothing       -> Nothing
      Just (x, s'') -> Just (g x, s'')

foldr1 :: (a -> b -> b) -> b -> List s a -> b
foldr1 g b (List f s) = go b s
  where
    go b' s' = case f s' of
      Nothing       -> b'
      Just (x, s'') -> go (g x b') s''
```

Now that we store initial list in `List` itself, we can write a function
that converts normal list to a delayed one:

```haskell
fromLinkedList :: [a] -> List a a
fromLinkedList = List uncons
```

And just for the sake of completeness, here is how to get it back:

```haskell
toLinkedList :: List a b -> [b]
toLinkedList (List f s) = unfoldr f s
```

Here is `unfoldr` from `Data.List`:

```haskell
unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr f s = case f s of
  Nothing      -> []
  Just (x, s') -> x : unfoldr f s'
```

`unfoldr` takes initial state, passes it to given function and gets one
element of final list and new state. It contitues till `Nothing` is
returned.

Finally, we can build `fused1` that solves the same problem of summing up a
list of squared numbers:

```haskell
fused1 :: [Int] -> Int
fused1 = foldr1 (+) 0 . map1 sqr . fromLinkedList
```

Elegance and composability: check. Let's benchmark it:

```
benchmarking fused1
time                 3.422 ms   (3.412 ms .. 3.433 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.432 ms   (3.427 ms .. 3.440 ms)
std dev              19.74 μs   (14.15 μs .. 29.65 μs)

Case                 Bytes  GCs  Check
fused1          80,000,016  153  OK
```

It's fastest implementation so far! What's wrong with our simple-minded
`manuallyFused` BTW? Shouldn't it be the fastest? Well, it's not
tail-recursive, we can rewrite it like this:

```haskell
manuallyFused' :: [Int] -> Int
manuallyFused' = go 0
  where
    go !n []     = n
    go !n (x:xs) = go (n + x * x) xs
```

And then it sure wins:

```
benchmarking manuallyFused'
time                 3.206 ms   (3.202 ms .. 3.210 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.213 ms   (3.210 ms .. 3.217 ms)
std dev              11.28 μs   (7.599 μs .. 17.46 μs)

Case                  Bytes  GCs  Check
manuallyFused'   80,000,016  153  OK
```

Returning to `List`, one thing we would like to do is to remove the type of
elements it consumes. I mean, if you have list of `a` elements, shouldn't it
be `List a`? Sure it should. Let's see definition of `List` again:

```haskell
data List a b = List ([a] -> Maybe (b, [a])) [a]
```

`a` here doesn't really ever change as per our idea of not touching it. We
could just hide it then using existential quantification:

```
data List b = forall a. List ([a] -> Maybe (b, [a])) [a]
≡ <via alpha reduction>
data List a = forall s. List (s -> Maybe (a, s)) s
```

With this we get the following signatures (implementations stay the same):

```haskell
fromLinkedList :: [a] -> List a
toLinkedList   :: List a -> [a]
map1           :: (a -> b) -> List a -> List b
foldr1         :: (a -> b -> b) -> b -> List a -> b
```

Much better! This section has demonstrated that fusion is doable and nice
without rewrite rules, in the next section we will explore the notion of
“fusion system”.

### `build`/`foldr` fusion system

Motivation behind fusion “systems”.

Mention `foldr/build` system and that it's used in base, but `foldl` and
`zip` are problematic to represent in this system. Also, we will need
familiarity with `unbuild`/`unfoldr` system to talk about stream fusion.

Briefly describe the fusion system and how some function and represented in
it Perhaps walk through a substitution process.

…

### What is stream fusion?

Motivation behind this framework.

https://www.youtube.com/watch?v=jyaLZHiJJnE

### Implementing your own stream fusion framework

…

## Conclusion

…

## TODO

* Once the text is ready, I need to do a pass over entire text and add
  examples, how they look compiled to core, how they perform before and
  after optimization (benchmarks). In the first example I also should show
  how to see which rules fire and show to see core.

## See also

Here are some links about GHC pragmas and fusion:

* [The section about inlining (GHC user guide)](https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#inline-and-noinline-pragmas)
* [Secrets of the GHC inliner (paper)](http://research.microsoft.com/en-us/um/people/simonpj/Papers/inlining/inline.pdf)
* [The section about specializing (GHC user guide)](https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#specialize-pragma)
* [The section about rewrite rules (GHC user guide)](https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#rewrite-rules)
* [Question on Stack Overflow: What is fusion in Haskell?](https://stackoverflow.com/questions/38905369/what-is-fusion-in-haskell)
* [Instructions how to use rewrite rules from Haskell Wiki](https://wiki.haskell.org/GHC/Using_rules)
* [Stream Fusion: Practical shortcut fusion for coinductive sequence types (Duncan Coutts' thesis)](http://community.haskell.org/~duncan/thesis.pdf)

If you are into this topic, you may want to learn about GHC primitives as
well:

* [Primitive Haskell](https://www.schoolofhaskell.com/user/commercial/content/primitive-haskell), an article from School of Haskell
* [The relevant section in the GHC user guide](https://downloads.haskell.org/%7Eghc/8.0.1/docs/html/users_guide/glasgow_exts.html#unboxed-types-and-primitive-operations)
* [`GHC.Prim` module](https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html)
