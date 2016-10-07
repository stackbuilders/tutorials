---
title: GHC optimization and fusion
published: 2016-10-03
ghc: 8.0.1
lts: 7.2
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

1. Choose a better algorithm.
2. Use GHC pragmas (covered in the tutorial).
3. Rewrite critical bits in C.

As it turns out, we may be already using the best algorithm for the task in
hand, so using GHC pragmas may be the only way to make considerable
improvements without going to the C land (which sure may make things faster,
but it also takes away the possibility of compiling your code with GHCJS for
example).

This tutorial would be normally considered as rather “advanced”, because it
has to do with various GHC trickery, but I think it's quite approachable for
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
improve performance of some functions. Furthermore, we will see shortly that
inlining in GHC is not just about eliminating calls themselves, it's also a
way to let other optimizations be applied.

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

**Does it make sense to inline something at particular call site?** The GHC
user guide shows the following example:

```haskell
map f xs
```

Here, inlining `f` would produce `map (\x -> body) xs`, which is not any
better than the original, so GHC does not inline it.

The case shown in the example can be generalized to he following rule: **GHC
only inlines functions that are applied to as many arguments as they have
syntactically on left-hand side (LHS) of function definition.** This makes
sense because otherwise labmda-wrapping would be necessary anyway.

To clarify, let's steal one more example from the GHC user guide:

```haskell
comp1 :: (b -> c) -> (a -> b) -> a -> c
comp1 f g = \x -> f (g x)

comp2 :: (b -> c) -> (a -> b) -> a -> c
comp2 f g x = f (g x)
```

`comp1` has only two arguments on its LHS, while `comp2` has three, so call
like this

```haskell
map (comp1 not not) xs
```

…optimizes better than a similar call with `comp2`.

**How much code duplication inlining would cause?** Code bloat is bad as it
increases compilation time, size of program, and cache hit rates.

**How much work duplication inlining would cause?** Consider the next two
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
time `f` is called. The example shows that inlining inside of lambda may be
a dangerous business.

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

One of simplest optimization techniques GHC can use is plain old
β-reduction. But β-reduction, combined with inlining, is nothing short of
compile-time evaluation of program. Which means that GHC should somehow
ensure that it terminates.

This brings us to two edge cases:

* **Self-recursive functions are never inlined.** This should be quite
  obvious, because if we chose to inline it, we would never finish.

* **With mutually recursive definitions**, **GHC selects** one or more
  **loop breakers**. GHC tries not to select a function that would be very
  beneficial to inline (but if it has no choice, it will). Once loop
  breakers are chosen, GHC inlines everything it wants, except for those
  loop breakers.

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
myFunciton :: Int -> Int
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
able to do that. Here the `NOINLINE` pragma may be helpful.

Remember that GHC won't optimize body of an inlineable function? If you
don't care if some function `myFunction` will be inlined or not, but you
want its body to be optimized, you may solve the problem like this:

```haskell
myFunction :: Int -> Int
myFunction = …
{-# NOINLINE myFunction #-}
```

It's rather rare to see this in source code though. Often times, you will
want to **prevent inlining until some other optimization happens**. This is
also done with `NOINLINE`, but to control order in which optimizations are
applied, we will need to master more black magic than we know now, so let's
move on to specializing.

### Specializing

To understand how specializing works (what it is, for that matter), we first
need to review how ad-hoc polymorphism with type classes is implemented in
GHC. When there is a constraint in signature of a function:

```haskell
foo :: Num a => a -> a
foo = …
```

it means the function should work differently for different `a` that
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
actually use a polymorphic function, we have concrete types**. It should be
possible for GHC to figure out which implementation in used in every place
(we know types at compile time) and speed up things considerably. When we
turn a polymorphic function into one specialized for concrete type(s), we do
specializing.

You may be wondering now why GHC doesn't do this for us automatically? Well,
it tries, but there are cases (and we run into them pretty often) when it
cannot specialize:

* A module exports a polymorphic function. To specialize we need function's
  body, but in this case we only have compiled version of the function, so
  we just use it without specializing. The solution is to use `INLINEABLE`
  on the exported polymorphic function combined with `SPECIALIZE` in the
  module where we wish to specialize the function (see below).

* Every time we specialize, we create a new function with about the same
  size of the original (polymorphic) one. This leads to code bloat.

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

is valid when

```haskell
f_spec :: <type>
f_spec = f
```

is valid. It makes sense!

The actual effect of the pragma is to generate a specialized version of
specified function and a rewrite rule (they are described below with more
details of how `SPECIALIZE` works, in the section about rewrite rules) which
rewrites calls to the original function to calls to the specialized its
version whenever types match.

There is a way to specialize all methods in a type class for specific
instance of that class. It looks like this (example from GHC user guide):

```haskell
instance (Eq a) => Eq (Foo a) where
  {-# SPECIALIZE instance Eq (Foo [(Int, Bar)]) #-}
  … usual stuff …
```

It's also possible to inline specialized version of a function using the
`SPECIALIZE INLINE` pragma. It may be surprising, but it will even work with
self-recursive functions. The motivation here is the fact that a polymorphic
function, unlike a function that works with concrete types, may actually use
different instances every time it's called, so inlining specialized versions
of the function does not necessarily diverges. An obvious consequence of
this is that GHC can also go into an infinite loop, so be careful.
`SPECIALIZE NOINLINE` variant is also available.

Finally, it's worth noticing that explicit use of `SPECIASIZE` is not always
necessary. For instance, it's common for GHC to specialize of its own if
some function is inilined into non-polymorhpic context. It just inlines it
and subsequent passes of optimizer remove excessive polymorphism — one more
example of how simple inlining can trigger a “chain reaction” significantly
improving performance. As always, to make sure whether something works or
not, benchmark and profile — that's the only way to find out!

### Rewrite rules

Haskell, being a pure language, gives GHC the magic ability to perform wide
range of transformations over Haskell programs. And GHC allows programmer to
take part in that process. Thank you, GHC!

What rewrite rules are, and why they are possible in Haskell. Show the
`RULES` pragma, explain what different parts of the rule mean. How the
process happens (pretty mechanically), GHC gives no guarantees.

Phase control (runs of simplifier). How phase numbers are counted (43210). The
syntax for phase control.

Order of rule matching (bottom-up).

The main reason for using phase control on specialisations is so that you
can write optimisation RULES that fire early in the compilation pipeline,
and only then specialize the calls to the function. If specialization is
done too early, the optimisation rules might fail to fire.

It's interesting to note, that declaring a function as INLINE disables
fusion for the inner of the function.

Show the problem that everything happens at once and introduce phase control
to solve it.

How rules interact with class methods.

Show a table that summarizes what should happen in what order (might be
tricky, but looks like it's doable).

In a confluent term rewriting system you will always end up in the same
result if you apply rewriting rules in different order until no more rules
can be applied. We assume that non-confluent rewriting systems are bad
design, but it is not clear how to achieve confluence for any system.

The `reserse (reverse xs) = xs` example.

Pair rules.

Inlining is safe when the thing we're inlinig is

* a variable
* a constructor application
* a lambda abstraction
* an expression that is sure to diverge (if it's evaluated at least once,
  the program will diverge anyway)

GHC maintains invariant that every constructor application has arguments
that can be duplicated at no cost: variables, literals, and type
applications.

Inline conlike. (Move this to the section about rules?)

Notice that the phase control is not powerful enough to account for all the
things described.

Write, compile, and profile an example (take something from base or
`map/map`). Show that it gets faster, and use `-ddump-rule-firings` or
`-ddump-simple-stats` to show that the rule fires indeed.

## Fusion

Explain what fusion generally is and it's not always about rewriting code
with rules.

…

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
