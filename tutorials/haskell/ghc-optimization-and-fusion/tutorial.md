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

As it turns out, we may be already using the best algorithm for the task in
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

Often times, you will also want to **prevent inlining until some other
optimization happens**. This is also done with `NOINLINE`, but to control
order in which optimizations are applied, we will need to master more black
magic than we know now, so let's move on to specializing.

### Specializing

To understand how specializing works (what it is, for that matter), we first
need to review how ad-hoc polymorphism with type classes is implemented in
GHC. When there is a constraint in signature of a function:

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

Finally, it's worth noticing that explicit use of `SPECIASIZE` is not always
necessary. For instance, it's common for GHC to specialize on its own if
some function is inilined into non-polymorhpic context. It just inlines it
and subsequent passes of optimizer remove excessive polymorphism — one more
example of how simple inlining can trigger a “chain reaction” significantly
improving performance. As always, to make sure whether something works or
not, benchmark and profile — that's the only way to find out!

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
makes working with rules a bit tricky. That is, sometimes rule does not
“fire”. Some cases of this are covered in the next section, called
“Gotchas”.

Another important thing to mention is that when several rules match at once,
GHC will choose one arbitrarily to apply. You might be wondering “why not to
choose the first one for example” — well, given that rules are much like
instance declarations with respect to how they are imported, there is no
order for them, and the only thing GHC can do when several rules match is to
either apply none (probably it's worse than apply at least something) or
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
really suck out power from your program, so fusion can be a very nice
optimization technique.

To demonstrate benefits of fusion it's enough to start with a simple
composition of functions you may find yourself writing quite often:

```haskell
TODO filter, map, other stuff, but defined manually, not from base
```

With Criterion benchmarks I get the following results for that code:

```
TODO benchmark and put results here
```

Now let's see if the situation improves if we avoid building intermediate
lists:

```haskell
here use equivalent function that does “all at once”
```

Let's benchmark it:

```
TODO
```

TODO A paragraph

We just manually “fused” the two functions and produced code that runs
faster, consumes less memory, and does the same thing. But how can we give
up on composability and elegance of functional programming? No way!

What we would like to achieve is the following:

1. Ability to write beautiful, composable programs.
2. Avoid allocating intermediate results because it sucks.

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
reliable. Let's see it in action.

### Fusion without rewriting rules

In that sense, fusion does not necessarily have to happen via “invisible”
rewriting of your beautiful program by compiler, it can be quite explicit.
An example of such explicit approach is the `repa` library.

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
