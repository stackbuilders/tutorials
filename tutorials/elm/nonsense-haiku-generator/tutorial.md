---
title: A nonsense haiku generator in Elm
published: 2019-12-31
tags: elm
language: elm
author-name: Juan Pedro Villa Isaza
twitter-profile: jpvillaisaza
github-profile: jpvillaisaza
description: ...
---

https://github.com/stackbuilders/haiku

elm/browser
elm/core
elm/html
elm/http
elm/json
elm/random

A few weeks (months?) ago, GitHub released the Noops, inexplicable machines that
don't do anything at all. There are several interesting noops, but one that I
like is Wordbot, which allows to request English words. It's level 1, but good
enough to make something interesting and getting started with a programming
language.

In this case, we'll use Elm to see how to do things that are very common to any
web application (generating random numbers, making an HTTP request and parsing
the response), but the idea can be applied to any language and the benefits of
such a simple app should work there as well.

There are many things we can do with words. An idea  is to make haikus. This is
more complicated if we care to make good haikus, so instead we'll be making
nonsense haikus that break some of the rules, but the results are sometimes
good, and most of the time bad.

A haiku is a poem that consists of three lines. We need to think of syllables,
but in this case we don't want to, so we'll think of words. For simplicity,
we'll say that our fake haiku consists of three lines, each line consists of 1
to 3 words. Also, we'll get random words, any kind of word, but note that
Wordbot allows us to request specific kinds of words. The way this should work
is as follows: for each line, we generate a random number from 1 to 3, which
corresponds to the number of words that the line will have. Then, we request the
total number of words to Wordbot, and then we'll display the result to the user.
This will happen automatically upon page load, so refreshing should result in a
new haiku.

Let's begin by creating an Elm project with elm init. This creates a elm.json,
which contains information about our dependencies. There's no need to edit this
file directly. This is configured to use the src directory, so we can create an
Elm file there. To begin, let's use sandbox from elm/browser to create an empty
app. A sandbox application cannot do anything useful like requesting something,
so this is just temporary, we need to define what our model is (let's assume
it's nothing for now), how to update that model (there's nothing to update yet,
so it's always the same), and how to view it (in this case, this is very close
to HTML, except it's Elm code).

Before thinking of how to get random words to make up a model, let's think of
what the model should look like. We need something to hold the words, which is
just a list of words or some way to represent a poem as a list of lines, and
each line is a set of words. Now what is the initial model? Well, let's say it's
the empty poem with no words.

If we had some words, then this is how it would look like, the value (TODO: find
a haiku, either a real one or one of the generated ones).

The next thing we'll do is see how to generate the number of words that we want
the poem to have. We already mentioned we can't do something like this using a
sandbox, so we'll have to switch to an element, which is very similar, except we
now get commands and subscriptions. Commands are things that we can ask Elm to
do (like please go and generate a random number or please go fetch this thing
from this URL), and subscriptions are things we listen to (like please let me
know every time a second passes). In this case, if we want one or more random
numbers, we have to ask Elm to do it and we have to define how to handle the
result (that is, we tell Elm to do this, and then we also tell it how to process
the result once it's ready).

Let's see this in action. Upon loading the app, when we init, let's request a
random number from 1 to 3. We can use the int function. Now we need a way to
update our state via the Msg type, so let's add a case there.

And let's handle this in the update function in such a way that we simply set
the number to something.

Okay, now how do we generate three numbers?

Now that we have the number of words that we need, we have to see how to request
a word to wordbot. This is how the request looks and the response if we use
curl. The way to do this is very similar, we use the get from elm/url. We have
to explicitly say that we're expecting JSON as a response, and we need to say
how to decode this JSON and what to do with it (similar to how we asked Elm to
get a random number and then tell it what to do with the result once ready).

We come to a similar question, which is now how do we get more than one word,
but the answer is simpler now. We could request n times, but we can also use the
count query parameter for wordbot to get the exact number of words that we need.

Let's see one way to do this. Okay, and we update the words in the state.

So we have the words, now we need to make them fit into the model we defined for
a haiku, as follows.

And now we have all of the data, but it's not showing up in the view. We have to
go through each word and display it, which will look like html.

You can see an example here and the code is also available. Mention some options
to improve the code or to tweak it, or mention the idea of implementing this
using a different language such as Reason, or the idea of doing something with
one of the other Noops.

TODO: Check the linked repository to make sure it's configured to work as a
GitHub page and that it has CI configured (either CircleCI or GitHub Actions).
