---
title: Nonsense! Getting started with Reason and ReasonReact
published: 2020-04-16
tags: reason
language: reason
author-name: Juan Pedro Villa Isaza
twitter-profile: jpvillaisaza
github-profile: jpvillaisaza
description:
  In this tutorial, we see how to get started with the Reason programming
  language and the ReasonReact library. As an example, we create an application
  that generates nonsense English sentences using the Wordbot GitHub Noop.
---

> “the planetarium presented a jar of pickles”

In 2019, GitHub published the [Noops][noops], twenty mysterious machines that
don’t do anything at all besides returning random stuff, from color codes and
coordinates to encryption riddles. Each Noop is a coding challenge to make it do
something and just have fun with code along the way.

[noops]: https://noopschallenge.com

In this tutorial, we’ll use the [Wordbot][wordbot] Noop to create an application
that generates nonsense English sentences. Wordbot is just an endpoint that
gives a different set of words with every request. It knows about 171,476 words,
which is the number of words in the second edition of the Oxford English
dictionary, published in 1989 (see [How many words are there in the English
language?][1]).

[wordbot]: https://noopschallenge.com/challenges/wordbot
[1]: https://www.lexico.com/explore/how-many-words-are-there-in-the-english-language

Let’s see how Wordbot works using cURL:

```
curl https://api.noopschallenge.com/wordbot
{"words":["pyjama"]}
```

We can request more than one word using the `count` parameter or request words
from predefined sets of words (for example, nouns, verbs, and dinosaurs) using
the `set` parameter:

```
curl 'https://api.noopschallenge.com/wordbot?count=3&set=nouns'    
{"words":["panacea","no","perquisite"]}
```

There are a lot of ways to create an English sentence, but we’re going to use a
fixed structure:

1. an article and a noun
2. a verb in the past tense
3. an article and an object
4. an optional adverb

For example:

- the spade sealed the bottle of lotion thoroughly
- the newsmonger earned a cell phone
- the rioter mugged the squirrel

The idea is simple, but involves several tasks that help with getting some
familiarity with a programming language, such as making a request and parsing
the response, and generating random data.

In this case, we’re going to use the [Reason][reason] programming language and
the [ReasonReact][reason-react] library to solve the challenge. Using
ReasonReact allows us to apply the things we know about React, but in a safer
and simpler way because we can take advantage of the Reason (and OCaml) type
system (see [What and why (Reason)][2] and [What and why (ReasonReact)][3]).

[reason]: https://reasonml.github.io
[reason-react]: https://reasonml.github.io/reason-react/en/
[2]: https://reasonml.github.io/docs/en/what-and-why
[3]: https://reasonml.github.io/reason-react/docs/en/what-and-why

Reason is part of [BuckleScript][bucklescript], which we can install via npm, so
let’s create a project using npm:

[bucklescript]: https://bucklescript.github.io

```
mkdir nonsense && cd nonsense/
npm init --yes
```

This creates a `package.json` file with minimal configuration:

```json
{
  "name": "nonsense",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  "keywords": [],
  "author": "",
  "license": "ISC",
}
```

And we can now install BuckleScript:

```
npm install --save-dev bs-platform
```

This adds a development dependency to the package configuration file and
installs several executables, including `bsb`, which we can use to build Reason
files and convert them to JavaScript.

To work with BuckleScript and Reason, we need a separate configuration file,
`bsconfig.json`. The `bsb` executable can be used to initialize a project from a
template, but we’ll create it manually:

```json
{
  "name": "nonsense",
  "namespace": true,
  "sources": ["src"],
  "package-specs": {
    "module": "commonjs",
    "in-source": true
  },
  "suffix": ".bs.js",
}
```

The main parts of the configuration are the name, which matches the name in the
`package.json` file, the list of sources, which is where we should put our
Reason files, and the suffix, which is what `bsb` will use for the generated
files.

Next, let’s create a `src/index.re` file that prints something to the console:

```reason
Js.log("Nonsense!");
```

To build the file, we first need to add some scripts to the `package.json` file:

```
"scripts": {
  "build": "bsb -make-world",
  "start": "bsb -make-world -w"
},
```

Running `npm run build` builds the Reason files and generates JavaScript files,
and running `npm run start` additionally watches for changes to the Reason
files.

To see what this looks like, let’s create a `src/index.html` and load the
generated JavaScript file (after running `npm run build`):

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Nonsense!</title>
  </head>
  <body>
    <script src="./index.bs.js"></script>
  </body>
</html>
```

If we open the file in a browser and open the developer console, it should have
a log message, which is the only thing that the application does so far.

To make the actual application, we have to install React and ReasonReact:

```
npm install --save react react-dom reason-react
```

This installs the dependencies and we can now add ReasonReact to the
BuckleScript configuration file as a dependency. Additionally, we enable the JSX
syntax for React:

```
"bs-dependencies": [
  "reason-react"
],
"reason": {
  "react-jsx": 3
},
"refmt": 3
```

Let’s create a `src/Nonsense.re` file, which will contain a React component that
initially just displays a message (instead of logging a message):

```reason
[@react.component]
let make = _ => <div> {React.string("Nonsense!")} </div>;
```

And update the `src/index.re` file to render the component to an element:

```reason
ReactDOMRe.renderToElementWithId(<Nonsense />, "nonsense");
```

The element ID has to exist somewhere in the HTML, so we also need to update the
`src/index.html` file and add it to the body:

```html
<div id="nonsense"></div>
```

This time, running `npm run build` and opening `src/index.html` with a browser
shouldn’t work because now we have some additional dependencies and would need
to bundle the JavaScript files. A simple solution is to install
[`moduleserve`][moduleserve]:

[moduleserve]: https://www.npmjs.com/package/moduleserve

```
npm install --save-dev moduleserve
```

And add a new script to serve the application:

```
"scripts": {
  "build": "bsb -make-world",
  "start": "bsb -make-world -w",
  "serve": "moduleserve src/ --port 8000"
},
```

The scripts in the `src/index.html` file have to be updated as well following
the `moduleserve` documentation:

```html
<script>
  window.process = {
    env: {
      NODE_ENV: "development"
    }
  };
</script>
<script src="/moduleserve/load.js" data-module="/index.bs.js"></script>
```

Note that this HTML file and the new script are only for development. For a
complete project, take a look at one of the [BuckleScript templates][e], which
can be used with the `bsb` program to create a project from a template.

[e]: https://github.com/BuckleScript/bucklescript/tree/master/jscomp/bsb/templates

With this setup, we can run `npm run start` in one terminal tab and `npm run
serve` in another terminal tab, and access the application. It doesn’t do
anything interesting yet, but we’re ready to start adding the solution.

The state of the application will be a simple type that represents three
possible scenarios: an error state (`Error`), an empty or loading state
(`Words([])`), and a loaded state (for example, `Words(["the",
"planetarium"])`):

```reason
type state =
  | Error
  | Words(list(string));
```

The idea is that we get the loading state after the component mounts and then
switch to the loaded state once we get a word for the sentence.

To set the initial state, we use the state hook and also modify the rendering
part of the component so that it displays something different for each value of
the state:

```reason
[@react.component]
let make = _ => {
  let (state, setState) = React.useState(_ => Words([]));

  <div>
    {switch (state) {
     | Error => React.string("Error!")
     | Words([]) => React.string("Loading...")
     | Words(words) => React.string(String.concat(" ", words))
    }}
  </div>;
};
```

[Hooks in ReasonReact][f] look almost the same as [hooks in React][g], except
for a few syntax differences.

[f]: https://reasonml.github.io/reason-react/docs/en/components#hooks
[g]: https://reactjs.org/docs/hooks-intro.html

With this change, if we open the application, it should always display the
loading state because we’re not doing anything to change it. To do so, we need
to get a word from Wordbot, which requires some way of making a request. A quick
way to accomplish this is to install [bs-fetch][bs-fetch]:

[bs-fetch]: https://github.com/reasonml-community/bs-fetch

```
npm install --save bs-fetch
```

Like we did before, we have to modify the dependencies for the BuckleScript
project as well:

```
"bs-dependencies": [
  "bs-fetch",
  "reason-react"
],
```

And we can use the effect hook to get a word when first loading the application.
This should be placed after the state hook and before the rendering code:

```reason
React.useEffect0(() => {
  Fetch.fetch("https://api.noopschallenge.com/wordbot")
  |> Js.Promise.then_(Fetch.Response.json)
  |> Js.Promise.then_(json => {
       switch (decodeWord(json)) {
       | None => setState(_ => Error)
       | Some(word) => gotWord(word)
       };
       Js.Promise.resolve();
     })
  |> ignore;
  None;
});
```

This is similar to making a cURL request, except we have to handle the response,
which means handling a promise. The fetch function returns a promise of a
response and we use the [`(|>)`][h] operator to apply something to the result
and then use the same operator to apply something to the new result (that is, `x
|> f |> g` is the same as `g(f(x))`). The first thing we do is turn the response
into JSON, which also returns a promise. The second thing we do is try to decode
the JSON into a single word using the `decodeWord` function (the response of
Wordbot is an object with a field of words). If decoding a single word fails, we
set the state to the error state. Otherwise, we call a `gotWord` function that
is not defined yet.

[h]: https://reasonml.github.io/api/Pervasives.html#VAL(|>)

The `decodeWord` word function takes a JSON value and returns an optional string
(it can be a value of `None` representing an error or a value such as
`Some("planetarium")`). Here’s an initial implementation of the function (this
should be placed above the effect code):

```reason
let decodeWord = (json: Js.Json.t): option(string) =>
  switch (Js.Json.decodeObject(json)) {
  | None => None
  | Some(object_) =>
    switch (Js.Dict.get(object_, "words")) {
    | None => None
    | Some(words) =>
      switch (Js.Json.decodeArray(words)) {
      | None => None
      | Some(words) =>
        switch (Belt.Array.get(words, 0)) {
        | None => None
        | Some(word) => Js.Json.decodeString(word)
        }
      }
    }
  };
```

That’s a lot of nested switches, but the idea is simple: we try to turn the JSON
into an object, then try to get the field called `words` inside the object, then
try to decode that field as an array, then try get the first word (we’re only
requesting one word), and then decoding that to a string. Each step handles the
error or `None` case and the whole function fails if any of the steps is an
error.

One way to simplify this function is to use the [`Belt.Option.flatMap`][flatMap]
function, which is exactly what we’re doing (at each step, take an optional
value, map a function over the optional value, and then flatten the result so
that it’s only one option instead of two):

[flatMap]: https://bucklescript.github.io/bucklescript/api/Belt.Option.html#VALflatMap

```reason
let decodeWord = (json: Js.Json.t): option(string) =>
  Js.Json.decodeObject(json)
  ->Belt.Option.flatMap(object_ => Js.Dict.get(object_, "words"))
  ->Belt.Option.flatMap(Js.Json.decodeArray)
  ->Belt.Option.flatMap(words => Belt.Array.get(words, 0))
  ->Belt.Option.flatMap(Js.Json.decodeString);
```

Note that we’ve used functions from different APIs: there’s a [Reason
API][reason-api] and a [BuckleScript API][bs-api], which includes the
BuckleScript standard library (Belt) and some common JavaScript functions (Js).

[reason-api]: https://reasonml.github.io/api/index
[bs-api]: https://bucklescript.github.io/docs/en/stdlib-overview

Now let’s move on to the `gotWord` function, which takes a word and updates the
state (this should be placed above the effect code):

```reason
let gotWord = (word: string): unit =>
  setState(state =>
    switch (state) {
    | Error => Error
    | Words(words) => Words(List.append(words, [word]))
    }
  );
```

If the current state is the error state, we don’t do anything. Otherwise, we
append the word to the existing list of words. After these changes, the
application should be getting and displaying a random word from Wordbot.

To make a sentence we’ll have to get words from a predefined set of words, so
we’ll extract the fetching code to a separate function and add a parameter to
specify the set that we want:

```reason
let getWord = (~set: string): unit => {
  Fetch.fetch("https://api.noopschallenge.com/wordbot?set=" ++ set)
  |> Js.Promise.then_(Fetch.Response.json)
  |> Js.Promise.then_(json => {
       switch (decodeWord(json)) {
       | None => setState(_ => Error)
       | Some(word) => gotWord(word)
       };
       Js.Promise.resolve();
     })
  |> ignore
};
```

This allows us to simplify the effect code to use the new helper function, and
we can specify the nouns set, which is the first type of word that we want:

```reason
React.useEffect0(() => {
  getWord(~set="nouns");
  None;
});
```

Before requesting the rest of words, we have to process the noun to choose an
article. For this, we’ll update the `gotWord` function to choose a random
article and add both the article and the noun to the list of words. To get
random data, we initialize the generator (we can add this right at the beginning
of `make`):

```reason
Random.self_init();
```

And now we can add the changes to `gotWord`:

```reason
let gotWords = (words': list(string)): unit =>
  setState(state =>
    switch (state) {
    | Error => Error
    | Words(words) => Words(List.append(words, words'))
    }
  );

let isVowel = (letter: char): bool =>
  List.mem(letter, ['a', 'e', 'i', 'o', 'u']);

let gotWord = (set: string, word: string) =>
  switch (set) {
  | "nouns"
  | "objects" =>
    switch (Random.int(2)) {
    | 0 => gotWords(["the", word])
    | _ when isVowel(word.[0]) => gotWords(["an", word])
    | _ => gotWords(["a", word])
    }
  | _ => gotWords([word])
  };

let getWord = (~set: string): unit => {
  Fetch.fetch("https://api.noopschallenge.com/wordbot?set=" ++ set)
  |> Js.Promise.then_(Fetch.Response.json)
  |> Js.Promise.then_(json => {
        switch (decodeWord(json)) {
        | None => setState(_ => Error)
        | Some(word) => gotWord(set, word)
        };
        Js.Promise.resolve();
     })
  |> ignore;
};
```

The first change we added is that `gotWord` is now taking the set of words to
determine whether we need an article or not. If we do (the set is nouns or
objects), then we ask for a random number between 0 and 1 (`Random.int(2)`
returns a number from 0 (inclusive) to 2 (exclusive)). If the number is 0, we
use the article “the”. If the number is 1, we use “an” if the word starts with a
vowel or “a” if the word doesn’t start with a vowel. This check will result in
some incorrect sentences, but the results will be good most of the time.

At this point, if we load the application, it should display an article and a
noun.

We can’t request more than one word at the same time because we want words from
different sets, so we’ll make a change to the `getWord` function to be able to
request the following words:

```reason
let getWord = (~set: string): Js.Promise.t(unit) => {
  Fetch.fetch("https://api.noopschallenge.com/wordbot?set=" ++ set)
  |> Js.Promise.then_(Fetch.Response.json)
  |> Js.Promise.then_(json => {
       switch (decodeWord(json)) {
       | None => setState(_ => Error)
       | Some(word) => gotWord(set, word)
       };
       Js.Promise.resolve();
     });
};
```

The only difference is that we removed the `ignore`, which means that `getWord`
now returns a promise that we can handle to make a new request once a previous
request is done. The next word that we need is a verb, so the effect code can be
changed to look like this:

```reason
React.useEffect0(() => {
  getWord(~set="nouns")
  |> Js.Promise.then_(_ => getWord(~set="verbs_past"))
  |> ignore;
  None;
});
```

We also moved the `ignore` part to the effect code. Now, we get a noun from
Wordbot, and then, once we have a noun, we request a verb in the past tense
(we’re using verbs in the past tense because they’re already conjugated). This
allows us to simply add a new line to request the next type of word, an object:

```reason
React.useEffect0(() => {
  getWord(~set="nouns")
  |> Js.Promise.then_(_ => getWord(~set="verbs_past"))
  |> Js.Promise.then_(_ => getWord(~set="objects"))
  |> ignore;
  None;
});
```

Finally, let’s add a case to get an adverb. For adverbs, we’ll first generate a
random number from 0 to 99 and only request an adverb if the number is less than
30:

```reason
React.useEffect0(() => {
  getWord(~set="nouns")
  |> Js.Promise.then_(_ => getWord(~set="verbs_past"))
  |> Js.Promise.then_(_ => getWord(~set="objects"))
  |> Js.Promise.then_(_ =>
       switch (Random.int(100)) {
       | n when n < 30 => getWord(~set="adverbs")
       | _ => Js.Promise.resolve()
       }
     )
  |> ignore;
  None;
});
```

And that’s it! The application is now returning proper sentences most of the
time even though most of them don’t make sense...

> “a confederater punished a pasta strainer awkwardly”

As next steps, there are a lot of improvements that can be added to the
application. For example, the way to determine whether to use “a” or “an” can be
improved, it can use more than one sentence structure, and it can be turned into
a generator of different things, such as a nonsense haiku generator.

For the complete application, see <https://github.com/stackbuilders/nonsense>.
