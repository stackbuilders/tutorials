---
title: Connecting a Haskell Backend to a Purescript Frontend
published: not-yet
ghc: 7.10.3
lts: 5.18
libraries: hspec-2.2.3 QuickCheck-2.8.1
language: haskell purescript
author-name: Javier Casas Velasco
description: In this tutorial we will implement a way to extend the types in the Haskell backend to the Purescript frontend while maintaining consistency and simplifying communication.
---
# Connecting a Haskell Backend to a Purescript Frontend
## Introduction
At Stackbuilders we are working on a full-stack app with a client using functional languages.
We have a Haskell backend, based on Servant, that manipulates the database and offers some endpoints to a Purescript frontend,
that does all the React-like magic to show a really nice interface on the user's browser.
It's great because we have advanced types, purity and all the awesome benefits that offers the Functional world.
But not everything is perfect.

### Motivation
The problem is that we have two different codebases: one in Haskell and the other in Purescript.
The syntax is almost the same, but that small difference means we can't directly share files.
So we have types that describe the entities in the backend,
and we have the same types that describe the same entities in the frontend, in different files.
And every now and then, the backend team changes something on the backend types,
but forgets to change it on the front end types.
Suddenly we start to get the dreaded runtime errors,
because the frontend is no longer able to decode the data sent from the backend,
because that data no longer conforms to the standard the frontend expects.

Ideally, we would separate the common types to some files, and use these files in the backend and the frontend.
Then, the frontend inevitably follows the changes in types from the backend,
and refuses to compile if the change is too big and a developer has to look at it.
But, again, Haskell code is almost like Purescript code, but not completely compatible.
So this is not possible.

### The next best thing
Well, if we can't use the same files, we have to look for something not that far from that ideal world.
If we can somehow automatically generate the Purescript code from the Haskell code,
we could prevent the problem of type difference. We would effectively extend the typesystem from the backend to the frontend.

### `purescript-bridge` to the rescue
Turns out this idea is not new, and Robert Klotzner has already done it for us, which is quite nice.
From the docs, purescript-bridge tells us it will write Purescript code from Haskell types,
as long as those types conform to some restrictions.
But let's not talk about limitations. Instead, let's talk about awesomeness. But, before that, let's review the general architecture.

## Simple WebApp
Our app will be split in two parts:
 * A Haskell backend that talks to the database, coordinates people, sends emails and all that awesome stuff backends do.
 * A Purescript frontend that compiles to Javascript and runs on the browser;
   showing, in marvellous details using React, all the data that fetches from the backend.

The two parts have to talk to eachother in order to have something useful.
We will use REST and JSON. That's it, the frontend will send HTTP requests full of JSON messages to the backend, in order to trigger actions;
and the backend will respond to those requests with more JSON full of data, to be shown to the user.

### WebApp idea
So we are going to build the next big thing. The website everyone definitely needs in their lives: a scientist browser, where we can browse names, photos and biographies of renowed scientists. Well, it may not be the next big thing, but you will definitely need it. I promise.

### Backend
The backend is going to be simple: trusty Servant is going to provide us with endpoints that talk JSON and are full of scientist biographies. The API will provide (for now) a single endpoint:
* GET /scientist/ : return a list of scientist biographies

```haskell
data Scientist = Scientist
  { sId        :: Int
  , sFirstName :: String
  , sLastName  :: String
  } deriving (Eq, Show)
```

### Frontend
The frontend is (somewhat) simple. It's going to be the Pux starter app with small modifications to pull data from our backend. After pulling the data, it's going to show the scientists one at a time, offering buttons to see the next or the previous one.

![The Best Thing Since Sliced Bread: the Scientist Viewer!](scientist_viewer_1.png "The Best Thing Since Sliced Bread: the Scientist Viewer!")

_The Best Thing Since Sliced Bread: the Scientist Viewer!_

### Changing the app
Did you know we have already ran into limitations?
Some scientists don't just have a name and surname.
My friend Gottfried Wilhelm Leibniz - the man behind Calculus - has a middle name. But that is not the only case.
Wernher von Braun was a rocket scientist, and he has `von` in the middle of his name. That's not technically a middle name, but we somehow have to accept it.
And Pythagoras, one of the pioneers of Geometry, well, we have no idea of his surname, if he ever had one.

So it's time to change the name format.

```haskell
 data Scientist = Scientist
  { sId        :: Int
  , sNames     :: [String]
  } deriving (Eq, Show)
```

![](error_on_frontend_2.png "Runtime errors? I hate runtime errors!")

_Runtime errors? I hate runtime errors!_

Ooops. We need to tweak the frontend to make it accept the new format.

```haskell
data Scientist = Scientist {
                 id :: Int
               , names :: Array String
               }

instance decodeScientist :: DecodeJson Scientist where
  decodeJson j = case toObject j of
                  Just o -> do
                    id <- o .? "sId"
                    names <- o .? "sNames"
                    pure $ Scientist {
                        id: id,
                        names: names
                      }
                  Nothing -> Left "Noparse"
```

![](frontend_works_again_2.png "Yay, rolling again!")

_Yay, rolling again!_

Ok, we can do that, but it's kinda silly, isn't it? I'm copying the same code from the backend to the frontend, from data structures to serializer algorithms. I can watch myself getting very annoyed because of this repetition. But, do you imagine what would happen if we happened to have a backend team and a frontend team? Unless the communication is excellent, we are going to have trouble every single day.


## Tutorial
### Connecting the types on the backend to the frontend
First of all, let's extend the types from the backend to the frontend.

* TODO: How to change the backend to enable it to use puescript-bridge
* TODO: How to change the frontend to use purescript-bridge generated results

### Using generics to simplify communication
But that's not enough. Although the types are the same, the JSON instances are not the same. And they should be. But having to copy instances from the backend to the frontend is kind of silly. All I want is to copy this backend data to the frontend, where both use an equivalent representation! There has to be a way to do that automatically.

* TODO: Changing backend endpoints to use generic fromJson/toJson
* TODO: Changing frontend endpoints to use generic fromJson/toJson

Now we are rolling!

### Changing the app again
You know what? We don't need that much of an ID, but a photo would definitely help here. Let's change the types again.

* TODO: Implement another minor change on the backend

Now we run the bridge.

* TODO: Run the new toolchain to auto-extend the change

Oooopsie again.

* TODO: The frontend doesn't typecheck. Fix it.

The frontend no longer typechecks. Well, it's kinda obvious, we are trying to render an ID that no longer exists. Let's fix the view.

* TODO: Everything works!

### Analysing the result
Thanks to purescript-bridge, we have removed the mental tax on the shared types on the frontend.
The backend will generate those types for us, so we no longer have to care about them.

Also, the backend guys can change without thinking too much about compatiblity with the frontend,
because the tools we built will tell us when something is broken.

Even better, the communication has been simplified a lot.
We don't have to care anymore about message format, decoders and encoders, because Aeson and Argonaut, along with purescript-bridge, handles that for us.

And finally, the most awesome of all is that the type system is automatically consistent on the frontend and the backend.
We have successfully connected the two worlds; and, as a result, we have gained some extra safety and peace of mind.

## Conclusion
I shall thank Robert Klotzner for the awesome package he made.
Purescript-bridge is incredible in the sense that it helps us extend the wonders of a strong type system across boundaries,
 such as different subsystems and languages. Definitely purescript-bridge it is worth every bit it costs.

More information:
* purescript-bridge on Hackage: https://hackage.haskell.org/package/purescript-bridge
* The code in this tutorial: TODO
* purescript-bridge Github's repository: https://github.com/eskimor/purescript-bridge