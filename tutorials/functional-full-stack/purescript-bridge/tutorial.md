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
In Stackbuilders we are working on a full-stack app with a client using functional languages.
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

### purescript-bridge to the rescue
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
* TODO: Describe the idea

### Backend
* TODO: Describe the Haskell backend and API endpoints

### Frontend
* TODO: Describe the Purescript frontend and how it uses the API endpoints

### Changing the app
* TODO: Implement a minor change on the backend that causes the frontend to fail on runtime
* TODO: Fix the frontend
* TODO: Show how clunky is having an app this way


## Tutorial
### Connecting the types on the backend to the frontend
* TODO: How to change the backend to enable it to use puescript-bridge
* TODO: How to change the frontend to use purescript-bridge generated results

### Using generics to simplify communication
* TODO: Changing backend endpoints to use generic fromJson/toJson
* TODO: Changing frontend endpoints to use generic fromJson/toJson

### Changing the app again
* TODO: Implement another minor change on the backend
* TODO: Run the new toolchain to auto-extend the change
* TODO: The frontend doesn't typecheck. Fix it.
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

* TODO: Next steps, and where to read more
