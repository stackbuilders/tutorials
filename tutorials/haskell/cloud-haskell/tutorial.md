---
title: Erlang programming style with Cloud Haskell: Implementing a chat
published: 2018-03-20
ghc: 8.0.2
lts: 9.17
tags: haskell, distributed-computing, erlang
libraries: distributed-process
language: haskell
author-name: Sebastian Pulido Gomez
github-profile: sebashack
description: This is missing...
---

## What is Cloud Haskell?

[Cloud Haskell](http://haskell-distributed.github.io/documentation.html) is a set of libraries that combines the power of Haskell’s type system with
[Erlang’s](https://www.erlang.org/) style of concurrency and distributed programming. This brings a lot of power in order to implement networking
applications that are fault tolerant, can work on several network transports, and have an expressive model of communication. By expressive we mean
that processes communicate explicitly by sending messages to each other rather than by sharing resources in memory. This also implies that
processes -running in the same local node or in a remote node- pattern match over the specific messages they can handle, which fits very well
Haskell’s capabilities of modeling messages with algebraic data types.
