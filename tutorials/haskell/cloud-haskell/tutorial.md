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


## Why Cloud Haskell?

Programming concurrent and distributed applications is way too hard. On top of the several challenges that you face when designing software, you
will also struggle with race conditions, deadlocks, bad communication protocols, network problems that are hard to detect and to recover from, and
code that is hard to debug and to maintain. That’s why Erlang was invented: Erlang is a language for manipulating distributed systems that focuses
on recovery from failure. Additionally, Erlang brought the possibility to write distributed programs in a functional style. All this is pretty
interesting. However, it still lacks type-level guarantees since Erlang is a dynamically typed language, and we cannot model our concurrent
programs as type-safe and predictable communication protocols. Cloud Haskell fills this gap by providing Erlang’s powerful distributed model
shielded by Haskell’s powerful type system, so you can write your distributed programs with the robustness of Haskell and the error recovery from
Erlang.

## Overview

As an overview, let’s see how Cloud Haskell makes use of Erlang’s model by analyzing a very simple example.  First, Cloud Haskell’s most
fundamental entity is a process. Processes are isolated and lightweight threads of execution which run in a node, and the only way they can
interact is by passing messages between each other. This is why processes are highly isolated since they do not share resources, which is the main
cause of deadlocks and race conditions in distributed and concurrent systems. Having this in mind, sending a message to a process is as easy as
creating a node for the process to reside and sending a message to it with its unique process-id:
