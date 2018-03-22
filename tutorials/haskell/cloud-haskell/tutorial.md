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

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node


```Haskell
main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "4001" defaultTCPParameters
  node <- newLocalNode transport initRemoteTable
  _ <- runProcess node $ do
    -- get the id of this process
    self <- getSelfPid
    send self "Talking to myself"
    message <- expect :: Process String
    liftIO $ putStrLn message
  return ()
```

Even though the example above is not very interesting, it exposes some cool concepts about this platform: on the one hand, the process logic is
decoupled from the network layer so that you can inject any transport backend you like. In our example, we are using [createTransport](http://hackage.haskell.org/package/network-transport-tcp-0.6.0/docs/Network-Transport-TCP.html#v:createTransport) to create a TCP/IP transport layer, and then we pass it to [newLocalNode](https://hackage.haskell.org/package/distributed-process-0.7.3/docs/Control-Distributed-Process-Node.html#t:LocalNode) to create the node where our process will reside. Nevertheless, you could create a node with a different protocol like SSH, UDP, Unix Sockets, etc. On the other hand, as long as you have the unique identifier for a process, you can send a message to it. Thus, we can use [getSelfPid](https://hackage.haskell.org/package/distributed-process-0.7.3/docs/Control-Distributed-Process.html#v:getSelfPid) to get the id of our process and then make it [send](https://hackage.haskell.org/package/distributed-process-0.7.3/docs/Control-Distributed-Process.html#v:send) the message “Talking to myself” to itself. After that, the process [expects](https://hackage.haskell.org/package/distributed-process-0.7.3/docs/Control-Distributed-Process.html#v:expect) a message of type String which is queued in its mailbox. If no message of the expected type is in the mailbox, then the process will block
until one arrives. In this case, the process is immediately receiving the “Talking to myself” string.
