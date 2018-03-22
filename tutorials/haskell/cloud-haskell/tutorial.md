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

In this tutorial we will approach more interesting and advanced concepts of Cloud Haskell by implementing a simple chat server and client which supports the following features:
  * Launching a chat server room in a specific endpoint address that can be found by chat clients.
  * Launching chat clients that can search a chat server room in a specific endpoint address and connect to it.
  * Command line interface for writing messages in the chat room.
  * Broadcasting messages to all clients which are connected to a chat server room.
  * Handling the disconnection of clients from a chat server room.


## First steps: The chat types

As mentioned above, one of the goals of Cloud Haskell is to set up an expressive messaging model between processes. By expressive we mean that we
can specify which messages we want our process to handle, similar to specifying a communication protocol. This is achieved by pattern matching
over the messages a process must handle and specifying some policy for unhandled messages which do not match any of the handlers. Thus, the first
step in our implementation consists of defining the data types that will be signaled between our chat server and clients.

Initially, our chat server will have to handle two types of messages coming from clients, namely, 1) a message that lets a new client join the
chat and 2) messages to be broadcast to all the clients on the chat.

The first of this these messages is straightforward:


```Haskell
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}

module Types where

import GHC.Generics
import Data.Binary
import Data.Typeable.Internal
import Data.Map (Map)
import Control.Distributed.Process (SendPort)

type NickName = String

newtype JoinChatMessage = JoinChatMessage {
    clientName :: NickName
  } deriving (Generic, Typeable, Show)

instance Binary JoinChatMessage
```

Whenever a client wants to join a chat server, it provides a unique nickname that identifies it. Note that we are using Generics to derive instances for [Binary](https://hackage.haskell.org/package/binary-0.9.0.0/docs/Data-Binary.html) and [Typeable](https://hackage.haskell.org/package/base-4.10.1.0/docs/Data-Typeable.html). This is necessary so that our type can automatically be an instance of [Serializable](https://hackage.haskell.org/package/distributed-process-0.7.3/docs/Control-Distributed-Process-Serializable.html#t:Serializable) which is about “objects that can be sent across the network” or, in other words, objects that can be encoded to raw bytestrings and decoded again to their original form.

Our second type represents any message which is broadcast to the clients connected to a chat:


```Haskell
data Sender = Server | Client NickName
  deriving (Generic, Typeable, Eq, Show)

data ChatMessage = ChatMessage {
    from :: Sender
  , message :: String
  } deriving (Generic, Typeable, Show)


instance Binary ChatMessage
```

Note that we capture the fact that the sender of that message can be either a client or the server itself -For example, when a client connects to
the chat, the server broadcasts a message to the other clients announcing that a new member has joined.

Finally, with Cloud Haskell we can define processes which can update their state after handling a message. Thus we can define the type of the state
that the chat server process will update after a client joins:

```Haskell
...

import Control.Distributed.Process (SendPort)

...

type ClientPortMap = Map NickName (SendPort ChatMessage)
```

The state of our chat server process consists of a map from a client’s nickname (or identifier) to a send port. We’ll give more details about the
SendPort data type when we talk about channels. Meanwhile, we can think about this type as an inventory of the clients that join the chat and a
port through which we can send messages to them.