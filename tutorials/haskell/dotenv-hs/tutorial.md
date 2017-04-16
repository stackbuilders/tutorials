---
title: Setting up your environment with dotenv
published: not-yet
ghc: 8.0.2
lts: 8.4
libraries: dotenv-0.4.0.0
language: haskell
author-name: Cristhian Motoche
description: Check Out the two working modes of dotenv to set up an
environment before the execution of an application.
---
# Introduction
## What is dotenv?
[Dotenv][dotenv] is a Haskell library and an standalone tool that enables to set
up the configuration of an environment before the execution of an application.
As it is suggested in [The Twelve Factor App](https://12factor.net/config)
the configuration should be separated from the code. A good way to do that is
by loading **environment variables** (env vars) and `dotenv` is the perfect tool
for that, because it loads all the needed env vars for an application to run.

## Why it is important?
The configuration should be separated from the code for one simple reason: the
code should be the same in every environment (development, testing, staging,
quality assurance, production, etc.) but configuration is the opposite, it
changes in every environment.

Loading env vars is a good way to set up the configuration of an application,
because they can change easily every time the environment changes and without
changing code.

## How it works?
Dotenv reads the needed application's env vars defined in a *dotenv example file*
(necessary env vars). If the required env vars are defined in *the environment*,
they stay the same. If they are not there, then dotenv checks if they defined
in a *dotenv file* and sets all the env vars that are there. The application will
fail because we shouldn't allow an application to start with an invalid state.
This strategy helps us to prevent runtime errors.

# Examples
The example will be a simple application that prints the content of a table in a
database. Really simple, but it needs some configuration to run. The standalone
tool will be used first, and after that, the dotenv library.

[PostgreSQL][https://www.postgresql.org/download/] will be necessary to follow
this example. Download the next [example code][example] to follow step by step
the rest of this tutorial.

## Dotenv as an Standalone tool
First, install the dotenv standalone tool. Use `stack` to install it:

```shell
$ stack install dotenv
```

Once installed, check the help from the tool using the following command
`dotenv --help`:

```shell
$ dotenv --help

dotenv - loads options from dotenv files

Usage: dotenv PROGRAM [-e|--dotenv DOTENV] [-x|--dotenv-example DOTENV_EXAMPLE]
              [-o|--override]
  Runs PROGRAM after loading options from DOTENV FILE

Available options:
  -h,--help                Show this help text
  -e,--dotenv DOTENV       File with the env variables (default: ".env")
  -x,--dotenv-example DOTENV_EXAMPLE
                           File with all the necessary env
                           variables (default: ".env.example")
  -o,--override            Override existing variables
```

The minimum necessary is a **PROGRAM** to run. The paths for the *dotenv file*
and the *dotenv example file* can be set with the short flags `-e` and `-x`,
respectively. The default values are *.env* and *.env.example*, respectivelly.
The env vars defined in the environment can be overrided with the ones defined
in the *dotenv file* using the `-o` flag.

Now, go to the directory of the example and follow the next instructions:

Install the dependencies and build the project:

```shell
$ stack build
```

Two executable applications will be created: `dotenv-st` and `dotenv-code`. The
first one will be used for this part of the example.

Create the databases, one for development and the other one for testing:

```shell
$ createdb dotenv-dev
$ createdb dotenv-test
```

Load the data:

```shell
$ psql -d dotenv-dev  < sql/dotenv-dev.sql
$ psql -d dotenv-test < sql/dotenv-test.sql
```

Before executing the application, check out the *.env.example* file.

```shell
$ cat .env.example

DB_HOST=
DB_PORT=
DB_NAME=
DB_USER=
DB_PASS=
```

This file contains all the *necessary env vars* for the configuration of the
connection with the database: the host, the port, the name of the database,
the username and its password. Without these variables defined the application
will be stopped before start.

Also, check the *.env* file. The *.env.test* file is the same, except by the
`DB_NAME`.

```shell
$ cat .env

DB_HOST=localhost
DB_PORT=5432
DB_NAME=dotenv-dev
DB_USER=postgres
DB_PASS=postgres
```

The application will read this environment variables and get the connection
information to connect with the database. As the below code shows:

```haskell
readConfig :: IO ConnectInfo
readConfig =
  ConnectInfo
    <$> getEnv "DB_HOST"
    <*> fmap read (getEnv "DB_PORT")
    <*> getEnv "DB_NAME"
    <*> getEnv "DB_USER"
    <*> getEnv "DB_PASS"
```

Run the `dotenv-st` application:

```shell
$ stack exec dotenv-st
dotenv-st: DB_HOST: getEnv: does not exist (no environment variable)
```

The error displayed in the screen says that the `DB_HOST` environment variable
must exist before executing the application. Let's execute it again, but
this time using `dotenv`.

```shell
$ dotenv "stack exec dotenv-st"
User {id_ = 1, name = "User 1", lastname = "User 1", birthDate = 1990-01-01}
User {id_ = 2, name = "User 2", lastname = "User 2", birthDate = 1990-01-01}
User {id_ = 3, name = "User 3", lastname = "User 3", birthDate = 1990-01-01}
```

The application was executed without any problem. Awesome! Let's run the
tests for the application with `dotenv`.

```shell
$ dotenv "stack test"
...
User.User
  findAllUsers
    finds all the users FAILED [1]

Failures:

  test/User/UserSpec.hs:25:
  1) User.User.findAllUsers finds all the users
       expected: [User {id_ = 1, name = "User 1", lastname = "User 1", birthDate = 1990-01-01}
                 ,User {id_ = 2, name = "User 2", lastname = "User 2", birthDate = 1990-01-01}
                 ,User {id_ = 3, name = "User 3", lastname = "User 3", birthDate = 1990-01-01}
                 ,User {id_ = 4, name = "User 4", lastname = "User 4", birthDate = 1990-01-01}]

        but got: [User {id_ = 1, name = "User 1", lastname = "User 1", birthDate = 1990-01-01}
                 ,User {id_ = 2, name = "User 2", lastname = "User 2", birthDate = 1990-01-01}
                 ,User {id_ = 3, name = "User 3", lastname = "User 3", birthDate = 1990-01-01}]

Randomized with seed 1355675075

Finished in 0.0494 seconds
1 example, 1 failure

Completed 2 action(s).
Test suite failure for package dotenv-hs-example-0.1.0.0
```

The tests failed, because the database for tests should be used insted of the
development one. Run the tests again giving a different *dot env* file, in this
case *.env.test*.

```shell
$ dotenv "stack test" -e .env.test
dotenv-hs-example-0.1.0.0: test (suite: dotenv)


User.User
  findAllUsers
    finds all the users

Finished in 0.0357 seconds
1 example, 0 failures
```

Yey! The test pass! The `dotenv` standalone tool can be used with any application
that needs env vars.

## Dotenv as a Library
To use `dotenv` as a library import the next modules in the code:

```haskell
import Configuration.Dotenv
import Configuration.Dotenv.Types
```

Define the dotenv configuration, using the `Config` type. For this example:

```haskell
config :: Config
config =
  Config
    { configExamplePath = ".env.example"
    , configOverride = True
    , configPath = ".env"
    }
```

Then use the `loadFile` function to load your env vars given `config`.

```haskell
main :: IO ()
main = do
  void $ loadFile config
  conn <- readConfig >>= getConnection
  users <- findAllUsers conn
  forM_ users print
```

As simple as that! Let's execute the application.

```shell
$ stack exec dotenv-code
User {id_ = 1, name = "User 1", lastname = "User 1", birthDate = 1990-01-01}
User {id_ = 2, name = "User 2", lastname = "User 2", birthDate = 1990-01-01}
User {id_ = 3, name = "User 3", lastname = "User 3", birthDate = 1990-01-01}
```

The env vars will be in the environment before the rest of the application starts.

# Conclusion
Safety and complete set up configuration before execution is assured with `dotenv`
Safety, because the execution of the application will stop if the required env
vars are not defined the environment or in the *dotenv file*. And complete
configuration, because the necessary env vars will be set up after `dotenv` execution.
Now, go ahead and use `dotenv` everywhere!

[dotenv]: https://github.com/stackbuilders/dotenv-hs
[example-code]: https://github.com/CristhianMotoche/tutorials/tree/how_to_use_dotenv/tutorials/haskell/dotenv-hs/code
