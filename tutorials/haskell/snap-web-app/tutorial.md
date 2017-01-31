---
title: How to create a web application in Haskell with Snap
published: 2017-01-30
ghc: 8.0.1
lts: 7.18
libraries: snap-core-1.0.1.0 snap-server-1.0.1.1 heist-1.0.1.0 HDBC-sqlite3-2.3.3.1    
language: haskell
description: In this tutorial we show you how to build a simple TODO web app using Snap framework
author-name: Omar García
---

## Introduction

[Snap](http://snapframework.com/) is a simple and [fast](http://snapframework.com/blog/2010/11/17/snap-0.3-benchmarks)
web development framework for the Haskell programming language with a clean
[monads](https://wiki.haskell.org/Monad) oriented design. Snap apps are often
composed by [snaplets](http://snapframework.com/docs/tutorials/snaplets-tutorial),
auto-contained pieces of reusable functionalities that can be glued together to build a website.

In this tutorial we'll show you how to build a simple TODO web app using Snap,
[SQLite](https://www.sqlite.org/), and [Stack](http://docs.haskellstack.org/en/stable/README/)
as our Haskell build tool. The target app should deliver the following features:

 - List Todo items
 - Create a new Todo item
 - Mark a Todo item as Done
 - Delete a Todo item


## Getting Started

Snap comes shipped with a [scaffolding utility](http://snapframework.com/docs/quickstart)
to scrap some boilerplate. So, when you first run the `snap init` command inside an empty
directory, it will create a bunch of files for you. But let's leave that aside,
for now let's focus on how to create a Snap web app from scratch using Stack.

Firstly, let's [create a project using Stack](http://docs.haskellstack.org/en/stable/README/#start-your-new-project) by running the following command:

```
$ stack new todo-webapp
```

Then you can edit the cabal file to match the one provided in this tutorial. There's a couple of things you won't
understand, but don't worry about it. The important thing is the
`build-depends` section of your library, which should look like:

```
build-depends:       base                      >= 4.7     && < 5
                   , bytestring                >= 0.9.1   && < 0.11
                   , heist                     >= 1.0     && < 1.1
                   , map-syntax                >= 0.2     && < 0.5
                   , lens                      >= 4.12    && < 4.15
                   , mtl                       >= 2       && < 3
                   , snap                      >= 1.0     && <1.1
                   , snap-core                 >= 1.0     && <1.1
                   , snap-server               >= 1.0     && <1.1
                   , snap-loader-static        >= 0.9     && < 1.1
                   , text                      >= 0.11    && < 1.3
                   , xmlhtml                   >= 0.1     && < 0.3
                   , HDBC                      >= 2.3     && <= 2.5
                   , HDBC-sqlite3              == 2.3.*
                   , interpolate               >= 0.1 && < 0.2
```

Those are the required libraries for the application to be built. Most of them
belong to Snap's core like [`snap`](http://hackage.haskell.org/package/snap), [`snap-core`](http://hackage.haskell.org/package/snap-core) ,
[`snap-server`](http://hackage.haskell.org/package/snap-server)
and [`heist`](http://hackage.haskell.org/package/heist)(the HTML template library preferred by the Snap creators). There are
others libraries like [`HDBC`](http://hackage.haskell.org/package/HDBC) and
[`HDBC-sqlite3`](http://hackage.haskell.org/package/HDBC-sqlite3), which enable
interaction with our database, and some others like [`text`](http://hackage.haskell.org/package/text)
and [`interpolate`](http://hackage.haskell.org/package/interpolate) to
allow us immutable Unicode text manipulation and strings interpolation.

## Coding our way out to the browser

The heart of the Snap's snaplets infrastructure is state management. Based on that
a good start for this application will be defining a data structure to hold the state,
which includes the state of all snaplets used by our application. Let's define this type in a module called [Application](https://github.com/stackbuilders/tutorials/tutorials/haskell/snap-web-app/code/src/Todo/Application.hs).

```haskell
data App = App
  { _heist :: Snaplet (Heist App)
  , _conn  :: Connection
  }

```
This data structure contains a `_heist` field holding the
[Heist Snaplet](http://snapforbeginners.com/chapters/heist.html)  and a `_conn`
for the database connection. This is the right place to include all the snaplets you
want if you're aiming to build a bigger app. Bear in mind that usually you will
be using a connection pool to manage your database connections instead of suing
a simple connection like this sample app does. But don't worry about it, most of
the libraries for accessing databases are shipped with that feature, like for example [Persistent](https://hackage.haskell.org/package/persistent).

The `App` data structure is used to initialize the web app in the
[Site module](https://github.com/stackbuilders/tutorials/tutorials/haskell/snap-web-app/code/src/Todo/Site.hs#L139-L146)
like follows:

```haskell
app :: SnapletInit App App
app = makeSnaplet "app" "A todo application." Nothing $ do
  h <- nestSnaplet "heist" heist $ heistInit "templates"
  c <- liftIO $ Db.startConnection "todos.db"
  liftIO $ Db.checkMigrationOrMigrate c

  addRoutes routes
  return $ App h c
```
All snaplets initializers must be wrapped in a call to [`makeSnaplet`](http://hackage.haskell.org/package/snap-0.14.0.6/docs/Snap-Snaplet.html#v:makeSnaplet). That's why the `app` function uses `makeSnaplet`.
If you take a closer look, the initializer computation starts up Heist and the database connection.
It also configures routes and checks for migrations, but we'll get to that later.
So, this module is really important as it holds our application stack and resource handlers.

As for the [Main module](https://github.com/stackbuilders/tutorials/tutorials/haskell/snap-web-app/code/src/Todo/Main.hs),
it just bootstraps the web server using the previous snaplet created in the
function `app` and the application configuration. You rarely will need to modify
it, but we encourage you take a look at it and understand what happens there.

## Setting up the model and migrations

This application goes about managing Todo items, which requires a very simple
data type definition with fields for description, completion status and,
of course, a primary key. This is defined in the
[Db module](https://github.com/stackbuilders/tutorials/tutorials/haskell/snap-web-app/code/src/Todo/Db.hs)
and this is how it looks like using
[record syntax](https://en.wikibooks.org/wiki/Haskell/More_on_datatypes#Named_Fields_.28Record_Syntax.29):

```haskell
data Todo = Todo
  { todoID      :: Maybe Int
  , description :: Text
  , completed   :: Bool
  } deriving (Eq, Show, Read)
```
[Back in the Site module](https://github.com/stackbuilders/tutorials/tutorials/haskell/snap-web-app/code/src/Todo/Site.hs#L143), it was used a function called `checkMigrationOrMigrate`
receiving a database connection as parameter, while initializing our Todo App.
This function verifies if `todos` table exists in the database. If not then creates it.

```haskell
checkMigrationOrMigrate :: Connection
                        -> IO ()
checkMigrationOrMigrate conn = do
  schemaCreated <- tableExists conn "todos"
  unless schemaCreated $ createTodoTable conn
```

In order to achieve its purpose, `checkMigrationOrMigrate` relies on a couple of
additional functions like `tableExists`:

```haskell
tableExists :: Connection
            -> String
            -> IO Bool
tableExists conn tblName = do
  let query = [i| SELECT name
                  FROM sqlite_master
                  WHERE type='table' AND name=? |]
  name <- listToMaybe . fmap rowToValue <$> quickQuery' conn query [toSql tblName]
  return (name == Just tblName)
  where
    rowToValue :: [SqlValue] -> String
    rowToValue (snippetBody:_) = fromSql snippetBody
    rowToValue _ = error "Unable to read single value"
```

And `createTodoTable`:

```haskell
createTodoTable :: Connection
                -> IO ()
createTodoTable conn = do
  let query = [i| CREATE TABLE todos (
                  id INTEGER PRIMARY KEY,
                  description TEXT,
                  completed BOOLEAN)|]

  quickQuery' conn query []
  commit conn
```

The function `tableExists` may look kind of tricky at first sight. It runs a very specific query
to know if a table exists and then it maps
`listToMaybe . fmap rowToValue` over the query result, which should be the table name
in debate, to extract it enclosed as a `Maybe` data type. Then it applies
pattern matching to check if the table name is the expected one.

I guess you noticed the use of
[`quickQuery'`](http://hackage.haskell.org/package/HDBC-2.4.0.1/docs/Database-HDBC.html#v:quickQuery-39-).
This function allows you to prepare, execute and fetch results in a more compact and idiomatic way.
You'll see it around a lot.

## Storing information in a database

In order to deliver the expected features mentioned earlier the application has
to perform the following operations with Todo items against a database:

 - create
 - update
 - obtain
 - delete
 - list

So, what happens if we map those operations over some code?. Well, the result
are the following functions from the
[Db module](https://github.com/stackbuilders/tutorials/tutorials/haskell/snap-web-app/code/src/Todo/Db.hs).

The `saveTodo`function receives a `Todo` data value. If its `todoID` field, which has a
`Maybe Int` type, holds `Nothing`, then it inserts a new Todo item row in the
database, otherwise it just updates the existing one using its `todoID` field,
which will be like `Just <some-int-value>` by then.

```haskell
saveTodo  :: Connection
          -> Todo
          -> IO ()
saveTodo conn todo =
  maybe newTodo updateTodo (todoID todo)
  where
    newTodo :: IO ()
    newTodo = do
      let query = [i| INSERT INTO todos(description, completed)
                      VALUES(?,?)|]
      execQuery query [toSql $ description todo
                      ,toSql $ completed todo
                      ]
    updateTodo :: Int -> IO ()
    updateTodo todoid = do
      let query = [i| UPDATE todos SET description = ?, completed = ?
                      WHERE (id = ?)|]
      execQuery query [toSql $ description todo
                      ,toSql $ completed todo
                      ,toSql todoid
                      ]
    execQuery :: String -> [SqlValue] -> IO ()
    execQuery q p  = do
      quickQuery' conn q p
      commit conn
```

The `getTodo`function returns a specific Todo Item by its primary key. It's very straight
forward. It performs the query, turns the result into a `Todo` data value,
performs `listToMaybe` to extract the first item, as the previous operation will
return a `[Todo]`, and finally it pulls the `Todo` value using `fromMaybe`.

If you take a closer look, you'll realize the functions uses `listToMaybe` instead
of `head` and `fromMaybe` instead of `fromJust`, which apparently would be easier
approaches. That's because `head` and `fromJust` are partial functions, and
[you should always strive to avoid partial functions](https://wiki.haskell.org/Avoiding_partial_functions).

```haskell
getTodo :: Connection
        -> Int
        -> IO Todo
getTodo conn todoId = do
  let query = [i| SELECT id, description, completed
                  FROM todos
                  WHERE id = ?|]

  maybeTodo <- listToMaybe . rights . fmap rowToTodo <$> quickQuery' conn query [toSql todoId]
  let todo = fromMaybe undefined maybeTodo
  return todo
```

The `deleteTodo` function removes a specific Todo Item receiving its primary key as parameter.
It just runs the deletion query.

```haskell
deleteTodo :: Connection
        -> Int
        -> IO ()
deleteTodo conn todoId = do
  let query = [i| DELETE FROM todos
                  WHERE id = ?|]

  quickQuery' conn query [toSql todoId]
  commit conn
```

Finally, `allTodos` selects all the Todo items while organizing them with the completed
ones listed last. Bear in mind `quickQuery'` returns a `[[SqlValue]]` value in the
context of an [IO monad](https://wiki.haskell.org/IO_inside), so it uses a simple
and easy to understand [`rowToTodo`](https://github.com/stackbuilders/tutorials/tutorials/haskell/snap-web-app/code/src/Todo/Db.hs#L80-L87) function to turn those rows into `Todo`
data values. Notice it's used the [rights](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Either.html#v:rights)
function in order to filter the
correct results from applying `rowToTodo`, which returns an `Either String Todo`
type.

```haskell
allTodos  :: Connection
          -> IO [Todo]
allTodos conn = do
  let query = [i| SELECT id, description, completed
                  FROM todos
                  ORDER BY completed ASC|]
  rights . fmap rowToTodo <$> quickQuery' conn query []

```

## Now you can see

Great, all is ready now to serve some content. This is a simple application so
it has a unique view which consists in a Todo items list with a couple of buttons
to mark it as done or delete it, and a form for new items submission purpose. The following image gives you a sneak peak.

<p align="center">
  <img alt="Snap Todo web app" src="todo.png" style="width:50%;height:50%;" />
  </br>
</p>


This application uses Snap and therefore it seemed logical to use its default template engine: Heist.

Heist uses simple XML tags to bind values to your templates in a straightforward
way. It's a great library which even allows to use compiled or interpreted templates,
but that's out of the scope of the tutorial. Anyway, you should know this
tutorial uses interpreted templates, and also another structure called [Splice](http://snapframework.com/docs/tutorials/heist#heist-programming) to pull
values from. You'll see more about that in a bit.

This is an extract of how the [todos.tpl](https://github.com/stackbuilders/tutorials/tutorials/haskell/snap-web-app/code/snaplets/heist/templates/todos.tpl) Heist template for this tutorial
application looks like. The submission form was left out for the sake of simplicity

```html
<apply template="base">
  <div class="col-xs-6 col-xs-offset-3">
    <div class="well">
      <h1>TODO Items</h1>
      <ul class="list-group">
        <todos>
          <input type="hidden" value="${id}">
          <li class="list-group-item clearfix ${successClass}">
            <span><description/></span>
            <div class="pull-right" role="group">
              <a class="btn btn-xs btn-success img-circle"
                  onclick="document.getElementById('form-check-${id}').submit();">
                  ✓
              </a>
              <form id="form-check-${id}" action="/todos/${id}/complete/${notCompleted}" method="post" class="hidden-form">
              </form>
              <span> </span>
              <a class="btn btn-xs btn-danger img-circle"
                  onclick="document.getElementById('form-rmv-${id}').submit();">
                Ｘ
              </a>
              <form id="form-rmv-${id}" action="/todos/${id}/delete" method="post" class="hidden-form">
              </form>
            </div>
          </li>
        </todos>
      </ul>
      <div>
        <hr>
        <div class="clearfix">
          <!-- forms goes here  -->
        </div>
      </div>
    </div>
  </div>
</apply>
```
Lets explain this a little bit. The `<apply template="base">` tag means this
view uses a layout Heist template called `base.tpl`, you can look at
it [here](https://github.com/stackbuilders/tutorials/tutorials/haskell/snap-web-app/code/snaplets/heist/templates/todos.tpl). The `<todos>` node implies everything it encloses will be repeated
for every single Todo item. That comes from the Todo index request handler

```haskell
handleIndexTodo :: Handler App App ()
handleIndexTodo = do
  c <- gets _conn
  todos <- liftIO $ Db.allTodos c
  renderWithSplices "todos" $
      "todos" ## spliceForTodos todos
```

which binds the `todos` tag with the splice obtained as result after calling `spliceForTodos` function.

```haskell
spliceForTodos :: [Db.Todo] -> SnapletISplice App
spliceForTodos = mapSplices $ runChildrenWith . spliceForTodo
```

This `spliceForTodos` function uses another one called `spliceForTodo`, for
creating a splice that binds Todo item values.

```haskell
spliceForTodo :: Db.Todo -> Splices (SnapletISplice App)
spliceForTodo todo = do
  "id"  ## textSplice . showAsText $ fromMaybe 0 ( Db.todoID todo )
  "description"  ## textSplice $ Db.description todo
  "completed"  ## textSplice . showAsText $ Db.completed todo
  "notCompleted"  ## textSplice . showAsText $ not (Db.completed todo)
  "successClass" ## textSplice $  if Db.completed todo
                                    then "list-group-item-success"
                                    else ""
```

Those values are used back in our [todos.tpl](https://github.com/stackbuilders/tutorials/tutorials/haskell/snap-web-app/code/snaplets/heist/templates/todos.tpl#L7-L22) Heist template as the
`<description/>` tag or to build the **marks as done** link URL `/todos/${id}/complete/${notCompleted}`.

All this content is served through request handlers, which also control the
application flow. This handlers do their job because they're bound to specific
routes. Remember we talk about this while initializing the app?. Well, this is the route configuration provided to the application when calling the [`addRoutes`](http://hackage.haskell.org/package/snap-0.14.0.6/docs/Snap-Snaplet.html#v:addRoutes) function when it initializes.

```haskell
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",                                    handleIndexTodo)
         , ("/todos/new",                           method GET handleIndexTodo
                                                <|> method POST handleNewTodo)
         , ("/todos/:todoId/complete/:completed",   method POST handleCompleteTodo)
         , ("/todos/:todoId/delete",                method POST handleDeleteTodo)
         , ("/static",                              serveDirectory "static")
         ]

```
Notice it's not a common practice anymore to configure your routes using strings.
Libraries that provide type safe routing, like
[`web-routes-boomerang`](http://hackage.haskell.org/package/web-routes-boomerang),
are more welcome these days.

## Handling POST Requests

So far now we've covered how to show the list of Todo items, but we still have
to create new Todo items, mark them as done and also delete them.

Creating a new Todo item is quite easy. The application should have a handler
for the incoming POST request, which contains a POST param with the description
of the Todo item. Just like this:

```haskell
handleNewTodo :: Handler App App ()
handleNewTodo = do
  maybeDescriptionBs <- getPostParam "description"
  case maybeDescriptionBs of
    -- Note: for a real app, you should validate a required
    -- 'description' parameter using digestive functors.
    -- In this example we just ignored that and redirect to '/'
    -- for the sake of simplicity.
    Nothing -> redirect "/"
    Just descriptionBs -> do
        c <- gets _conn
        let todo = Db.Todo Nothing (byteStringToText descriptionBs) False
        liftIO $ Db.saveTodo c todo
        redirect "/"

```

The handler grabs the `description` param as a `Maybe ByteString` using
 [`getPostParam`](http://hackage.haskell.org/package/snap-core-0.9.8.0/docs/Snap-Core.html#v:getPostParam),
 check it isn't `Nothing`, creates a `Text` value from the `ByteString` one and
uses it to create a new `Todo` data value, which then gets passed to the
`Db.saveTodo`. All this before redirecting the request to the Todo index page(`/`).
Of course, this is the optimistic request's flow. If anything goes wrong we're
 just redirecting to `/` as well, just for the sake of simplicity.
 If you want go deeper on that, we recommend you to have a look at [digestive functors](https://hackage.haskell.org/package/digestive-functors) library.

Also, all the previous happens in the context of the [`Snap`](https://hackage.haskell.org/package/snap-core-0.9.8.0/docs/Snap-Core.html#t:Snap)
monad, which is the same as `Handler App App`, so if you want to perform a computation
in the context of the `IO` monad, like calling `Db.saveTodo`, you have to lift it
to the context of the current `Snap` monad using [`liftIO`](https://hackage.haskell.org/package/transformers-0.4.2.0/docs/Control-Monad-IO-Class.html).

[Mark a Todo item as done](https://github.com/stackbuilders/tutorials/tutorials/haskell/snap-web-app/code/src/Todo/Site.hs#L63-L86), which is just updating it, is also quite simple. It follows
the same approach with a couple of differences. First, it uses
 [`getParam`](http://hackage.haskell.org/package/snap-core-1.0.1.0/docs/Snap-Core.html#v:getParam)
instead of `getPostParam` because this time we're dealing with route params.
Remember the `:todoId` segment in routes definitions?. That means we can capture
a route param by calling `getParam "todoId"`.

After this, It uses a utility function called [`fromBSTo`](https://github.com/stackbuilders/tutorials/tutorials/haskell/snap-web-app/code/src/Todo/Utilities.hs#L32-L35) to extract the `ByteString` enclosed
Todo primary key in order to retrieve the `Todo` data value. This is used to feed
`Db.updateTodoCompleted` along with the updated `completed` field value.
`Db.updateTodoCompleted` function returns an updated `Todo` which then is saved to the database.

```haskell
let todoId = fromBSTo 0 todoIdBs :: Int
todo <- liftIO $ Db.getTodo c todoId
let updatedCompletedValue = fromBSTo False (fromMaybeBs maybeCompletedBs) :: Bool
liftIO $ Db.saveTodo c  (Db.updateTodoCompleted todo
                                                updatedCompletedValue)
```

Once understood this, it will be very easy for you to go over the [handler for
removing Todo items](https://github.com/stackbuilders/tutorials/tutorials/haskell/snap-web-app/code/src/Todo/Site.hs#L91-L104) by yourself. Its practically the same thing. It grabs the
Todo primary key and the passes it to the `Db.deleteTodo` function which do the
rest of the job.

## Wrapping up

That's it! We've just implemented a simple Todo App using Snap.

You might enjoy this other tutorial as well: [**CSV Encoding/Decoding**](https://github.com/stackbuilders/tutorials/blob/tutorials/tutorials/haskell/csv-encoding-decoding/tutorial.md).
