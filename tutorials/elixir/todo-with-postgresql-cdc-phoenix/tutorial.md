---
title: Reactive TODO list with Postgresql, CDC and Phoenix
published: 2021-04-16
tags: elixir, CDC, Postgresql, socket, phoenix, live view
libraries: servant-auth-cookie-0.3.0.2 servant-0.8
language: elixir
author: Cristopher Rodrigues
author-name: Cristopher Rodrigues
github-profile: cristopher-rodrigues
description: Learn how CDC can be leveraged to handle data changes, and how to use Phoenix to build an
application on top of it.
---

At Stack Builders, we're constantly thinking out of the box while looking for pragmatic software solutions.
We're about to see how CDC is important and can help you interact with data change much smoother, reducing
this practice's friction (might take architectural aspects into account). It hasn't been clear how
straightforward it can be to use within the Elixir ecosystem, and we're going to demonstrate one of the most
naive ways we can easily achieve it.

To experiment with that, we will see how a naive TODO would be done on top of CDC in conjunction with the
Phoenix ecosystem. The main goal is to visualize how this approach fits in many other scenarios and bring
you to meet and brief these powerful technologies.

## Setup Phoenix

Considering you already have [Elixir](https://elixir-lang.org/install.html) and
[PostgreSQL](https://www.postgresql.org/download/) installed on your machine, you
might consider using the below docker-compose structure used for this Demo:

```yaml
version: "3.4"
volumes:
 cdc_demo:
services:
 postgres:
   container_name: cdc_demo_postgresql
   image: postgres:12.0-alpine
   restart: always
   environment:
     - POSTGRES_USER=postgres
     - POSTGRES_PASSWORD=postgres
     - POSTGRES_DB=cdc_demo_dev
   ports:
     - "5432:5432"
   volumes:
     - cdc_demo:/var/lib/postgresql/data
   logging:
     options:
       max-file: "5"
       max-size: "10m"
```

Let's install [Phoenix](https://hexdocs.pm/phoenix/installation.html):

```bash
mix archive.install hex phx_new
```

Now, after choosing your projects directory, we will
[generate a new Phoenix](https://hexdocs.pm/phoenix/up_and_running.html) (Live) project:
* When asked if you want to install the dependencies type Y

```bash
# In this case, I choose cdc_demo but feel free to choose any other name you'd like to.

mix phx.new cdc_demo --live
```

Following the next instructions suggested by the generator, let's configure the PostgreSQL connection in
`config/dev.exs` using your own Database credentials:


```elixir
# Configure your database
# If you choose to use the Docker compose suggested above, no changes are needed.
config: cdc_demo, CdcDemo.Repo,
username: "postgres",
password: "postgres",
database: "cdc_demo_dev",
hostname: "localhost",
show_sensitive_data_on_connection_error: true,
pool_size: 10
```

To save us time, we're going to use a [Phoenix generator](https://hexdocs.pm/phoenix/Mix.Tasks.Phx.Gen.Html.html)
to generate the TODO scaffolding, which is going to generate most pieces we need to have the TODO CRUD:

```bash
mix phx.gen.html Todos Todo todos todo:text marked_as_done:boolean --table todos --web Todos
```

As suggested by the output, we should run the migration:

```bash
mix ecto.migrate
```

And, update our router `lib/cdc_demo_web/router.ex`

```elixir
scope "/todos", CdcDemoWeb.Todos, as: :todos do
    pipe_through :browser

    resources "/todos", TodoController
end
```

If everything went well, you should be able to find the **todos** table on your database, and we should be
able to see all tests passing:

```bash
mix test
```

```bash
...........

Finished in 0.5 seconds
19 tests, 0 failures

Randomized with seed 345370
```

Try starting your Phoenix server using mix:

```bash
mix phx.server
```

And, access [http://localhost:4000/](http://localhost:4000/) on your preferred web browser.

<small>*Do not proceed if you can't see the Phoenix Welcome page.*</small>

We should also be able to access the TODOs page [http://localhost:4000/todos/todos](http://localhost:4000/todos/todos).
Try playing with it to see how things work.

### CDC

> "...change data capture (CDC) is a set of software
> [design patterns](https://en.m.wikipedia.org/wiki/Design_pattern_(computer_science))
> used to determine and track the data that has changed so that action can be
> taken using the changed data.” - Wikipedia.

CDC has been applied in an assortment of different and complex scenarios, for example, auditing, data-replication,
analytics systems, and many others; however, we're going to use this simple TODO example to learn a bit more
about this technology.

###  Capturing changes

This post will be using the [Cainophile](https://github.com/cainophile/cainophile) library to simplify the setup
on how the events are exposed from PostgreSQL to the app layer.

First, let's prepare our Database; for it, we need to create a
[PostgreSQL publication](https://www.postgresql.org/docs/12/logical-replication-publication.html) only for the todos table:

```bash
mix ecto.gen.migration create_todos_publication
```

Replace the file content with the following:

`priv/repo/migrations/20210314133342_create_todos_publication.exs`

<small>*The timestamp at the beginning will be different*</small>

```elixir
defmodule CdcDemo.Repo.Migrations.CreateTodosPublication do
    use Ecto.Migration

    def up do
        execute("CREATE PUBLICATION todos_publication FOR TABLE todos;")
    end

    def down do
        execute("DROP PUBLICATION todos_publication;")
    end
end
```

Now, let's set todos table replica identity to FULL

```bash
mix ecto.gen.migration change_todos_replica_identity_to_full
```

We should also replace its content:

`priv/repo/migrations/20210314133715_change_todos_replica_identity_to_full.exs`

<small>*The timestamp at the beginning will be different*</small>


```elixir
defmodule CdcDemo.Repo.Migrations.ChangeTodosReplicaIdentityToFull do
    use Ecto.Migration

    def up do
        execute("ALTER TABLE todos REPLICA IDENTITY FULL;")
    end

    def down do
        execute("ALTER TABLE todos REPLICA IDENTITY DEFAULT;")
    end
```

Finally, we need to enable PostgreSQL
[logical replication](https://www.postgresql.org/docs/12/runtime-config-wal.html).


```bash
mix ecto.gen.migration alter_system_wal_level_to_logical
```

```elixir
defmodule CdcDemo.Repo.Migrations.AlterSystemWalLevelToLogical do
    use Ecto.Migration

    @disable_ddl_transaction true # needed to avoid ALTER SYSTEM cannot run inside a transaction block

    def change do
        execute("ALTER SYSTEM SET wal_level = 'logical';")
    end
end
```

Let's apply the new migrations:

```bash
mix ecto.migrate
```

You should be able to find the **todos_publication** and the **todos** table replica identity on your
own PostgreSQL instance:


```SQL
SELECT * FROM pg_publication WHERE pubname = 'todos_publication';
SELECT relreplident FROM pg_class WHERE oid = 'todos'::regclass;
```

We need to restart the PostgreSQL server to apply the **wal_level**. If you're using the suggested Docker compose,
simply run:

```bash
docker-compose restart
```

After restarting the PostgreSQL server, you should be able to see the new value to **wal_level**.
```bash
show "wal_level"; -- logical
```

Now, PostgreSQL is prepared and ready for logical replication; so let's set up our App.

First, we need to add Cainophile to our project dependencies:

`mix.exs`
```elixir
{:cainophile, "~> 0.1.0"}
```

And install it by running:

```bash
mix deps.get
```

A quick break to explain the strategy we're going to use in the post with Cainophile to listen to events.
As described in the Cainophile doc page, there are several ways we could approach it within a Phoenix app.
Still, we’re going to use a naive version of a GenServer (no worries, if you're not familiar with the
[GenServer](https://hexdocs.pm/elixir/GenServer.html) concept, for now, just think about it as an Event
Subscriber - for this case).

We already have our TODO CRUD working on Phoenix, and now we will build an isolated
[Phoenix Live View](https://github.com/phoenixframework/phoenix_live_view) screen for the reading part to
demonstrate how we'd use PostgreSQL CDC in this case. First, let's add the required Cainophile config to our `config/dev.exs`.

```elixir
config :cdc_demo, Cainophile.Adapters.Postgres,
    register: Cainophile.CdcDemoPublisher, # name this process will be registered globally as, for usage with Cainophile.Adapters.Postgres.subscribe/2
    epgsql: %{
        host: 'localhost',
        username: "postgres",
        database: "cdc_demo_dev",
        password: "postgres"
    },
    slot: :temporary,
    wal_position: {"0", "0"},
    publications: ["todos_publication"]
```

Then, we will add the subscriber (GenServer), that will subscribe to the registered Cainophile process (which will be added
to the App Supervision tree very shortly) and will be listening to Database changes on the **todos** table:

`lib/cdc_demo/todos/subscriber.ex `

```elixir
defmodule CdcDemo.Todos.Subscriber do
 use GenServer

 alias Cainophile.{Adapters, Changes}

 def start_link(_) do
  GenServer.start_link(__MODULE__, %{})
 end

 @impl true
 def init(state) do
  Adapters.Postgres.subscribe(register(), self()) # subscribe to the Cainophile process in order to receive "todos" events (see handle_info bellow)

  {:ok, state}
 end

 @impl true
 def handle_info(
   %Changes.Transaction{
     changes: changes
   },
   state
 ) do
   IO.inspect changes # temporary debug code

   {:noreply, state}
  end

 @impl true
 def handle_info(_, state), do: {:noreply, state}

 defp register do
  :cdc_demo
  |> Application.fetch_env!(Cainophile.Adapters.Postgres)
  |> Keyword.fetch!(:register) # this is the register we set above in config/dev.exs
 end
end
```

Now, we need to add the GenServer under our application
[supervision three](https://elixir-lang.org/getting-started/mix-otp/supervisor-and-application.html):

`lib/application.ex`

```elixir
children = [
     # ...
     CdcDemoWeb.Endpoint,
     {
       Cainophile.Adapters.Postgres,
       Application.fetch_env!(:cdc_demo, Cainophile.Adapters.Postgres)
     },
     CdcDemo.Todos.Subscriber
   ]
```

The server should be running fine, and if you add the Cainophile config to config/test.exs as we've done for dev,
the tests should also be passing (Note you need to set the correct database for the test, e.g.,
`"cdc_demo_test#{System.get_env("MIX_TEST_PARTITION")}")`

So, let's see it in action!

Start the Phoenix server via [IEX](https://hexdocs.pm/iex/IEx.html):

```bash
iex -S mix phx.server
```

Once it starts, try creating a TODO in the IEX prompt:

```bash
iex(3)> CdcDemo.Todos.create_todo(%{todo: "testing"})
```

If the TODO was successfully created, you should be able to see the inspect we added on our GenServer:

```bash
 [
  %Cainophile.Changes.NewRecord{
    record: %{
      "id" => "1",
      "inserted_at" => "2021-03-14 14:54:26",
      "marked_as_done" => "f",
      "todo" => "testing",
      "updated_at" => "2021-03-14 14:54:26"
    },
    relation: {"public", "todos"}
  }
 ]
```

Let's try to update an existing TODO:
```bash
iex(4)> 1 |> CdcDemo.Todos.get_todo!() |> CdcDemo.Todos.update_todo(%{todo: "testing again"})
```

As expected, we should also see something similar to:

```bash
 [
  %Cainophile.Changes.UpdatedRecord{
    old_record: %{
      "id" => "1",
      "inserted_at" => "2021-03-14 14:54:26",
      "marked_as_done" => "f",
      "todo" => "testing",
      "updated_at" => "2021-03-14 14:54:26"
    },
    record: %{
      "id" => "1",
      "inserted_at" => "2021-03-14 14:54:26",
      "marked_as_done" => "f",
      "todo" => "testing again",
      "updated_at" => "2021-03-14 14:59:23"
    },
    relation: {"public", "todos"}
  }
]
```

As we saw above, the struct Cainophile.Changes.NewRecord represents an inserted record and the struct
Cainophile.Changes.UpdatedRecord represents an updated record. Both are
[Cainophile internal data structures](https://github.com/cainophile/cainophile/blob/3c35c974541bd6cc0ed150d771c54e63c96ad9ea/lib/cainophile/changes.ex#L3).

To make things even simpler, we're going to use the existing LiveView components that Phoenix generated.

Replace `lib/cdc_demo_web/live/page_live.html.leex content` with the following:


```html
<%= for todo <- @todos do %>
 <h1><%= todo.todo %></h1>
<% end %>
```
<small>*We're just looping through a variable **@todos** and displaying its **todo** property.*</small>


Now, let's replace the `lib/cdc_demo_web/live/page_live.ex` with the following:

```elixir
defmodule CdcDemoWeb.PageLive do
  use CdcDemoWeb, :live_view

  alias Phoenix.Socket.Broadcast

   @impl true
   def mount(_params, _session, socket) do
     if connected?(socket) do
       Phoenix.PubSub.subscribe(CdcDemo.PubSub, "todos")
     end

     todos = CdcDemo.Todos.list_todos()

     {:ok, assign(socket, todos: todos)}
   end

   @impl true
   def handle_info(%Cainophile.Changes.Transaction{
     changes: [
     %Cainophile.Changes.NewRecord{
       record: new_todo,
       relation: {"public", "todos"}
     }
   ]
   }, state) do
     CdcDemoWeb.Endpoint.broadcast_from!(self(), "todos", "new_todo", %{todo: new_todo})

   {:noreply, state}
  end
end
```

As you probably guessed, we are subscribing to a topic todos in the
[Phoenix PubSub](https://hexdocs.pm/phoenix_pubsub/Phoenix.PubSub.html) and initializing the
view todos state with all existing todos. Then, we handle a Broadcast that matches the event "new_todo"
to append the current Socket "todos" assignment with the incoming new todo.

Running the server using IEX (as already done above) we can access our project home page at
[http://localhost:4000/](http://localhost:4000/) and, test the live view by sending a new todo
by broadcasting a fake message:

It should be something similar to:

![First look at todos](/tutorials/elixir/todo-with-postgresql-cdc-phoenix/elixir-todos.png)

Finally, we're going to connect our GenServer Subscriber to our Live View, by replacing its handle_info
method with the following:

```elixir
@impl true
 def handle_info(%Cainophile.Changes.Transaction{
   changes: [
     %Cainophile.Changes.NewRecord{
       record: new_todo,
       relation: {"public", "todos"}
     }
   ]
 }, state) do
   CdcDemoWeb.Endpoint.broadcast_from!(self(), "todos", "new_todo", %{todo: new_todo})

   {:noreply, state}
 end
```

As you see, now we match the `NewRecord` struct, expecting only inserted `todos` and we broadcast the incoming one to the live view topic.

***Side Note**: You might ask yourself why we need this entire Cainophile structure at all. Why not just broadcast it right after persisting the TODO on the Todos module? So, this approach gives us some benefits like better isolation. It reduces the complexity while testing a business module. It’s a much more agnostic architecture, and it allows us to listen for Database changes produced by a third-party system. Imagine an existing app with lots of modules dealing with the TODO table so that this approach would speed up the process, and you wouldn't even need to change the existing modules. In general, the standalone approach proposed here is just another way of approaching the problem.*

Restart the server and try creating another todo, after accessing the home page:

```bash
(1)> CdcDemo.Todos.create_todo(%{todo: "magic!"})
```


I want to challenge you to add the remaining event to our Subscriber and the Page Live View to handle
update and deletion operations, then access simultaneously (side-by-side) the home (view page)
and the new todo page ([http://localhost:4000/](http://localhost:4000/) -
[http://localhost:4000/todos/todos/new](http://localhost:4000/todos/todos/new)).


### Optional

If you want all tests to be passing, simply replace test/cdc_demo_web/live/page_live_test.exs with the following:


```elixir
defmodule CdcDemoWeb.PageLiveTest do
 use CdcDemoWeb.ConnCase

 import Phoenix.LiveViewTest

 test "disconnected and connected render", %{conn: conn} do
   {:ok, page_live, disconnected_html} = live(conn, "/")
   assert disconnected_html =~ "Get Started"
   refute render(page_live) =~ "Get Started"
 end

 test "render todos", %{conn: conn} do
   {:ok, todo1} = CdcDemo.Todos.create_todo(%{todo: "first todo"})
   {:ok, todo2} = CdcDemo.Todos.create_todo(%{todo: "second todo"})

   {:ok, page_live, _} = live(conn, "/")

   render_content = render(page_live)

   assert render_content =~ todo1.todo
   assert render_content =~ todo2.todo
 end
end
```

# Conclusion

This tutorial leveraged a simple TODO as an experiment for powerful tools like PostgreSQL
logical replication and the Phoenix ecosystem. Today, we saw how simple the setup can be for
such excellent technology and how great Phoenix is. With this naive example, we can now
extend the usage of both to any scenario we find helpful.

There are several other similar options on PostgreSQL; if you get interested, here are some
options that might be nice to check: Triggers, PG Notify, Pg Subscription, PgAudit, and the
native pg_statement.

The code used in this post can be found at [cdc_demo](https://github.com/cristopher-rodrigues/cdc_demo).

For more content check our [blogs](https://www.stackbuilders.com/news/page/1) and
[tutorials](https://www.stackbuilders.com/tutorials/).

Thanks for reading this tutorial! If you have any feedback, please feel free to drop us a line on
[Twitter](https://twitter.com/stackbuilders),
[LinkedIn](https://www.linkedin.com/company/stack-builders/mycompany/) or
[Facebook](https://www.facebook.com/StackBuildersEcuador/).
You could also open issues and pull requests on [GitHub](https://github.com/stackbuilders/tutorials).
