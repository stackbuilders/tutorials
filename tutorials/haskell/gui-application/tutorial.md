---
title: Creating a GUI application in Haskell
published: 2016-06-29
update: 2016-06-29
ghc: 7.10.3
lts: 5.18
libraries: gtk3-0.14.4 glib-0.13.2.2
language: haskell
author: Mark
author-name: Mark Karpov
---

This tutorial shows how to build a GUI application in Haskell using bindings
to GTK+. While working on a calculator program we'll cover he following
topics:

* GTK+ terminology
* Writing handlers that process user's actions (button click, etc.)
* Introducing global state
* Manual creation of forms and creation of forms using Glade

Once you finish with the tutorial you will have solid understanding how to
move on, read the documentation of the
[`gtk3`](https://hackage.haskell.org/package/gtk3) package and accomplish
your tasks.

The tutorial does not assume any knowledge of Haskell except for very basic
understanding of how to work with `IO` monad. The GTK+ binding is very
straightforward and imperative in its nature. This may be seen as a
downside, but I think it also may make things easier for newcomers to
Haskell programming with imperative background.

## Available libraries

Before we start with GTK+ bindings, it's reasonable to ask whether there is
a better/alternative solution. Indeed, several libraries are available to
create GUI in Haskell:

* [`wx`](https://hackage.haskell.org/package/wx) is bindings to
  [wxWidgets](https://wxwidgets.org/). A couple of things about this package
  I find suspicious: 1) it had delays in development when for a couple of
  years no new version was released 2) it's still not present on Stackage.
  Practical conclusion from the point 2 is that it's not very popular
  nowadays, or at least not many Haskellers start writing any application
  with it otherwise it would be added already.

* [`X11`](https://hackage.haskell.org/package/X11) — *direct translation of
  the C binding to X11 graphics library* (quote taken from the package
  description). “Direct translation” means that you pass around pointers.
  Yes, in Haskell code. For documentation authors suggest look
  [here](https://tronche.com/gui/x/xlib/) (although Haddocks are not blank
  either). Last release was in May 2014.

* [`hsqml`](https://hackage.haskell.org/package/hsqml) — the Haskell
  bindings to
  [Qt Quick](https://wiki.qt.io/Category:Developing_with_Qt::Qt_Quick).
  Missing from Stackage. Releases seem to happen once a year.

* [`fltkhs`](https://hackage.haskell.org/package/fltkhs) — the Haskell
  bindings to the [FLTK GUI library](http://www.fltk.org/index.php). The
  package seems to be pretty new and I personally don't see any benefit in
  using it over more mature solutions.

As to GTK+, it seems to be:

* Easy to install.

* Robust and mature. Documentation is quite good and comprehensive.

It's worth mentioning that Haskell has developed much stronger ecosystem
with respect to web-development than GUI and desktop development in general.
For further reference about state of standalone GUI applications in Haskell,
refer to:

* The section in Gabriel Gonzalez's
  [State of Haskell Ecosystem](https://github.com/Gabriel439/post-rfc/blob/master/sotu.md#standalone-gui-applications)

* The [blog post](http://keera.co.uk/blog/2014/05/23/state-gui-programming-haskell/) by Keera Studios

## Installation

Please see [this article](https://wiki.haskell.org/Gtk2Hs/Installation) for
Gtk2Hs installation. It has instructions for Windows, Linux, Mac, and
FreeBSD.

## First steps

The calculator application has been chosen because its logic is very
straightforward and we can focus on working with GTK+ framework without much
distraction while keeping the tutorial reasonably practical.

Let's start by importing some modules:

```haskell
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)
```

As I said, GTK+ bindings are very imperative. All binding code lives in `IO`
monad except for some cases that we will cover shortly.

```haskell
main :: IO ()
main = do
  void initGUI          -- (1)
  window <- windowNew   -- (2)
                        -- (3)
  widgetShowAll window  -- (4)
  mainGUI               -- (5)
```

1. The first thing we need to do in any program that uses GTK+ is to call
   the `initGUI` function. This function allocates some resources and
   prepares GTK+ for work, it also looks up command line arguments that are
   relevant to GTK+, parses them, and returns all non-parsed arguments. For
   our purposes, we don't need the command line arguments, so let's wrap it
   with the `void`.

2. Next, we need a window to build interface of our calculator inside it. To
   create a new top-level window we use the `newWindow` action. It returns
   opaque `Window` value that can be used to manipulate the window.

3. After creation of a new window we typically want to change some
   parameters and then render it. For now we just render the window “as is”,
   but in the next section we will see how to customize widgets using
   *attributes*.

4. `widgetShowAll` works with any kind of widget. It performs all the
   necessary allocations and makes widget passed to it as an argument
   visible together with all its children widgets.

5. `mainGUI` is the main loop. The loop listens to events such as button
   click and mouse pointer movement and let appropriate handlers run.

A note about threads. Make sure that all GTK actions happen on the same OS
thread (note, this is different from lightweight Haskell threads). This only
important when you compile with multi-threaded runtime, but who knows, maybe
the need for concurrent execution will arise later, so my advice is to keep
all GTK-related code in one thread and simplest way to do it is to keep
everything is the main thread. For more information about multi-threaded
GUIs with GTK+ see [here](http://dmwit.com/gtk2hs/).

If we compile and run the program we will see the following:

![First look at the calculator application](/tutorials/haskell/gui-application/calc-0.png)

Nothing fancy. One nasty detail is that when we close the window the program
continues to run. This is because the default handler for click on the
“close” button of window just makes it invisible, and the main loop
`mainGUI` continues to run. We will see how to handle this situation
properly soon.

## Attributes

*Attributes* allow to customize widgets, such as our window. There are two
methods to specify widget attributes:

1. Set them with the `set` action.

2. Use Glade to design your UI.

We will touch the second option later, but for now let's become familiar
with the `set` action. Typical usage of `set` is the following:

```haskell
set widget [ attributeA := valueA, attributeB := valueB, … ]
```

The GTK modules are structured by widget type, and every module typically
has the “attributes” section. This is how to find out which attributes we
can tweak. For example a `Window` has the following:

* `windowTitle`
* `windowType`
* `windowResizable`

…and many others. Let's change title of our window and make it
non-resizeable:

```haskell
main :: IO ()
main = do
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "Calculator"
             , windowResizeable    := False
             , windowDefaultWidth  := 230
             , windowDefaultHeight := 250 ]
  widgetShowAll window
  mainGUI
```

Looks like it works:

![Calculator window with adjusted attributes](/tutorials/haskell/gui-application/calc-1.png)

## Containers

Still, even non-resizable window with title is boring. What we would like to
do is to *put something inside* that window. This brings us to the GTK+
notion of *container*. Container is a widget that can contain another
widgets inside. There are two types of containers:

* those that serve purely decorative purpose and can contain only one
  widget;

* those that help organize forms and can contain several widgets.

Inner widgets are typically called *children*, while the enclosing widget is
called *parent*.

Most important actions that you will want to perform on containers are:

* `containerAdd parent child` to add `child` widget to `parent` widget

* `containerRemove parent child` to remove `child` widget from `parent`
  widget

* `containerGetChildren` to get all children of a container widget

* `containerForeach` to perform an action on all children of a container

For now we will need a non-editable text area where we will show the number
that is being entered and result of computations:

```haskell
main :: IO ()
main = do
  …
  display <- entryNew
  set display [ entryEditable := False
              , entryXalign   := 1 -- makes contents right-aligned
              , entryText     := "0" ]
  …
```

We use the `Entry` widget to display numbers, but it's not editable and
right-aligned. We don't hurry to insert it into our `window` because we need
some sort of “grid” to make the form look like a real calculator.

Indeed, there is the `Grid` widget in the `Graphics.UI.Gtk.Layout.Grid`
module. This is an example of a more complex container that has its own
interface for better control of layout. We will be using the following
functions from its API:

```haskell
-- | Creates a 'Grid'.
gridNew :: IO Grid

-- | Sets whether all rows of grid will have the same height.
gridSetRowHomogeneous :: GridClass self
  => self              -- ^ The grid
  -> Bool              -- ^ 'True' to make rows homogeneous
  -> IO ()

-- | Adds a widget to the grid. The position of child is determined by left
-- and top. The number of “cells” that child will occupy is determined by
-- width and height.
gridAttach :: (GridClass self, WidgetClass child)
  => self    -- ^ The grid
  -> child   -- ^ The widget to add
  -> Int     -- ^ The column number of to attach the left side of child to
  -> Int     -- ^ The row number to attach the top side of child to
  -> Int     -- ^ Width — the number of columns that child will span
  -> Int     -- ^ Height — the number of rows that child will span
  -> IO ()
```

`gridNew` and `gridSetRowHomogeneous` should be self-explanatory.
`gridAttach` allows to insert widgets into the grid controlling their
position and size. This is very handy for our calculator application, let's
use it:

```haskell
main :: IO ()
main = do
  …
  grid <- gridNew                  -- (1)
  gridSetRowHomogeneous grid True  -- (2)
  let attach x y w h item = gridAttach grid item x y w h -- (3)
  attach 0 0 5 1 display           -- (4)
  mkBtn "MC"  >>= attach 0 1 1 1   -- (5)
  mkBtn "MR"  >>= attach 1 1 1 1
  mkBtn "MS"  >>= attach 2 1 1 1
  mkBtn "M+"  >>= attach 3 1 1 1
  mkBtn "M–"  >>= attach 4 1 1 1
  mkBtn "←"   >>= attach 0 2 1 1
  mkBtn "CE"  >>= attach 1 2 1 1
  mkBtn "C"   >>= attach 2 2 1 1
  mkBtn "±"   >>= attach 3 2 1 1
  mkBtn "√"   >>= attach 4 2 1 1
  mkBtn "7"   >>= attach 0 3 1 1
  mkBtn "8"   >>= attach 1 3 1 1
  mkBtn "9"   >>= attach 2 3 1 1
  mkBtn "÷"   >>= attach 3 3 1 1
  mkBtn "%"   >>= attach 4 3 1 1
  mkBtn "4"   >>= attach 0 4 1 1
  mkBtn "5"   >>= attach 1 4 1 1
  mkBtn "6"   >>= attach 2 4 1 1
  mkBtn "*"   >>= attach 3 4 1 1
  mkBtn "1/x" >>= attach 4 4 1 1
  mkBtn "1"   >>= attach 0 5 1 1
  mkBtn "2"   >>= attach 1 5 1 1
  mkBtn "3"   >>= attach 2 5 1 1
  mkBtn "–"   >>= attach 3 5 1 1
  mkBtn "="   >>= attach 4 5 1 2
  mkBtn "0"   >>= attach 0 6 2 1
  mkBtn "."   >>= attach 2 6 1 1
  mkBtn "+"   >>= attach 3 6 1 1
  containerAdd window grid         -- (6)
  …
```

1. `gridNew` creates a new grid.

2. `gridSetRowHomogeneous grid True` makes every row have equal height.

3. Here we define the `attach` helper function. It attaches given widget to
   our `grid`. The argument order of this function helps to use it with
   `(>>=)`.

4. We attach `display` we created previously to the `grid`. It will occupy
   the entire top row.

5. Here we use combination of `mkBtn` helper and `attach` to quickly create
   buttons and place them on the grid. I'll show `mkBtn` is a moment.

6. Now the grid itself needs to be inserted into `window` to be visible.
   This is done with help of above-mentioned `containerAdd` function.

`mkBtn` is a helper for button creation, right now it's very simple:

```haskell
mkBtn :: String -> IO Button
mkBtn label = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  return btn
```

We create new button, set its attributes (just label in our case) and return
the button.

![Our calculator looks like a real calculator](/tutorials/haskell/gui-application/calc-2.png)

## Signals and events

The application looks like a calculator but does not behave like one yet. To
fix this, we need to learn about *signals* and *events*.

*Signal* is a name for things that may happen on the form. Almost always
signals are connected with user's actions. An example of signal is *focus* —
the moment when a widget becomes active on the form.

To execute some code on signal we use the `on` helper:

```haskell
on :: object -> Signal object callback -> callback -> IO (ConnectId object)
```

Here `object` is the widget of interest, `callback` is the action that we
want to perform. The `on` function returns `ConnectId` parametrized over the
`object` type (so we cannot mix up connection identifiers for different
types of objects). This is the identifier of signal handler and its sole
purpose to give you a way to disconnect a signal handler if you ever need
it. You can use the `disconnect` from `System.Glib.Signals` to do that.

Every signal dictates type that `callback` function will have. The following
cases are most frequent:

* Just `IO ()`, no information is given to the handler it is not expected to
  return anything. Example of such signal is `showSignal`.

* Handlers that are given arguments: `a -> IO Bool`. Example of such signal
  is `focus` whose handlers have the type `DirectionType -> IO Bool`.
  Another interesting thing here is returned value of the type `Bool`. This
  is a convention in GTK+ allowing to disable default handling of some
  signals. If we return `True`, default handling will be disabled, while
  `False` will keep it active executing our handler *and* default handler as
  well.

There is one more way to get some information from within signal's handler.
Some signals dictate that handler should live in special monad called
`EventM` instead of plain `IO`. Signals that like their handlers to be in
`EventM` are called events.

What is the `EventM` monad? Actually it's a type synonym for a simple monad
stack with `IO` at the bottom:

```haskell
type EventM t = ReaderT (Ptr t) IO
```

This is just a reader monad transformer on top of `IO`. `t` specifies type
of information we can extract and which helper function we can use inside
the `EventM` monad. These are different for every event. For example
`configureEvent` allows to extract information about window size, while
`keyPressEvent` event provides information about key that has been pressed,
which modifier key was held at that time and so forth. The type system does
not allow to try to extract information that particular event does not
provide.

I would like to quote the docs to accent importance of returned Boolean
value:

> Note that an event handler must always returns `True` if the event was
> handled or `False` if the event should be dealt with by another event
> handler. For instance, a handler for a key press should return `False` if
> the pressed key is not one of those that the widget reacts to. In this
> case the event is passed to the parent widgets. This ensures that
> pressing, say, `Alt-F` opens the file menu even if the current input focus
> is in a text entry widget. In order to facilitate writing handlers that
> may abort handling an event, this module provides the function `tryEvent`.
> This function catches pattern match exceptions and returns `False`. If the
> signal successfully runs to its end, it returns `True`.

Knowing all that, we can write a simple handler to run on button activation.
Looking at the “signals” section in `Graphics.UI.Gtk.Buttons.Button`,
`buttonActivated` looks like our frined here:

```haskell
-- | Emitted when the button has been activated (pressed and released).
buttonActivated :: ButtonClass self => Signal self (IO ())
```

Just for a test, let's re-write `mkBtn` to attach a handler that will update
the display with name of pressed button (we still don't know the whole lot
to make a working calculator):

```haskell
mkBtn :: String -> Entry -> IO Button
mkBtn label display = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  btn `on` buttonActivated $
    set display [ entryText := label ]
  return btn
```

And we need to pass `display` to `mkBtn` like this:

```haskell
  …
  mkBtn "MC"  display >>= attach 0 1 1 1
  mkBtn "MR"  display >>= attach 1 1 1 1
  …
```

Another thing that we can deal with now is proper closing of our
application. For this we need a way to call the `mainQuit` function:

```haskell
-- | Exit the main event loop.
mainQuit :: IO ()
```

As you may have guessed by now, convenient place to put the `mainQuit`
function is on closing of `window`. Event that we're looking for is called
`deleteEvent`:

```haskell
-- | The deleteEvent signal is emitted if a user requests that a toplevel
-- window is closed. The default handler for this signal destroys the window.
-- Calling 'widgetHide' and returning 'True' on reception of this signal will
-- cause the window to be hidden instead, so that it can later be shown again
-- without reconstructing it.
deleteEvent :: WidgetClass self => Signal self (EventM EAny Bool)
```

In our case we just want to close it, so:

```haskell
  …
  containerAdd window grid
  window `on` deleteEvent $ do -- handler to run on window destruction
    liftIO mainQuit
    return False
  widgetShowAll window
  mainGUI
```

Note that `deleteEvent` parametrizes `EventM` type by `EAny` type-level tag.
Its description:

```haskell
-- | A tag for events that do not carry any event-specific information.
data EAny
```

Even though it does not carry any event-specific information, a lot of
useful information can be extracted, such as current time at the moment when
event fired (`eventTime`). See full list of helpers in the “Accessor
functions for event information” section of the `Graphics.UI.Gtk.Gdk.EventM`
module.

I encourage you compile and run the application to see that it responds to
button activation and closes properly.

## Using `IORef`s for application state

Buttons can change display dynamically, but it's still not enough to make
our calculator actually useful. For this (as with most other applications),
we need some sort of state.

The creators of GTK+ binding didn't give us too many options here because
type of handler monad is fixed: it's either plain `IO` or `EventM`, which is
as we already know is just `ReaderT (Ptr t) IO`. We cannot return anything
non-standard from handlers, so the only way to communicate with outside
world is via mutable references.

There are two most obvious options:

* `IORef`s — mutable references inside `IO` monad.

* `TVar`s from `Control.Concurrent.STM`.

`TVar`s are probably overkill unless you do complex concurrent work. What is
good about using `TVar`s is that we can update them atomically. This may be
not very important for some applications, but I recommend build with
concurrency in mind from the very beginning. But `IORef`s can be changed
atomically as well with help of `atomicModifyIORef`.

Now we got to the question how to model calculator logic. Since actual logic
is not our primary concern in this tutorial, we will go the easy way.

```haskell
-- | 'Value' holds textual representation of first argument reversed and
-- 'Action' to apply to it, which see.
data Value = Value String (Maybe Action)

-- | Action to apply to first argument and textual representation of second
-- argument reversed (if relevant).
data Action
  = Addition       String
  | Subtraction    String
  | Multiplication String
  | Division       String
```

`Value` is our state, it contains textual representation of first argument
and optionally representation of action that should be performed on it. The
`String`s representing arguments are reversed because this way it's faster
to add/drop a character at the end of the string. We will reverse the
strings back when it's time to turn them into `Double`s.

We will need a couple of helper functions too. Here they are:

```haskell
-- | Change second argument inside of 'Action'.
mapAction :: (String -> String) -> Action -> Action
mapAction f (Addition       x) = Addition       (f x)
mapAction f (Subtraction    x) = Subtraction    (f x)
mapAction f (Multiplication x) = Multiplication (f x)
mapAction f (Division       x) = Division       (f x)

-- | Get second argument from 'Action'.
getSndArg :: Action -> String
getSndArg (Addition       x) = x
getSndArg (Subtraction    x) = x
getSndArg (Multiplication x) = x
getSndArg (Division       x) = x

-- | Render given 'Value'.
renderValue :: Value -> String
renderValue (Value x action) =
  g x ++ f a ++ (if null y then "" else g y)
  where
    (a, y) =
      case action of
        Nothing                   -> ("", "")
        Just (Addition       arg) -> ("+", arg)
        Just (Subtraction    arg) -> ("–", arg)
        Just (Multiplication arg) -> ("*", arg)
        Just (Division       arg) -> ("÷", arg)
    f "" = ""
    f l  = " " ++ l ++ " "
    g "" = "0"
    g xs = reverse xs
```

The first two help change and extract the second argument in `Action`, while
`renderValue` does its best to render current calculator state. Having
`renderValue`, it's easy to write a function that would update the
calculator display:

```haskell
-- | Make calculator's display show given 'Value'.
updateDisplay :: Entry -> Value -> IO ()
updateDisplay display value =
  set display [ entryText := renderValue value ]
```

Finally, instead of `mkBtn` let's have `mkButton` of the following form:

```haskell
-- | Create a button and attach handler to it that mutates calculator's
-- state with given function.
mkButton
  :: IORef Value       -- ^ 'IORef' to calculator state
  -> Entry             -- ^ Our display to update
  -> String            -- ^ Button label
  -> (Value -> Value)  -- ^ How this button affects calculator state
  -> IO Button         -- ^ Resulting button object
mkButton st display label mutateState = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  btn `on` buttonActivated $ do  -- (1)
    value <- atomicModifyIORef st $ \x -> let r = mutateState x in (r, r) -- (2)
    updateDisplay display value  -- (3)
  return btn
```

1. Just like before we register a handler that will fire on button
   activation.

2. `atomicModifyIORef` modifies given `IORef` atomically. The callback
   should return a tuple, first element is the new value to put into
   `IORef`, second value is the return value of the action. In this case we
   want the values to be equal.

3. We call `updateDisplay` to make results of last action visible to the
   user.

Now we can define a helper called `mkBtn` in `main`:

```haskell
main :: IO ()
main = do
  st <- newIORef (Value "" Nothing)  -- (1)
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "Calculator"
             , windowResizable     := False
             , windowDefaultWidth  := 230
             , windowDefaultHeight := 250 ]
  display <- entryNew
  set display [ entryEditable := False
              , entryXalign   := 1 -- makes contents right-aligned
              , entryText     := "0" ]
  grid <- gridNew
  gridSetRowHomogeneous grid True
  let attach x y w h item = gridAttach grid item x y w h
      mkBtn = mkButton st display    -- (2)
  attach 0 0 5 1 display
  mkBtn "MC"  id >>= attach 0 1 1 1  -- (3)
  mkBtn "MR"  id >>= attach 1 1 1 1
  …
```

1. We need to create `IORef` to keep the program's state there. `Value ""
   Nothing` is its initial value.

2. The helper function `mkBtn` uses previously written `mkButton` and just
   saves us the boilerplate of passing `st` and `display` again and again.

3. Some examples of `mkBtn` use. By passing `id` as state mutating function
   we make buttons have no effect, but all the machinery for actual work is
   already in place.

The only thing that remains is state-mutating functions per button. Here I
will show some of them:

```haskell
-- | Change state as if a dot is entered.
enterDot :: Value -> Value
enterDot (Value x action) =
  let f xs = if '.' `elem` xs then xs else '.' : xs
  in case action of
       Nothing -> Value (f x) Nothing
       Just  a -> Value x (Just $ mapAction f a)

-- | Change state as if specific char (digit) is entered.
enterDigit :: Char -> Value -> Value
enterDigit ch (Value x action) =
  case action of
    Nothing -> Value (ch:x) Nothing
    Just  a -> Value x (Just $ mapAction (ch:) a)

-- | Change state as if last character of current argument is removed.
backspace :: Value -> Value
backspace (Value x action) =
  case action of
    Nothing -> Value (drop 1 x) Nothing
    Just  a -> Value x (Just $ mapAction (drop 1) a)

-- | Apply given operator to current state. If some action is already fully
-- constructed, evaluate it first.
operator :: (String -> Action) -> Value -> Value
operator op value =
  let (Value x action) = equals value
  in Value x $ Just $
    case action of
      Nothing -> op ""
      Just  a -> op (getSndArg a)

-- | Change state as if current argument is removed.
clearEntry :: Value -> Value
clearEntry (Value x action) =
  case action of
    Nothing -> Value "" Nothing
    Just  a ->
      if null (getSndArg a)
        then Value "" Nothing
        else Value x (Just $ mapAction (const "") a)

-- | Change state returning it to the default value.
clearAll :: Value -> Value
clearAll = const (Value "" Nothing)

-- | Evaluate current calculator's state putting result in place of first
-- argument.
equals :: Value -> Value
equals (Value x action) =
  case action of
    Nothing -> Value x Nothing
    Just  a ->
      if null (getSndArg a)
        then Value x action
        else Value result Nothing
          where
            g  :: String -> Double
            g ""       = 0
            g ('.':xs) = g ('0':'.':xs)
            g xs       = read (reverse xs)
            x' = g x
            y' = g (getSndArg a)
            result = reverse . show $
              case a of
                Addition       _ -> x' + y'
                Subtraction    _ -> x' - y'
                Multiplication _ -> x' * y'
                Division       _ -> x' / y'
```

The calculator is not perfect, but good enough for our purposes. Compile,
run it and see how it works. Implementation of the rest of functionality is
left as an exercise for the reader.

## Using Glade to design forms

You probably have noticed that manual creation of forms introduces quite a
bit of boilerplate in our code. This can become even worse as your forms get
more complex. Because of this, I think it's time to try our hand on a modern
UI designer for GTK+ called [Glade](https://glade.gnome.org/).

Glade is straightforward to install and use. Open the application, you will
see panels with various widgets: top-level objects (such as window),
containers, and controls (such as buttons). Select window button on the
topmost palette and the window will appear on the working area.

To replicate our window with Glade, enter “Calculator” in the title field.

Don't forget to fill out the “ID” attribute, this is how you will access
widgets on your form in the Haskell code.

Create grid with id `grid`. When asked about number of rows and columns,
choose 7 × 5. Select the “Homogeneous” check box under the “Rows” title. Now
insert buttons to match our existing design.

TODO Write it out normally

![Building the calculator form in Glade](/tutorials/haskell/gui-application/calc-3-mini.png)

[Bigger image](/tutorials/haskell/gui-application/calc-3.png).

## See also

* [`gtk3` on Hackage](https://hackage.haskell.org/package/gtk3)

* [Haskell port of GTK tutorial](http://code.haskell.org/gtk2hs/docs/tutorial/Tutorial_Port/)

* [Writing multi-threaded GUI applications](http://dmwit.com/gtk2hs/)

* [Glade tutorials](https://wiki.gnome.org/action/show/Apps/Glade/Tutorials?action=show&redirect=Glade%2FTutorials)
