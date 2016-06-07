module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

----------------------------------------------------------------------------
-- The main function

-- | The program's entry point.
main :: IO ()
main = do
  st <- newIORef (Value "" Nothing)
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
      mkBtn = mkButton st display
  attach 0 0 5 1 display
  mkBtn "MC"  id >>= attach 0 1 1 1
  mkBtn "MR"  id >>= attach 1 1 1 1
  mkBtn "MS"  id >>= attach 2 1 1 1
  mkBtn "M+"  id >>= attach 3 1 1 1
  mkBtn "M–"  id >>= attach 4 1 1 1
  mkBtn "←"   backspace  >>= attach 0 2 1 1
  mkBtn "CE"  clearEntry >>= attach 1 2 1 1
  mkBtn "C"   clearAll >>= attach 2 2 1 1
  mkBtn "±"   id >>= attach 3 2 1 1
  mkBtn "√"   id >>= attach 4 2 1 1
  mkBtn "7"   (enterDigit '7') >>= attach 0 3 1 1
  mkBtn "8"   (enterDigit '8') >>= attach 1 3 1 1
  mkBtn "9"   (enterDigit '9') >>= attach 2 3 1 1
  mkBtn "÷"   (operator Division) >>= attach 3 3 1 1
  mkBtn "%"   id >>= attach 4 3 1 1
  mkBtn "4"   (enterDigit '4') >>= attach 0 4 1 1
  mkBtn "5"   (enterDigit '5') >>= attach 1 4 1 1
  mkBtn "6"   (enterDigit '6') >>= attach 2 4 1 1
  mkBtn "*"   (operator Multiplication) >>= attach 3 4 1 1
  mkBtn "1/x" id >>= attach 4 4 1 1
  mkBtn "1"   (enterDigit '1') >>= attach 0 5 1 1
  mkBtn "2"   (enterDigit '2') >>= attach 1 5 1 1
  mkBtn "3"   (enterDigit '3') >>= attach 2 5 1 1
  mkBtn "–"   (operator Subtraction) >>= attach 3 5 1 1
  mkBtn "="   equals >>= attach 4 5 1 2
  mkBtn "0"   (enterDigit '0') >>= attach 0 6 2 1
  mkBtn "."   enterDot >>= attach 2 6 1 1
  mkBtn "+"   (operator Addition) >>= attach 3 6 1 1
  containerAdd window grid
  window `on` deleteEvent $ do
    liftIO mainQuit
    return False
  widgetShowAll window
  mainGUI

----------------------------------------------------------------------------
-- Calucaltor's state

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

----------------------------------------------------------------------------
-- Calculator's operations

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

----------------------------------------------------------------------------
-- Helpers

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
  btn `on` buttonActivated $ do
    value <- atomicModifyIORef st $ \x -> let r = mutateState x in (r, r)
    updateDisplay display value
  return btn

-- | Make calculator's display show given 'Value'.
updateDisplay :: Entry -> Value -> IO ()
updateDisplay display value =
  set display [ entryText := renderValue value ]
