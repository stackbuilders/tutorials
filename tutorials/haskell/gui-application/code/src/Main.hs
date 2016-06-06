module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

data Value = Value String (Maybe Action)

data Action
  = Addition       String
  | Subtraction    String
  | Multiplication String
  | Division       String

mapAction :: (String -> String) -> Action -> Action
mapAction f (Addition       x) = Addition       (f x)
mapAction f (Subtraction    x) = Subtraction    (f x)
mapAction f (Multiplication x) = Multiplication (f x)
mapAction f (Division       x) = Division       (f x)

getSndArg :: Action -> String
getSndArg (Addition       x) = x
getSndArg (Subtraction    x) = x
getSndArg (Multiplication x) = x
getSndArg (Division       x) = x

main :: IO ()
main = do
  st <- newIORef (Nothing :: Maybe Value)
  void initGUI
  window <- windowNew
  set window [ windowTitle         := "Calculator"
             , windowResizable     := False
             , windowDefaultWidth  := 250
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
  window `on` deleteEvent $ liftIO mainQuit >> return False
  widgetShowAll window
  mainGUI

enterDot :: (Maybe Value -> Maybe Value)
enterDot value =
  case value of
    Nothing -> Just (Value "0." Nothing)
    Just (Value x action) ->
      case action of
        Nothing -> Just (Value ('.':x) Nothing)
        Just  a -> Just (Value x (Just $ mapAction ('.':) a))

enterDigit :: Char -> (Maybe Value -> Maybe Value)
enterDigit ch value =
  case value of
    Nothing -> Just (Value [ch] Nothing)
    Just (Value x action) -> Just $
      case action of
        Nothing -> Value (ch:x) Nothing
        Just  a -> Value x (Just $ mapAction (ch:) a)

backspace :: Maybe Value -> Maybe Value
backspace value =
  case value of
    Nothing -> Nothing
    Just (Value x action) -> Just $
      case action of
        Nothing -> Value (drop 1 x) Nothing
        Just  a -> Value x (Just $ mapAction (drop 1) a)

clearEntry :: Maybe Value -> Maybe Value
clearEntry value =
  case value of
    Nothing -> Nothing
    Just (Value x action) ->
      case action of
        Nothing -> Nothing
        Just  a ->
          if null (getSndArg a)
            then Nothing
            else Just $ Value x (Just $ mapAction (const "") a)

clearAll :: Maybe Value -> Maybe Value
clearAll = const Nothing

equals :: Maybe Value -> Maybe Value
equals value =
  case value of
    Nothing -> Nothing
    Just (Value x action) ->
      case action of
        Nothing -> Just (Value x Nothing)
        Just  a ->
          if null (getSndArg a)
            then Just (Value x action)
            else Just (Value result Nothing)
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

operator :: (String -> Action) -> Maybe Value -> Maybe Value
operator op value = Just $
  case equals value of
    Nothing -> Value "" (Just $ op "")
    Just (Value x action) -> Value x $ Just $
      case action of
        Nothing -> op ""
        Just  a -> op (getSndArg a)

mkButton
  :: IORef (Maybe Value)          -- ^ 'IORef' to calculator state
  -> Entry                        -- ^ Our display to update
  -> String                       -- ^ Button label
  -> (Maybe Value -> Maybe Value) -- ^ How this button affects calculator state
  -> IO Button                    -- ^ Resulting button object
mkButton st display label mutateState = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  btn `on` buttonActivated $ do
    value <- atomicModifyIORef st $ \x -> let r = mutateState x in (r, r)
    updateDisplay display value
  return btn

updateDisplay :: Entry -> Maybe Value -> IO ()
updateDisplay display value =
  set display [ entryText := renderValue value ]

renderValue :: Maybe Value -> String
renderValue value =
  case value of
    Nothing -> "0"
    Just (Value x action) ->
      if null x
        then "0"
        else reverse x ++ f a ++ reverse y
      where
        (a, y) =
          case action of
            Nothing                   -> ("", "")
            Just (Addition       arg) -> ("+ ", arg)
            Just (Subtraction    arg) -> ("– ", arg)
            Just (Multiplication arg) -> ("* ", arg)
            Just (Division       arg) -> ("÷ ", arg)
        f "" = ""
        f l  = " " ++ l ++ " "

-- TODO comments
-- TODO go though
