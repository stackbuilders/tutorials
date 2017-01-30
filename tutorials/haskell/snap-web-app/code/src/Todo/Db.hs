{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Todo.Db
    ( startConnection
    , checkMigrationOrMigrate
    , saveTodo
    , allTodos
    , getTodo
    , deleteTodo
    , updateTodoCompleted
    , Todo(..)
    )where

import           Control.Monad           (unless)
import           Data.Either             (rights)
import           Data.Maybe              (fromMaybe, listToMaybe)
import           Data.String.Interpolate
import           Data.Text               (Text)
import           Database.HDBC
import           Database.HDBC.Sqlite3


-- | Todo data type definition
data Todo = Todo
  { todoID      :: Maybe Int
  , description :: Text
  , completed   :: Bool
  } deriving (Eq, Show, Read)


-- | Starts Connection
startConnection :: FilePath
                -> IO Connection
startConnection = connectSqlite3


-- |  Check if a given table exists
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


-- | Created the Todo table
createTodoTable :: Connection
                -> IO ()
createTodoTable conn = do
  let query = [i| CREATE TABLE todos (
                  id INTEGER PRIMARY KEY,
                  description TEXT,
                  completed BOOLEAN)|]

  quickQuery' conn query []
  commit conn


-- | Check required table are present.
-- If not, then performs the required operations to achieve it
checkMigrationOrMigrate :: Connection
                        -> IO ()
checkMigrationOrMigrate conn = do
  -- Note: for a real app, you probably want to create a 'version'
  -- table too and use it to keep track of schema version and
  -- implement your schema upgrade procedure here.
  schemaCreated <- tableExists conn "todos"
  unless schemaCreated $ createTodoTable conn


-- | Builds a Todo data value from a Sql Row
rowToTodo :: [SqlValue]
          -> Either String Todo
rowToTodo(todoId:todoDescription:todoCompleted:_) =
  Right Todo  { todoID =  fromSql todoId
              , description = fromSql todoDescription
              , completed  = fromSql todoCompleted
              }
rowToTodo _ = Left "Unable to create Todo data type from Row"


-- | Creates a new Todo row in the database
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


-- | Retrieves all Todos rows from the database
allTodos  :: Connection
          -> IO [Todo]
allTodos conn = do
  let query = [i| SELECT id, description, completed
                  FROM todos
                  ORDER BY completed ASC|]
  rights . fmap rowToTodo <$> quickQuery' conn query []


-- | Retrieves a specific Todo data value from the database
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


-- | Removes a specific Todo data value/row from the database
deleteTodo  :: Connection
            -> Int
            -> IO ()
deleteTodo conn todoId = do
  let query = [i| DELETE FROM todos
                  WHERE id = ?|]

  quickQuery' conn query [toSql todoId]
  commit conn


-- | Returns a Todo data value with the provided completed value
updateTodoCompleted :: Todo
                    -> Bool -- ^ The completed value
                    -> Todo
updateTodoCompleted (Todo tId tDescription _) =
  Todo tId tDescription
