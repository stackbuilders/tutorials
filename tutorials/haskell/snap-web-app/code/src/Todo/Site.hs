{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Todo.Site
    ( app
    ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.State
import           Data.ByteString                             (ByteString)
import           Data.Map.Syntax                             ((##))
import           Data.Maybe                                  (fromMaybe)
import           Heist
import           Heist.Interpreted                           (mapSplices,
                                                              runChildrenWith,
                                                              textSplice)
import           Snap.Core                                   (Method (..),
                                                              getParam, getPostParam,
                                                              method, redirect)
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession ()
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Todo.Application
import qualified Todo.Db                                     as Db
import           Todo.Utilities

------------------------------------------------------------------------------
-- | GET '/'
handleIndexTodo :: Handler App App ()
handleIndexTodo = do
  c <- gets _conn
  todos <- liftIO $ Db.allTodos c
  renderWithSplices "todos" $
    "todos" ## spliceForTodos todos


------------------------------------------------------------------------------
-- | POST: /todos/new
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


------------------------------------------------------------------------------
-- | POST: /todos/:id/complete/:completed
handleCompleteTodo :: Handler App App ()
handleCompleteTodo = do
  maybeTodoIdBs <- getParam "todoId"
  maybeCompletedBs <- getParam "completed"
  case maybeTodoIdBs of
    -- Note: for a real app, you should validate a required
    -- 'completed' parameter using digestive functors and
    -- redirect to a not found page if you can't find the
    -- row identified by the todo id.
    -- In this example we just ignore all that and redirect to '/'
    -- for the sake of simplicity.
    Nothing -> redirect "/"
    Just todoIdBs -> do
        c <- gets _conn
        let todoId = fromBSTo 0 todoIdBs :: Int
        todo <- liftIO $ Db.getTodo c todoId
        let updatedCompletedValue = fromBSTo False (fromMaybeBs maybeCompletedBs) :: Bool
        liftIO $ Db.saveTodo c  (Db.updateTodoCompleted todo
                                                        updatedCompletedValue)
        redirect "/"

  where
    fromMaybeBs :: Maybe ByteString -> ByteString
    fromMaybeBs = fromMaybe ""


------------------------------------------------------------------------------
-- | POST: /todos/:id/delete/
handleDeleteTodo :: Handler App App ()
handleDeleteTodo = do
  maybeTodoIdBs <- getParam "todoId"
  case maybeTodoIdBs of
    -- Note: for a real app, you should redirect to a not found
    -- page if you can't find the row identified by the todo id
    -- In this example we just ignore that and redirect to '/'
    -- for the sake of simplicity.
    Nothing -> redirect "/"
    Just todoIdBs -> do
        c <- gets _conn
        let todoId = fromBSTo 0 todoIdBs :: Int
        liftIO $ Db.deleteTodo c todoId
        redirect "/"


------------------------------------------------------------------------------
-- | Splice to display Todos
spliceForTodos :: [Db.Todo] -> SnapletISplice App
spliceForTodos = mapSplices $ runChildrenWith . spliceForTodo


------------------------------------------------------------------------------
-- | Splice to display a simple Todo
spliceForTodo :: Db.Todo -> Splices (SnapletISplice App)
spliceForTodo todo = do
  "id"  ## textSplice . showAsText $ fromMaybe 0 ( Db.todoID todo )
  "description"  ## textSplice $ Db.description todo
  "completed"  ## textSplice . showAsText $ Db.completed todo
  "notCompleted"  ## textSplice . showAsText $ not (Db.completed todo)
  "successClass" ## textSplice $  if Db.completed todo
                                    then "list-group-item-success"
                                    else ""

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/",                                    handleIndexTodo)
         , ("/todos/new",                           method GET handleIndexTodo
                                                <|> method POST handleNewTodo)
         , ("/todos/:todoId/complete/:completed",   method POST handleCompleteTodo)
         , ("/todos/:todoId/delete",                method POST handleDeleteTodo)
         , ("/static",                              serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
  h <- nestSnaplet "heist" heist $ heistInit "templates"
  c <- liftIO $ Db.startConnection "todos.db"
  liftIO $ Db.checkMigrationOrMigrate c

  addRoutes routes
  return $ App h c
