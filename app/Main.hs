module Main where

import qualified AddTodoForm as TF
import qualified Brick.Forms as F
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.List as L
import qualified Data.ByteString.Lazy as BSL
import Data.Char (chr)
import Data.Text (unpack)
import qualified Data.Vector as Vec
import qualified DeleteTodoDialog as DT
import qualified InvalidFormDialog as IFD
import Lens.Micro ((&), (.~), (^.))
import System.Directory (doesFileExist)
import qualified TodoList as TL

filePath :: String
filePath = "todos.json"

newTodoFromForm :: TF.FormState -> TL.Todo
newTodoFromForm formResult =
    TL.Todo
        { TL.completed = False
        , TL.title = unpack $ formResult ^. TF.title
        , TL.description = unpack $ formResult ^. TF.description
        }

writeTodosToFile :: TL.AppState -> IO ()
writeTodosToFile currentState = do
    let theList = currentState ^. TL.todosList
    let els = theList ^. L.listElementsL
    let todosList = Vec.toList els
    let todos = TL.TodoFile{TL.todos = todosList}
    let todosString = map (chr . fromIntegral) $ BSL.unpack $ TL.encodeTodoFile todos
    writeFile filePath todosString

addTodo :: TL.AppState -> TF.FormState -> IO TL.AppState
addTodo currentState formState = do
    formResult <- TF.runTodoForm formState
    let newFormState = F.formState formResult
    let newTodo = newTodoFromForm newFormState
    let theList = currentState ^. TL.todosList
    let els = theList ^. L.listElementsL
    let pos = Vec.length els
    let updatedList = L.listInsert pos newTodo theList
    let newState = currentState & TL.todosList .~ updatedList
    if F.allFieldsValid formResult
        then return newState
        else do
            IFD.showDialog
            addTodo currentState newFormState

removeTodo :: Int -> TL.Todo -> TL.AppState -> IO TL.AppState
removeTodo pos todo currentState = do
    confirm <- DT.showDialog todo
    let theDialog = confirm ^. DT.deleteDialog
    let maybeChoice = D.dialogSelection theDialog
    case maybeChoice of
        Nothing -> return currentState
        (Just (_, choice)) -> do
            case choice of
                DT.Delete -> do
                    let newState = TL.removeTodo pos currentState
                    return newState
                DT.Cancel -> return currentState

runApp :: TL.AppState -> IO ()
runApp st = do
    currentState <- TL.runTodoList st
    let exitStatus = currentState ^. TL.exitStatus
    case exitStatus of
        TL.Add -> do
            newState <- addTodo currentState TF.initialFormState
            writeTodosToFile newState
            runApp $ TL.setExitStatus TL.None newState
        (TL.Delete pos todo) -> do
            newState <- removeTodo pos todo currentState
            writeTodosToFile newState
            runApp $ TL.setExitStatus TL.None newState
        _ -> return ()

main :: IO ()
main = do
    fileExists <- doesFileExist filePath
    if not fileExists
        then do
            writeFile filePath "{ \"todos\": [] }"
            main
        else do
            contents <- TL.getTodoFile filePath
            case contents of
                Nothing -> error "FUCK WE CANT READ THE FILE"
                (Just todos) -> do
                    runApp $ TL.initialState todos
