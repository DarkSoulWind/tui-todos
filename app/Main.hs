module Main where

import qualified AddTodoForm as TF
import qualified Brick.Forms as F
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.List as L
import Data.Text (unpack)
import qualified Data.Vector as Vec
import qualified DeleteTodoDialog as DT
import qualified InvalidFormDialog as IFD
import Lens.Micro ((^.))
import qualified TodoList as TL

newTodoFromFormResult :: TF.FormState -> TL.Todo
newTodoFromFormResult formResult =
    TL.Todo
        { TL.completed = False
        , TL.title = unpack $ formResult ^. TF.title
        , TL.description = unpack $ formResult ^. TF.description
        }

addTodo :: TL.AppState -> TF.FormState -> IO TL.AppState
addTodo currentState st = do
    formState <- TF.runTodoForm st
    let formResult = F.formState formState
    let newTodo = newTodoFromFormResult formResult
    let theList = currentState ^. TL.todosList
    let els = theList ^. L.listElementsL
    let pos = Vec.length els
    let updatedList = L.listInsert pos newTodo theList
    let newState =
            TL.AppState
                { TL._todosList = updatedList
                , TL._exitStatus = TL.None
                }
    if F.allFieldsValid formState
        then return newState
        else do
            IFD.showDialog
            let invalidFields = F.invalidFields formState
            putStrLn $ "INVALID FIELDS " ++ show invalidFields
            addTodo currentState formResult

runApp :: TL.AppState -> IO ()
runApp st = do
    currentState <- TL.runTodoList st
    print currentState
    let exitStatus = currentState ^. TL.exitStatus
    case exitStatus of
        TL.Add -> do
            newState <- addTodo currentState TF.initialFormState
            runApp $ TL.setExitStatus TL.None newState
        (TL.Delete pos todo) -> do
            confirm <- DT.showDialog todo
            let theDialog = confirm ^. DT.deleteDialog
            let maybeChoice = D.dialogSelection theDialog
            case maybeChoice of
                Nothing -> return ()
                (Just (_, choice)) -> do
                    case choice of
                        DT.Delete -> do
                            let newState = TL.removeTodo pos currentState
                            runApp $ TL.setExitStatus TL.None newState
                        DT.Cancel -> runApp $ TL.setExitStatus TL.None currentState
        _ -> return ()

main :: IO ()
main = do
    contents <- TL.getTodoFile "todos.json"
    case contents of
        Nothing -> error "FUCK WE CANT READ THE FILE"
        (Just todos) -> do
            runApp $ TL.initialState todos
