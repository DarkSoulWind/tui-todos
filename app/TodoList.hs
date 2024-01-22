{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TodoList where

import Control.Monad.State
import Lens.Micro ((&), (.~), (^.))
import Lens.Micro.TH (makeLenses)

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Table as TB
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

data Todo = Todo
    { title :: String
    , description :: String
    , completed :: Bool
    }
    deriving (Show)

newtype TodoFile = TodoFile
    { todos :: [Todo]
    }
    deriving (Show)

data ExitStatus = None | Add | Delete Int Todo | Exit deriving (Show)

data AppState = AppState
    { _todosList :: L.List () Todo
    , _exitStatus :: ExitStatus
    }
    deriving (Show)

makeLenses ''AppState

instance ToJSON TodoFile where
    toJSON (TodoFile todos') =
        object
            [ "todos" Data.Aeson..= todos'
            ]

instance FromJSON TodoFile where
    parseJSON = withObject "TodoFile" $ \v ->
        TodoFile
            <$> v .: "todos"

instance ToJSON Todo where
    toJSON (Todo title' description' completed') =
        object
            [ "title" Data.Aeson..= title'
            , "description" Data.Aeson..= description'
            , "completed" Data.Aeson..= completed'
            ]

instance FromJSON Todo where
    parseJSON = withObject "Todo" $ \v ->
        Todo
            <$> v .: "title"
            <*> v .: "description"
            <*> v .: "completed"

encodeTodoFile :: TodoFile -> BSL.ByteString
encodeTodoFile = encode

formatTodo :: Todo -> String
formatTodo = title

renderListElement :: Bool -> Todo -> Widget ()
renderListElement sel e =
    let selStr =
            if sel
                then ">"
                else " "
     in str $ selStr ++ " " ++ formatTodo e

drawUI :: AppState -> [Widget ()]
drawUI x = [ui]
  where
    theList = x ^. todosList
    selectedIndex = theList ^. L.listSelectedL
    currentIndexStr = case selectedIndex of
        Nothing -> str "-"
        Just i -> str $ show $ i + 1
    total = str $ show $ Vec.length $ theList ^. L.listElementsL
    label = str " Item " <+> currentIndexStr <+> str " of " <+> total <+> str " "
    box = B.borderWithLabel label $ L.renderList renderListElement True theList
    listUI = hLimit 25 $ vLimit 15 box
    helpTableRows =
        map
            (\[l, r] -> [padRight (Pad 2) l, padLeft (Pad 2) r])
            [ [str "q", str "quit"]
            , [str "+", str "add todo"]
            , [str "-", str "remove todo"]
            ]
    helpTable =
        TB.surroundingBorder False $
            TB.rowBorders False $
                TB.columnBorders False $
                    TB.alignRight 1 $
                        TB.alignLeft 0 $
                            TB.table helpTableRows
    helpBox =
        B.borderWithLabel (str " help ") $
            padTopBottom 1 $
                padLeftRight 2 $
                    TB.renderTable helpTable
    ui = C.center $ vBox [C.hCenter listUI, C.hCenter helpBox]

handleEvent :: BrickEvent () e -> EventM () AppState ()
handleEvent (VtyEvent e) =
    case e of
        V.EvKey (V.KChar 'q') [] -> halt
        V.EvKey (V.KChar '-') [] -> do
            st <- get
            let sel = st ^. (todosList . L.listSelectedL)
            case sel of
                Nothing -> return ()
                Just i -> do
                    let todoToBeDeleted = (st ^. todosList . L.listElementsL) Vec.! i
                    modify $ setExitStatus $ Delete i todoToBeDeleted
                    halt
        V.EvKey (V.KChar '+') [] -> do
            modify $ setExitStatus Add
            halt
        V.EvKey (V.KChar 'j') [] -> moveDown
        V.EvKey V.KDown [] -> moveDown
        V.EvKey (V.KChar 'k') [] -> moveUp
        V.EvKey V.KUp [] -> moveUp
        _ -> return ()
handleEvent _ = return ()

moveDown :: EventM () AppState ()
moveDown = do
    st <- get
    put $ st & todosList .~ L.listMoveDown (st ^. todosList)

moveUp :: EventM () AppState ()
moveUp = do
    st <- get
    put $ st & todosList .~ L.listMoveUp (st ^. todosList)

setExitStatus :: ExitStatus -> AppState -> AppState
setExitStatus x st = st & exitStatus .~ x

removeTodo :: Int -> AppState -> AppState
removeTodo i st = st & todosList .~ L.listRemove i (st ^. todosList)

customAttr :: AttrName
customAttr = L.listSelectedAttr <> attrName "custom"

theMap :: AttrMap
theMap =
    attrMap
        V.defAttr
        [ (L.listAttr, V.white `on` V.blue)
        , (L.listSelectedAttr, V.blue `on` V.white)
        , (customAttr, fg V.cyan)
        ]

theApp :: App AppState e ()
theApp =
    App
        { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return ()
        , appAttrMap = const theMap
        }

getTodoFile :: String -> IO (Maybe TodoFile)
getTodoFile path = do
    contents <- BSL.readFile path
    let todosData = decode contents :: Maybe TodoFile
    return todosData

initialState :: TodoFile -> AppState
initialState todos' =
    AppState
        { _todosList = L.list () (Vec.fromList $ todos todos') 1
        , _exitStatus = None
        }

runTodoList :: AppState -> IO AppState
runTodoList = do defaultMain theApp
