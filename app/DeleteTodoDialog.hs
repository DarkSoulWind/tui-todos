{-# LANGUAGE TemplateHaskell #-}

module DeleteTodoDialog where

import Brick
import qualified Brick.AttrMap as A
import qualified Brick.Types as T
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Dialog as D
import qualified Graphics.Vty as V
import Lens.Micro
import Lens.Micro.TH (makeLenses)
import qualified TodoList as TL

data Choice = Delete | Cancel
    deriving (Show)

data Name
    = DeleteButton
    | CancelButton
    deriving (Show, Eq, Ord)

data DeleteDialogState = DeleteDialogState
    { _deleteDialog :: D.Dialog Choice Name
    , _todoToBeDeleted :: TL.Todo
    }

makeLenses ''DeleteDialogState

drawUI :: DeleteDialogState -> [Widget Name]
drawUI d = [ui]
  where
    body =
        vBox
            [ str "Are you sure you want to delete this todo?"
            , str "Title: " <+> str (TL.title $ d ^. todoToBeDeleted)
            , str "Description: " <+> str (TL.description $ d ^. todoToBeDeleted)
            ]
    theDialog = d ^. deleteDialog
    ui = D.renderDialog theDialog $ C.hCenter $ padAll 1 body

appEvent :: BrickEvent Name e -> T.EventM Name DeleteDialogState ()
appEvent (VtyEvent ev) =
    case ev of
        V.EvKey V.KEnter [] -> halt
        V.EvKey V.KLeft [] -> do
            st <- get
            let newDialog = D.setDialogFocus CancelButton $ st ^. deleteDialog
            put $ st & deleteDialog .~ newDialog
        V.EvKey V.KRight [] -> do
            st <- get
            let newDialog = D.setDialogFocus DeleteButton $ st ^. deleteDialog
            put $ st & deleteDialog .~ newDialog
        _ -> return ()
appEvent _ = return ()

theMap :: A.AttrMap
theMap =
    A.attrMap
        V.defAttr
        [ (D.dialogAttr, V.white `on` V.blue)
        , (D.buttonAttr, V.black `on` V.white)
        , (D.buttonSelectedAttr, bg V.yellow)
        ]

theApp :: App DeleteDialogState e Name
theApp =
    App
        { appStartEvent = return ()
        , appHandleEvent = appEvent
        , appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appAttrMap = const theMap
        }

initialState :: TL.Todo -> DeleteDialogState
initialState todo =
    DeleteDialogState
        { _deleteDialog = D.dialog (Just $ str "Delete todo") (Just (CancelButton, choices)) 50
        , _todoToBeDeleted = todo
        }
  where
    choices =
        [ ("Cancel", CancelButton, Cancel)
        , ("Delete", DeleteButton, Delete)
        ]

showDialog :: TL.Todo -> IO DeleteDialogState
showDialog todo = do
    defaultMain theApp $ initialState todo
