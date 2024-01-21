{-# LANGUAGE OverloadedStrings #-}

module InvalidFormDialog where

import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (
    BrickEvent (..),
    Widget,
 )
import qualified Brick.Types as T
import Brick.Util (bg, on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (
    padAll,
    str,
 )
import qualified Brick.Widgets.Dialog as D
import Control.Monad (void)

data Choice = Ok
    deriving (Show)

data Name
    = OkButton
    deriving (Show, Eq, Ord)

drawUI :: D.Dialog Choice Name -> [Widget Name]
drawUI d = [ui]
  where
    body = str "The title field must not be empty."
    ui = D.renderDialog d $ C.hCenter $ padAll 1 body

appEvent :: BrickEvent Name e -> T.EventM Name (D.Dialog Choice Name) ()
appEvent (VtyEvent ev) =
    case ev of
        V.EvKey V.KEnter [] -> M.halt
        _ -> D.handleDialogEvent ev
appEvent _ = return ()

initialState :: D.Dialog Choice Name
initialState = D.dialog (Just $ str "Invalid field") (Just (OkButton, choices)) 50
  where
    choices =
        [ ("Ok", OkButton, Ok)
        ]

theMap :: A.AttrMap
theMap =
    A.attrMap
        V.defAttr
        [ (D.dialogAttr, V.white `on` V.blue)
        , (D.buttonAttr, V.black `on` V.white)
        , (D.buttonSelectedAttr, bg V.yellow)
        ]

theApp :: M.App (D.Dialog Choice Name) e Name
theApp =
    M.App
        { M.appDraw = drawUI
        , M.appChooseCursor = M.showFirstCursor
        , M.appHandleEvent = appEvent
        , M.appStartEvent = return ()
        , M.appAttrMap = const theMap
        }

showDialog :: IO ()
showDialog = do
    void $ M.defaultMain theApp initialState
