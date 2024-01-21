{-# LANGUAGE TemplateHaskell #-}

module AddTodoForm where

import Brick
import Brick.Focus (focusGetCurrent, focusRingCursor)
import Brick.Forms (Form (formFocus, formState), editTextField, focusedFormInputAttr, handleFormEvent, invalidFormInputAttr, newForm, renderForm, setFieldValid, (@@=))
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (hCenter, vCenter)
import qualified Brick.Widgets.Edit as E
import Data.Text (strip)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.TH

data FormState = FormState
    { _title :: T.Text
    , _description :: T.Text
    }
    deriving (Show)

makeLenses ''FormState

data Name = TitleField | DescriptionField deriving (Show, Eq, Ord)

mkForm :: FormState -> Form FormState e Name
mkForm =
    let label s w =
            padBottom (Pad 1) $
                vLimit 1 (hLimit 15 $ str s <+> fill ' ') <+> w
     in newForm
            [ label "Title" @@= editTextField title TitleField (Just 1)
            , label "Description" @@= editTextField description DescriptionField (Just 3)
            ]

draw :: Form FormState e Name -> [Widget Name]
draw f =
    let
        form = borderWithLabel (str " Add Todo ") $ padTop (Pad 1) $ hLimit 50 $ vLimit 20 $ renderForm f
     in
        [vCenter $ hCenter form]

theMap :: AttrMap
theMap =
    attrMap
        V.defAttr
        [ (E.editAttr, V.white `on` V.black)
        , (E.editFocusedAttr, V.black `on` V.yellow)
        , (invalidFormInputAttr, V.white `on` V.red)
        , (focusedFormInputAttr, V.black `on` V.yellow)
        ]

handleEvent :: BrickEvent Name e -> EventM Name (Form FormState e Name) ()
handleEvent ev = do
    f <- gets formFocus
    case ev of
        VtyEvent (V.EvResize{}) -> return ()
        VtyEvent (V.EvKey V.KEsc []) -> halt
        -- Enter quits only when we aren't in the multi-line editor.
        VtyEvent (V.EvKey V.KEnter [])
            | focusGetCurrent f /= Just DescriptionField -> halt
        _ -> do
            handleFormEvent ev

            st <- gets formState
            let isTitleValid = not . null . T.unpack . strip $ st ^. title
            modify $ setFieldValid isTitleValid TitleField

app :: App (Form FormState e Name) e Name
app = do
    App
        { appAttrMap = const theMap
        , appStartEvent = return ()
        , appHandleEvent = handleEvent
        , appChooseCursor = focusRingCursor formFocus
        , appDraw = draw
        }

initialFormState :: FormState
initialFormState = FormState{_title = T.pack "", _description = T.pack ""}

runTodoForm :: FormState -> IO (Form FormState e Name)
runTodoForm st = do
    defaultMain app $ mkForm st
