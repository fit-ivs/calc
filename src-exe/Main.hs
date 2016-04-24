module Main where

import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isAscii, isSymbol, isPunctuation)
import Data.Foldable (forM_)
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import Data.Text (unpack)
import Graphics.UI.Gtk
import Lib (parse, evaluate)
import Text.Megaparsec (errorPos, sourceColumn)

main :: IO ()
main = do
    _ <- initGUI
    -- Create the builder, and load the UI file
    builder <- builderNew
    builderAddFromFile builder "calc.glade"
    rcParse "calc.rc"

    mainLabel <- builderGetObject builder castToLabel "labelBig"
    sideLabel <- builderGetObject builder castToLabel "labelSmall"

    -- Color EventBoxes (= workaround for button colors)
    delBox <- builderGetObject builder castToEventBox "boxDel"
    widgetModifyBg delBox StateNormal (Color 59624 8824 8995)
    widgetModifyBg delBox StatePrelight (Color 59624 8824 8995)
    widgetModifyBg delBox StateActive (Color 59624 8824 8995)
    clrBox <- builderGetObject builder castToEventBox "boxClr"
    widgetModifyBg clrBox StateNormal (Color 59624 8824 8995)
    widgetModifyBg clrBox StatePrelight (Color 59624 8824 8995)
    widgetModifyBg clrBox StateActive (Color 59624 8824 8995)
    subBox <- builderGetObject builder castToEventBox "boxEq"
    widgetModifyBg subBox StateNormal (Color 0 65535 0)
    widgetModifyBg subBox StatePrelight (Color 0 65535 0)
    widgetModifyBg subBox StateActive (Color 0 65535 0)

    -- Global state
    hasAnswer <- newIORef False
    lastAnswer <- newIORef "0.0"

    window <- builderGetObject builder castToWindow "window1"
    _ <- on window objectDestroy mainQuit

    _ <- on window keyPressEvent $ tryEvent $ do
        Just char <- eventKeyVal >>= (liftIO . keyvalToChar)
        guard $ isAscii char
        hasAnswer_ <- liftIO $ readIORef hasAnswer
        liftIO $ if isSymbol char || isPunctuation char
                 then append [char] mainLabel hasAnswer_
                 else overwrite [char] mainLabel hasAnswer_
        liftIO $ writeIORef hasAnswer False

    _ <- on window keyPressEvent $ tryEvent $ do
        "Return" <- fmap unpack eventKeyName
        liftIO $ submit mainLabel sideLabel hasAnswer lastAnswer

    _ <- on window keyPressEvent $ tryEvent $ do
        "BackSpace" <- fmap unpack eventKeyName
        liftIO $ delete mainLabel

    _ <- on window keyPressEvent $ tryEvent $ do
        "Delete" <- fmap unpack eventKeyName
        liftIO $ clear mainLabel sideLabel hasAnswer

    _ <- on window keyPressEvent $ tryEvent $ do
        "Escape" <- fmap unpack eventKeyName
        liftIO $ clear mainLabel sideLabel hasAnswer

    -- Bind all number/operator buttons
    forM_ textButtonBindings $ \(btnId,action) -> do
        button <- builderGetObject builder castToButton btnId
        on button buttonActivated $ do
            readIORef hasAnswer >>= action mainLabel
            writeIORef hasAnswer False

    -- Bind action buttons
    subButton <- builderGetObject builder castToButton "buttonEq"
    _ <- on subButton buttonActivated (submit mainLabel sideLabel hasAnswer lastAnswer)
    delButton <- builderGetObject builder castToButton "buttonDel"
    _ <- on delButton buttonActivated (delete mainLabel)
    clrButton <- builderGetObject builder castToButton "buttonClr"
    _ <- on clrButton buttonActivated (clear mainLabel sideLabel hasAnswer)
    ansButton <- builderGetObject builder castToButton "buttonAns"
    _ <- on ansButton buttonActivated (answer mainLabel hasAnswer lastAnswer)

    -- Display the window
    widgetShowAll window
    mainGUI

textButtonBindings :: [(String, Label -> Bool -> IO ())]
textButtonBindings =
    [ ("buttonFact",  append "!")
    , ("buttonMod",   append "%")
    , ("buttonExp",   append "^")
    , ("buttonSciNot",append "*10^")
    , ("buttonExp2",  append "^2")
    , ("buttonInv",   append "^-1")
    , ("buttonSqrt",  append "^-0.5")
    , ("buttonAdd",   append "+")
    , ("buttonSub",   append "-")
    , ("buttonMul",   append "*")
    , ("buttonDiv",   append "/")
    , ("buttonLog",   overwrite "(log ")
    , ("buttonLn",    overwrite "(ln ")
    , ("buttonLeft",  overwrite "(")
    , ("buttonRight", overwrite ")")
    , ("buttonPoint", overwrite ".")
    , ("button0", overwrite "0")
    , ("button1", overwrite "1")
    , ("button2", overwrite "2")
    , ("button3", overwrite "3")
    , ("button4", overwrite "4")
    , ("button5", overwrite "5")
    , ("button6", overwrite "6")
    , ("button7", overwrite "7")
    , ("button8", overwrite "8")
    , ("button9", overwrite "9")]

answer :: Label -> IORef Bool -> IORef String -> IO ()
answer mainLabel hasAnswer lastAnswer = do
    hasAnswer_ <- readIORef hasAnswer
    if hasAnswer_ then writeIORef hasAnswer False else do
        answer_ <- readIORef lastAnswer
        append answer_ mainLabel hasAnswer_

submit :: Label -> Label -> IORef Bool -> IORef String -> IO ()
submit mainLabel sideLabel hasAnswer lastAnswer = do
    expr <- labelGetText mainLabel
    case parse expr of
        Left e ->
            let start = max 0 $ sourceColumn (errorPos e) - 2
                end = length expr
            in labelSetAttributes
               mainLabel
               [ AttrUnderlineColor start end (Color 65535 0 0)
               , AttrUnderline start end UnderlineError
               , AttrWeight 0 end WeightBold
               , AttrSize 0 end 12]
        Right val -> do
            let ans = case evaluate val of
                    Just n -> show n
                    Nothing -> "NaN"
            writeIORef hasAnswer True
            writeIORef lastAnswer ans
            labelSetText sideLabel expr
            setMainLabelText mainLabel ans

delete :: Label -> IO ()
delete label = do
    text <- labelGetText label :: IO String
    labelSetText label $ take (length text - 1) text

clear :: Label -> Label -> IORef Bool -> IO ()
clear label1 label2 hasAnswer = do
    writeIORef hasAnswer False
    labelSetText label1 ""
    labelSetText label2 ""

append :: String -> Label -> Bool -> IO ()
append string label _ = do
    current <- labelGetText label
    setMainLabelText label (current ++ string)

overwrite :: String -> Label -> Bool -> IO ()
overwrite string label hasAnswer =
    if hasAnswer
        then setMainLabelText label string
        else append string label False

setMainLabelText :: Label -> String -> IO ()
setMainLabelText label str = do
    let len = length str
    labelSetText label str
    labelSetAttributes label [AttrWeight 0 len WeightBold, AttrSize 0 len 12]
