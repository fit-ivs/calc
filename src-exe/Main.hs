module Main where

--import Lib (parse, evaluate)
import Graphics.UI.Gtk

main :: IO ()
main = do
    _ <- initGUI

    -- Create the builder, and load the UI file
    builder <- builderNew
    builderAddFromFile builder "calc.ui"

    -- Retrieve some objects from the UI
    window <- builderGetObject builder castToWindow "window1"
    button <- builderGetObject builder castToButton "button1"

    -- Basic user interation
    _ <- on button buttonActivated $ putStrLn "button pressed!"
    _ <- on window objectDestroy mainQuit

    -- Display the window
    widgetShowAll window
    mainGUI
