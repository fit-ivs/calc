{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Maybe
import Development.NSIS
import System.Process
import System.Directory (findExecutable)
import System.Exit

calc :: Action ()
calc = do
    name "IVS-Calc"
    outFile "ivs-calc.exe"
    installDir "$PROGRAMFILES/IVS-Calc"
    installDirRegKey HKLM "Software/IVS-Calc" "Install_Dir"
    requestExecutionLevel Admin

    page Components
    page Directory
    page InstFiles

    unpage Confirm
    unpage InstFiles

    _ <- section "IVS-Calc (required)" [Required] $ do
        setOutPath "$INSTDIR"

        file [] "ivs-calc.exe"

        file [] "libatk-1.0-0.dll"
        file [] "libbz2-1.dll"
        file [] "libcairo-2.dll"
        file [] "libexpat-1.dll"
        file [] "libffi-6.dll"
        file [] "libfontconfig-1.dll"
        file [] "libfreetype-6.dll"
        file [] "libgcc_s_seh-1.dll"
        file [] "libgdk-win32-2.0-0.dll"
        file [] "libgdk_pixbuf-2.0-0.dll"
        file [] "libgio-2.0-0.dll"
        file [] "libglib-2.0-0.dll"
        file [] "libgmodule-2.0-0.dll"
        file [] "libgobject-2.0-0.dll"
        file [] "libgraphite2.dll"
        file [] "libgthread-2.0-0.dll"
        file [] "libgtk-win32-2.0-0.dll"
        file [] "libharfbuzz-0.dll"
        file [] "libiconv-2.dll"
        file [] "libintl-8.dll"
        file [] "libpango-1.0-0.dll"
        file [] "libpangocairo-1.0-0.dll"
        file [] "libpangoft2-1.0-0.dll"
        file [] "libpangowin32-1.0-0.dll"
        file [] "libpcre-1.dll"
        file [] "libpixman-1-0.dll"
        file [] "libpng16-16.dll"
        file [] "libstdc++-6.dll"
        file [] "libwinpthread-1.dll"
        file [] "zlib1.dll"

        writeRegStr HKLM "SOFTWARE/IVS-Calc" "Install_Dir" "$INSTDIR"

        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/IVS-Calc" "DisplayName" "IVS-Calc"
        writeRegStr HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/IVS-Calc" "UninstallString" "\"$INSTDIR/uninstall.exe\""
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/IVS-Calc" "NoModify" 1
        writeRegDWORD HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/IVS-Calc" "NoRepair" 1
        writeUninstaller "uninstall.exe"

    -- Optional section (can be disabled by the user)
    _ <- section "Start Menu Shortcuts" [] $ do
        createDirectory "$SMPROGRAMS/IVS-Calc"
        createShortcut "$SMPROGRAMS/IVS-Calc/Uninstall.lnk"
            [ Target "$INSTDIR/uninstall.exe"
            , IconFile "$INSTDIR/uninstall.exe"
            , IconIndex 0]
        createShortcut "$SMPROGRAMS/IVS-Calc/IVS-Calc.lnk"
            [ Target "$INSTDIR/ivs-calc.exe"
            , IconFile "$INSTDIR/ivs-calc.exe"
            , IconIndex 0]

    ----------------------------------

    -- Uninstaller

    uninstall $ do
        deleteRegKey HKLM "Software/Microsoft/Windows/CurrentVersion/Uninstall/IVS-Calc"
        deleteRegKey HKLM "SOFTWARE/NSIS_IVS-Calc"

        delete [] "$INSTDIR/*.*"
        delete [] "$SMPROGRAMS/IVS-Calc/*.*"

        rmdir [] "$SMPROGRAMS/IVS-Calc"
        rmdir [] "$INSTDIR"

main :: IO ()
main = do
    b <- findExecutable "makensis"
    writeFile "ivs-calc.nsi" $ nsis calc
    r <- system "makensis -V3 ivs-calc.nsi"
    when (r /= ExitSuccess) $ error "NSIS FAILED"
    when (isNothing b) $
        putStrLn "Warning: No nsis on the PATH, files were not built"
