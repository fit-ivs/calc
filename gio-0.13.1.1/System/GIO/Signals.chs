{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- -*-haskell-*-
-- -------------------- automatically generated file - do not edit ------------
--  Callback installers for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--
--  Created: 1 July 2000
--
--  Copyright (C) 2000-2005 Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- #hide

-- These functions are used to connect signals to widgets. They are auto-
-- matically created through HookGenerator.hs which takes a list of possible
-- function signatures that are included in the GTK sources (gtkmarshal.list).
--
-- The object system in the second version of GTK is based on GObject from
-- GLIB. This base class is rather primitive in that it only implements
-- ref and unref methods (and others that are not interesting to us). If
-- the marshall list mentions OBJECT it refers to an instance of this
-- GObject which is automatically wrapped with a ref and unref call.
-- Structures which are not derived from GObject have to be passed as
-- BOXED which gives the signal connect function a possibility to do the
-- conversion into a proper ForeignPtr type. In special cases the signal
-- connect function use a PTR type which will then be mangled in the
-- user function directly. The latter is needed if a signal delivers a
-- pointer to a string and its length in a separate integer.
--
module System.GIO.Signals (
  module System.Glib.Signals,

  connect_NONE__NONE,
  connect_GLIBSTRING_GLIBSTRING_GLIBSTRING_ENUM__NONE,
  connect_OBJECT_OBJECT_ENUM__NONE,
  connect_ENUM__NONE,
  connect_OBJECT__NONE,
  connect_MOBJECT_MOBJECT_ENUM__NONE,
  
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.UTFString   (peekUTFString,maybePeekUTFString,newUTFString)
import qualified System.Glib.UTFString as Glib
import System.Glib.GError      (failOnGError)
{#import System.Glib.Signals#}
{#import System.Glib.GObject#}


{#context lib="gtk" prefix="gtk" #}


-- Here are the generators that turn a Haskell function into
-- a C function pointer. The fist Argument is always the widget,
-- the last one is the user g_pointer. Both are ignored.


connect_NONE__NONE :: 
  GObjectClass obj => SignalName ->
  ConnectAfter -> obj ->
  (IO ()) ->
  IO (ConnectId obj)
connect_NONE__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> IO ()
        action _ =
          failOnGError $
          user

connect_GLIBSTRING_GLIBSTRING_GLIBSTRING_ENUM__NONE :: 
  (Glib.GlibString a', Glib.GlibString b', Glib.GlibString c', Enum d, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> b' -> c' -> d -> IO ()) ->
  IO (ConnectId obj)
connect_GLIBSTRING_GLIBSTRING_GLIBSTRING_ENUM__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> CString -> CString -> CString -> Int -> IO ()
        action _ str1 str2 str3 enum4 =
          failOnGError $
          peekUTFString str3 >>= \str3' ->
          peekUTFString str2 >>= \str2' ->
          peekUTFString str1 >>= \str1' ->
          user str1' str2' str3' (toEnum enum4)

connect_OBJECT_OBJECT_ENUM__NONE :: 
  (GObjectClass a', GObjectClass b', Enum c, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> b' -> c -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT_OBJECT_ENUM__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Ptr GObject -> Int -> IO ()
        action _ obj1 obj2 enum3 =
          failOnGError $
          makeNewGObject (GObject, objectUnref) (return obj2) >>= \obj2' ->
          makeNewGObject (GObject, objectUnref) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1') (unsafeCastGObject obj2') (toEnum enum3)

connect_ENUM__NONE :: 
  (Enum a, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a -> IO ()) ->
  IO (ConnectId obj)
connect_ENUM__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Int -> IO ()
        action _ enum1 =
          failOnGError $
          user (toEnum enum1)

connect_OBJECT__NONE :: 
  (GObjectClass a', GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (a' -> IO ()) ->
  IO (ConnectId obj)
connect_OBJECT__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> IO ()
        action _ obj1 =
          failOnGError $
          makeNewGObject (GObject, objectUnref) (return obj1) >>= \obj1' ->
          user (unsafeCastGObject obj1')

connect_MOBJECT_MOBJECT_ENUM__NONE :: 
  (GObjectClass a', GObjectClass b', Enum c, GObjectClass obj) => SignalName ->
  ConnectAfter -> obj ->
  (Maybe a' -> Maybe b' -> c -> IO ()) ->
  IO (ConnectId obj)
connect_MOBJECT_MOBJECT_ENUM__NONE signal after obj user =
  connectGeneric signal after obj action
  where action :: Ptr GObject -> Ptr GObject -> Ptr GObject -> Int -> IO ()
        action _ obj1 obj2 enum3 =
          failOnGError $
          maybeNull (makeNewGObject (GObject, objectUnref)) (return obj2) >>= \obj2' ->
          maybeNull (makeNewGObject (GObject, objectUnref)) (return obj1) >>= \obj1' ->
          user (liftM unsafeCastGObject obj1') (liftM unsafeCastGObject obj2') (toEnum enum3)

