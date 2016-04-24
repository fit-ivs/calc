{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- -*-haskell-*-
-- -------------------- automatically generated file - do not edit ----------
--  Object hierarchy for the GIMP Toolkit (GTK) Binding for Haskell
--
--  Author : Axel Simon
--
--  Copyright (C) 2001-2005 Axel Simon
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

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- This file reflects the Gtk+ object hierarchy in terms of Haskell classes.
--
-- Note: the mk... functions were originally meant to simply be an alias
-- for the constructor. However, in order to communicate the destructor
-- of an object to objectNew, the mk... functions are now a tuple containing
-- Haskell constructor and the destructor function pointer. This hack avoids
-- changing all modules that simply pass mk... to objectNew.
--
module System.GIO.Types (

  OutputStream(OutputStream), OutputStreamClass,
  toOutputStream, 
  mkOutputStream, unOutputStream,
  castToOutputStream, gTypeOutputStream,
  FilterOutputStream(FilterOutputStream), FilterOutputStreamClass,
  toFilterOutputStream, 
  mkFilterOutputStream, unFilterOutputStream,
  castToFilterOutputStream, gTypeFilterOutputStream,
  DataOutputStream(DataOutputStream), DataOutputStreamClass,
  toDataOutputStream, 
  mkDataOutputStream, unDataOutputStream,
  castToDataOutputStream, gTypeDataOutputStream,
  BufferedOutputStream(BufferedOutputStream), BufferedOutputStreamClass,
  toBufferedOutputStream, 
  mkBufferedOutputStream, unBufferedOutputStream,
  castToBufferedOutputStream, gTypeBufferedOutputStream,
  FileOutputStream(FileOutputStream), FileOutputStreamClass,
  toFileOutputStream, 
  mkFileOutputStream, unFileOutputStream,
  castToFileOutputStream, gTypeFileOutputStream,
  MemoryOutputStream(MemoryOutputStream), MemoryOutputStreamClass,
  toMemoryOutputStream, 
  mkMemoryOutputStream, unMemoryOutputStream,
  castToMemoryOutputStream, gTypeMemoryOutputStream,
  InputStream(InputStream), InputStreamClass,
  toInputStream, 
  mkInputStream, unInputStream,
  castToInputStream, gTypeInputStream,
  MemoryInputStream(MemoryInputStream), MemoryInputStreamClass,
  toMemoryInputStream, 
  mkMemoryInputStream, unMemoryInputStream,
  castToMemoryInputStream, gTypeMemoryInputStream,
  FilterInputStream(FilterInputStream), FilterInputStreamClass,
  toFilterInputStream, 
  mkFilterInputStream, unFilterInputStream,
  castToFilterInputStream, gTypeFilterInputStream,
  BufferedInputStream(BufferedInputStream), BufferedInputStreamClass,
  toBufferedInputStream, 
  mkBufferedInputStream, unBufferedInputStream,
  castToBufferedInputStream, gTypeBufferedInputStream,
  DataInputStream(DataInputStream), DataInputStreamClass,
  toDataInputStream, 
  mkDataInputStream, unDataInputStream,
  castToDataInputStream, gTypeDataInputStream,
  FileInputStream(FileInputStream), FileInputStreamClass,
  toFileInputStream, 
  mkFileInputStream, unFileInputStream,
  castToFileInputStream, gTypeFileInputStream,
  FileMonitor(FileMonitor), FileMonitorClass,
  toFileMonitor, 
  mkFileMonitor, unFileMonitor,
  castToFileMonitor, gTypeFileMonitor,
  Vfs(Vfs), VfsClass,
  toVfs, 
  mkVfs, unVfs,
  castToVfs, gTypeVfs,
  MountOperation(MountOperation), MountOperationClass,
  toMountOperation, 
  mkMountOperation, unMountOperation,
  castToMountOperation, gTypeMountOperation,
  ThemedIcon(ThemedIcon), ThemedIconClass,
  toThemedIcon, 
  mkThemedIcon, unThemedIcon,
  castToThemedIcon, gTypeThemedIcon,
  Emblem(Emblem), EmblemClass,
  toEmblem, 
  mkEmblem, unEmblem,
  castToEmblem, gTypeEmblem,
  EmblemedIcon(EmblemedIcon), EmblemedIconClass,
  toEmblemedIcon, 
  mkEmblemedIcon, unEmblemedIcon,
  castToEmblemedIcon, gTypeEmblemedIcon,
  FileEnumerator(FileEnumerator), FileEnumeratorClass,
  toFileEnumerator, 
  mkFileEnumerator, unFileEnumerator,
  castToFileEnumerator, gTypeFileEnumerator,
  FilenameCompleter(FilenameCompleter), FilenameCompleterClass,
  toFilenameCompleter, 
  mkFilenameCompleter, unFilenameCompleter,
  castToFilenameCompleter, gTypeFilenameCompleter,
  FileIcon(FileIcon), FileIconClass,
  toFileIcon, 
  mkFileIcon, unFileIcon,
  castToFileIcon, gTypeFileIcon,
  VolumeMonitor(VolumeMonitor), VolumeMonitorClass,
  toVolumeMonitor, 
  mkVolumeMonitor, unVolumeMonitor,
  castToVolumeMonitor, gTypeVolumeMonitor,
  Cancellable(Cancellable), CancellableClass,
  toCancellable, 
  mkCancellable, unCancellable,
  castToCancellable, gTypeCancellable,
  SimpleAsyncResult(SimpleAsyncResult), SimpleAsyncResultClass,
  toSimpleAsyncResult, 
  mkSimpleAsyncResult, unSimpleAsyncResult,
  castToSimpleAsyncResult, gTypeSimpleAsyncResult,
  FileInfo(FileInfo), FileInfoClass,
  toFileInfo, 
  mkFileInfo, unFileInfo,
  castToFileInfo, gTypeFileInfo,
  AppLaunchContext(AppLaunchContext), AppLaunchContextClass,
  toAppLaunchContext, 
  mkAppLaunchContext, unAppLaunchContext,
  castToAppLaunchContext, gTypeAppLaunchContext,
  Icon(Icon), IconClass,
  toIcon, 
  mkIcon, unIcon,
  castToIcon, gTypeIcon,
  Seekable(Seekable), SeekableClass,
  toSeekable, 
  mkSeekable, unSeekable,
  castToSeekable, gTypeSeekable,
  AppInfo(AppInfo), AppInfoClass,
  toAppInfo, 
  mkAppInfo, unAppInfo,
  castToAppInfo, gTypeAppInfo,
  Volume(Volume), VolumeClass,
  toVolume, 
  mkVolume, unVolume,
  castToVolume, gTypeVolume,
  AsyncResult(AsyncResult), AsyncResultClass,
  toAsyncResult, 
  mkAsyncResult, unAsyncResult,
  castToAsyncResult, gTypeAsyncResult,
  LoadableIcon(LoadableIcon), LoadableIconClass,
  toLoadableIcon, 
  mkLoadableIcon, unLoadableIcon,
  castToLoadableIcon, gTypeLoadableIcon,
  Drive(Drive), DriveClass,
  toDrive, 
  mkDrive, unDrive,
  castToDrive, gTypeDrive,
  File(File), FileClass,
  toFile, 
  mkFile, unFile,
  castToFile, gTypeFile,
  Mount(Mount), MountClass,
  toMount, 
  mkMount, unMount,
  castToMount, gTypeMount
  ) where

import Foreign.ForeignPtr (ForeignPtr, castForeignPtr)
#if __GLASGOW_HASKELL__ >= 707
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
#else
import Foreign.ForeignPtr (unsafeForeignPtrToPtr)
#endif
import Foreign.C.Types    (CULong(..), CUInt(..), CULLong(..))
import System.Glib.GType  (GType, typeInstanceIsA)
import System.Glib.GObject

{# context lib="gio" prefix="g" #}

-- The usage of foreignPtrToPtr should be safe as the evaluation will only be
-- forced if the object is used afterwards
--
castTo :: (GObjectClass obj, GObjectClass obj') => GType -> String
                                                -> (obj -> obj')
castTo gtype objTypeName obj =
  case toGObject obj of
    gobj@(GObject objFPtr)
      | typeInstanceIsA ((unsafeForeignPtrToPtr.castForeignPtr) objFPtr) gtype
                  -> unsafeCastGObject gobj
      | otherwise -> error $ "Cannot cast object to " ++ objTypeName


-- *************************************************************** OutputStream

{#pointer *GOutputStream as OutputStream foreign newtype #} deriving (Eq,Ord)

mkOutputStream = (OutputStream, objectUnref)
unOutputStream (OutputStream o) = o

class GObjectClass o => OutputStreamClass o
toOutputStream :: OutputStreamClass o => o -> OutputStream
toOutputStream = unsafeCastGObject . toGObject

instance OutputStreamClass OutputStream
instance GObjectClass OutputStream where
  toGObject = GObject . castForeignPtr . unOutputStream
  unsafeCastGObject = OutputStream . castForeignPtr . unGObject

castToOutputStream :: GObjectClass obj => obj -> OutputStream
castToOutputStream = castTo gTypeOutputStream "OutputStream"

gTypeOutputStream :: GType
gTypeOutputStream =
  {# call fun unsafe g_output_stream_get_type #}

-- ********************************************************* FilterOutputStream

{#pointer *GFilterOutputStream as FilterOutputStream foreign newtype #} deriving (Eq,Ord)

mkFilterOutputStream = (FilterOutputStream, objectUnref)
unFilterOutputStream (FilterOutputStream o) = o

class OutputStreamClass o => FilterOutputStreamClass o
toFilterOutputStream :: FilterOutputStreamClass o => o -> FilterOutputStream
toFilterOutputStream = unsafeCastGObject . toGObject

instance FilterOutputStreamClass FilterOutputStream
instance OutputStreamClass FilterOutputStream
instance GObjectClass FilterOutputStream where
  toGObject = GObject . castForeignPtr . unFilterOutputStream
  unsafeCastGObject = FilterOutputStream . castForeignPtr . unGObject

castToFilterOutputStream :: GObjectClass obj => obj -> FilterOutputStream
castToFilterOutputStream = castTo gTypeFilterOutputStream "FilterOutputStream"

gTypeFilterOutputStream :: GType
gTypeFilterOutputStream =
  {# call fun unsafe g_filter_output_stream_get_type #}

-- *********************************************************** DataOutputStream

{#pointer *GDataOutputStream as DataOutputStream foreign newtype #} deriving (Eq,Ord)

mkDataOutputStream = (DataOutputStream, objectUnref)
unDataOutputStream (DataOutputStream o) = o

class FilterOutputStreamClass o => DataOutputStreamClass o
toDataOutputStream :: DataOutputStreamClass o => o -> DataOutputStream
toDataOutputStream = unsafeCastGObject . toGObject

instance DataOutputStreamClass DataOutputStream
instance FilterOutputStreamClass DataOutputStream
instance OutputStreamClass DataOutputStream
instance GObjectClass DataOutputStream where
  toGObject = GObject . castForeignPtr . unDataOutputStream
  unsafeCastGObject = DataOutputStream . castForeignPtr . unGObject

castToDataOutputStream :: GObjectClass obj => obj -> DataOutputStream
castToDataOutputStream = castTo gTypeDataOutputStream "DataOutputStream"

gTypeDataOutputStream :: GType
gTypeDataOutputStream =
  {# call fun unsafe g_data_output_stream_get_type #}

-- ******************************************************* BufferedOutputStream

{#pointer *GBufferedOutputStream as BufferedOutputStream foreign newtype #} deriving (Eq,Ord)

mkBufferedOutputStream = (BufferedOutputStream, objectUnref)
unBufferedOutputStream (BufferedOutputStream o) = o

class FilterOutputStreamClass o => BufferedOutputStreamClass o
toBufferedOutputStream :: BufferedOutputStreamClass o => o -> BufferedOutputStream
toBufferedOutputStream = unsafeCastGObject . toGObject

instance BufferedOutputStreamClass BufferedOutputStream
instance FilterOutputStreamClass BufferedOutputStream
instance OutputStreamClass BufferedOutputStream
instance GObjectClass BufferedOutputStream where
  toGObject = GObject . castForeignPtr . unBufferedOutputStream
  unsafeCastGObject = BufferedOutputStream . castForeignPtr . unGObject

castToBufferedOutputStream :: GObjectClass obj => obj -> BufferedOutputStream
castToBufferedOutputStream = castTo gTypeBufferedOutputStream "BufferedOutputStream"

gTypeBufferedOutputStream :: GType
gTypeBufferedOutputStream =
  {# call fun unsafe g_buffered_output_stream_get_type #}

-- *********************************************************** FileOutputStream

{#pointer *GFileOutputStream as FileOutputStream foreign newtype #} deriving (Eq,Ord)

mkFileOutputStream = (FileOutputStream, objectUnref)
unFileOutputStream (FileOutputStream o) = o

class OutputStreamClass o => FileOutputStreamClass o
toFileOutputStream :: FileOutputStreamClass o => o -> FileOutputStream
toFileOutputStream = unsafeCastGObject . toGObject

instance FileOutputStreamClass FileOutputStream
instance OutputStreamClass FileOutputStream
instance GObjectClass FileOutputStream where
  toGObject = GObject . castForeignPtr . unFileOutputStream
  unsafeCastGObject = FileOutputStream . castForeignPtr . unGObject

castToFileOutputStream :: GObjectClass obj => obj -> FileOutputStream
castToFileOutputStream = castTo gTypeFileOutputStream "FileOutputStream"

gTypeFileOutputStream :: GType
gTypeFileOutputStream =
  {# call fun unsafe g_file_output_stream_get_type #}

-- ********************************************************* MemoryOutputStream

{#pointer *GMemoryOutputStream as MemoryOutputStream foreign newtype #} deriving (Eq,Ord)

mkMemoryOutputStream = (MemoryOutputStream, objectUnref)
unMemoryOutputStream (MemoryOutputStream o) = o

class OutputStreamClass o => MemoryOutputStreamClass o
toMemoryOutputStream :: MemoryOutputStreamClass o => o -> MemoryOutputStream
toMemoryOutputStream = unsafeCastGObject . toGObject

instance MemoryOutputStreamClass MemoryOutputStream
instance OutputStreamClass MemoryOutputStream
instance GObjectClass MemoryOutputStream where
  toGObject = GObject . castForeignPtr . unMemoryOutputStream
  unsafeCastGObject = MemoryOutputStream . castForeignPtr . unGObject

castToMemoryOutputStream :: GObjectClass obj => obj -> MemoryOutputStream
castToMemoryOutputStream = castTo gTypeMemoryOutputStream "MemoryOutputStream"

gTypeMemoryOutputStream :: GType
gTypeMemoryOutputStream =
  {# call fun unsafe g_memory_output_stream_get_type #}

-- **************************************************************** InputStream

{#pointer *GInputStream as InputStream foreign newtype #} deriving (Eq,Ord)

mkInputStream = (InputStream, objectUnref)
unInputStream (InputStream o) = o

class GObjectClass o => InputStreamClass o
toInputStream :: InputStreamClass o => o -> InputStream
toInputStream = unsafeCastGObject . toGObject

instance InputStreamClass InputStream
instance GObjectClass InputStream where
  toGObject = GObject . castForeignPtr . unInputStream
  unsafeCastGObject = InputStream . castForeignPtr . unGObject

castToInputStream :: GObjectClass obj => obj -> InputStream
castToInputStream = castTo gTypeInputStream "InputStream"

gTypeInputStream :: GType
gTypeInputStream =
  {# call fun unsafe g_input_stream_get_type #}

-- ********************************************************** MemoryInputStream

{#pointer *GMemoryInputStream as MemoryInputStream foreign newtype #} deriving (Eq,Ord)

mkMemoryInputStream = (MemoryInputStream, objectUnref)
unMemoryInputStream (MemoryInputStream o) = o

class InputStreamClass o => MemoryInputStreamClass o
toMemoryInputStream :: MemoryInputStreamClass o => o -> MemoryInputStream
toMemoryInputStream = unsafeCastGObject . toGObject

instance MemoryInputStreamClass MemoryInputStream
instance InputStreamClass MemoryInputStream
instance GObjectClass MemoryInputStream where
  toGObject = GObject . castForeignPtr . unMemoryInputStream
  unsafeCastGObject = MemoryInputStream . castForeignPtr . unGObject

castToMemoryInputStream :: GObjectClass obj => obj -> MemoryInputStream
castToMemoryInputStream = castTo gTypeMemoryInputStream "MemoryInputStream"

gTypeMemoryInputStream :: GType
gTypeMemoryInputStream =
  {# call fun unsafe g_memory_input_stream_get_type #}

-- ********************************************************** FilterInputStream

{#pointer *GFilterInputStream as FilterInputStream foreign newtype #} deriving (Eq,Ord)

mkFilterInputStream = (FilterInputStream, objectUnref)
unFilterInputStream (FilterInputStream o) = o

class InputStreamClass o => FilterInputStreamClass o
toFilterInputStream :: FilterInputStreamClass o => o -> FilterInputStream
toFilterInputStream = unsafeCastGObject . toGObject

instance FilterInputStreamClass FilterInputStream
instance InputStreamClass FilterInputStream
instance GObjectClass FilterInputStream where
  toGObject = GObject . castForeignPtr . unFilterInputStream
  unsafeCastGObject = FilterInputStream . castForeignPtr . unGObject

castToFilterInputStream :: GObjectClass obj => obj -> FilterInputStream
castToFilterInputStream = castTo gTypeFilterInputStream "FilterInputStream"

gTypeFilterInputStream :: GType
gTypeFilterInputStream =
  {# call fun unsafe g_filter_input_stream_get_type #}

-- ******************************************************** BufferedInputStream

{#pointer *GBufferedInputStream as BufferedInputStream foreign newtype #} deriving (Eq,Ord)

mkBufferedInputStream = (BufferedInputStream, objectUnref)
unBufferedInputStream (BufferedInputStream o) = o

class FilterInputStreamClass o => BufferedInputStreamClass o
toBufferedInputStream :: BufferedInputStreamClass o => o -> BufferedInputStream
toBufferedInputStream = unsafeCastGObject . toGObject

instance BufferedInputStreamClass BufferedInputStream
instance FilterInputStreamClass BufferedInputStream
instance InputStreamClass BufferedInputStream
instance GObjectClass BufferedInputStream where
  toGObject = GObject . castForeignPtr . unBufferedInputStream
  unsafeCastGObject = BufferedInputStream . castForeignPtr . unGObject

castToBufferedInputStream :: GObjectClass obj => obj -> BufferedInputStream
castToBufferedInputStream = castTo gTypeBufferedInputStream "BufferedInputStream"

gTypeBufferedInputStream :: GType
gTypeBufferedInputStream =
  {# call fun unsafe g_buffered_input_stream_get_type #}

-- ************************************************************ DataInputStream

{#pointer *GDataInputStream as DataInputStream foreign newtype #} deriving (Eq,Ord)

mkDataInputStream = (DataInputStream, objectUnref)
unDataInputStream (DataInputStream o) = o

class BufferedInputStreamClass o => DataInputStreamClass o
toDataInputStream :: DataInputStreamClass o => o -> DataInputStream
toDataInputStream = unsafeCastGObject . toGObject

instance DataInputStreamClass DataInputStream
instance BufferedInputStreamClass DataInputStream
instance FilterInputStreamClass DataInputStream
instance InputStreamClass DataInputStream
instance GObjectClass DataInputStream where
  toGObject = GObject . castForeignPtr . unDataInputStream
  unsafeCastGObject = DataInputStream . castForeignPtr . unGObject

castToDataInputStream :: GObjectClass obj => obj -> DataInputStream
castToDataInputStream = castTo gTypeDataInputStream "DataInputStream"

gTypeDataInputStream :: GType
gTypeDataInputStream =
  {# call fun unsafe g_data_input_stream_get_type #}

-- ************************************************************ FileInputStream

{#pointer *GFileInputStream as FileInputStream foreign newtype #} deriving (Eq,Ord)

mkFileInputStream = (FileInputStream, objectUnref)
unFileInputStream (FileInputStream o) = o

class InputStreamClass o => FileInputStreamClass o
toFileInputStream :: FileInputStreamClass o => o -> FileInputStream
toFileInputStream = unsafeCastGObject . toGObject

instance FileInputStreamClass FileInputStream
instance InputStreamClass FileInputStream
instance GObjectClass FileInputStream where
  toGObject = GObject . castForeignPtr . unFileInputStream
  unsafeCastGObject = FileInputStream . castForeignPtr . unGObject

castToFileInputStream :: GObjectClass obj => obj -> FileInputStream
castToFileInputStream = castTo gTypeFileInputStream "FileInputStream"

gTypeFileInputStream :: GType
gTypeFileInputStream =
  {# call fun unsafe g_file_input_stream_get_type #}

-- **************************************************************** FileMonitor

{#pointer *GFileMonitor as FileMonitor foreign newtype #} deriving (Eq,Ord)

mkFileMonitor = (FileMonitor, objectUnref)
unFileMonitor (FileMonitor o) = o

class GObjectClass o => FileMonitorClass o
toFileMonitor :: FileMonitorClass o => o -> FileMonitor
toFileMonitor = unsafeCastGObject . toGObject

instance FileMonitorClass FileMonitor
instance GObjectClass FileMonitor where
  toGObject = GObject . castForeignPtr . unFileMonitor
  unsafeCastGObject = FileMonitor . castForeignPtr . unGObject

castToFileMonitor :: GObjectClass obj => obj -> FileMonitor
castToFileMonitor = castTo gTypeFileMonitor "FileMonitor"

gTypeFileMonitor :: GType
gTypeFileMonitor =
  {# call fun unsafe g_file_monitor_get_type #}

-- ************************************************************************ Vfs

{#pointer *GVfs as Vfs foreign newtype #} deriving (Eq,Ord)

mkVfs = (Vfs, objectUnref)
unVfs (Vfs o) = o

class GObjectClass o => VfsClass o
toVfs :: VfsClass o => o -> Vfs
toVfs = unsafeCastGObject . toGObject

instance VfsClass Vfs
instance GObjectClass Vfs where
  toGObject = GObject . castForeignPtr . unVfs
  unsafeCastGObject = Vfs . castForeignPtr . unGObject

castToVfs :: GObjectClass obj => obj -> Vfs
castToVfs = castTo gTypeVfs "Vfs"

gTypeVfs :: GType
gTypeVfs =
  {# call fun unsafe g_vfs_get_type #}

-- ************************************************************* MountOperation

{#pointer *GMountOperation as MountOperation foreign newtype #} deriving (Eq,Ord)

mkMountOperation = (MountOperation, objectUnref)
unMountOperation (MountOperation o) = o

class GObjectClass o => MountOperationClass o
toMountOperation :: MountOperationClass o => o -> MountOperation
toMountOperation = unsafeCastGObject . toGObject

instance MountOperationClass MountOperation
instance GObjectClass MountOperation where
  toGObject = GObject . castForeignPtr . unMountOperation
  unsafeCastGObject = MountOperation . castForeignPtr . unGObject

castToMountOperation :: GObjectClass obj => obj -> MountOperation
castToMountOperation = castTo gTypeMountOperation "MountOperation"

gTypeMountOperation :: GType
gTypeMountOperation =
  {# call fun unsafe g_mount_operation_get_type #}

-- ***************************************************************** ThemedIcon

{#pointer *GThemedIcon as ThemedIcon foreign newtype #} deriving (Eq,Ord)

mkThemedIcon = (ThemedIcon, objectUnref)
unThemedIcon (ThemedIcon o) = o

class GObjectClass o => ThemedIconClass o
toThemedIcon :: ThemedIconClass o => o -> ThemedIcon
toThemedIcon = unsafeCastGObject . toGObject

instance ThemedIconClass ThemedIcon
instance GObjectClass ThemedIcon where
  toGObject = GObject . castForeignPtr . unThemedIcon
  unsafeCastGObject = ThemedIcon . castForeignPtr . unGObject

castToThemedIcon :: GObjectClass obj => obj -> ThemedIcon
castToThemedIcon = castTo gTypeThemedIcon "ThemedIcon"

gTypeThemedIcon :: GType
gTypeThemedIcon =
  {# call fun unsafe g_themed_icon_get_type #}

-- ********************************************************************* Emblem

{#pointer *GEmblem as Emblem foreign newtype #} deriving (Eq,Ord)

mkEmblem = (Emblem, objectUnref)
unEmblem (Emblem o) = o

class GObjectClass o => EmblemClass o
toEmblem :: EmblemClass o => o -> Emblem
toEmblem = unsafeCastGObject . toGObject

instance EmblemClass Emblem
instance GObjectClass Emblem where
  toGObject = GObject . castForeignPtr . unEmblem
  unsafeCastGObject = Emblem . castForeignPtr . unGObject

castToEmblem :: GObjectClass obj => obj -> Emblem
castToEmblem = castTo gTypeEmblem "Emblem"

gTypeEmblem :: GType
gTypeEmblem =
  {# call fun unsafe g_emblem_get_type #}

-- *************************************************************** EmblemedIcon

{#pointer *GEmblemedIcon as EmblemedIcon foreign newtype #} deriving (Eq,Ord)

mkEmblemedIcon = (EmblemedIcon, objectUnref)
unEmblemedIcon (EmblemedIcon o) = o

class GObjectClass o => EmblemedIconClass o
toEmblemedIcon :: EmblemedIconClass o => o -> EmblemedIcon
toEmblemedIcon = unsafeCastGObject . toGObject

instance EmblemedIconClass EmblemedIcon
instance GObjectClass EmblemedIcon where
  toGObject = GObject . castForeignPtr . unEmblemedIcon
  unsafeCastGObject = EmblemedIcon . castForeignPtr . unGObject

castToEmblemedIcon :: GObjectClass obj => obj -> EmblemedIcon
castToEmblemedIcon = castTo gTypeEmblemedIcon "EmblemedIcon"

gTypeEmblemedIcon :: GType
gTypeEmblemedIcon =
  {# call fun unsafe g_emblemed_icon_get_type #}

-- ************************************************************* FileEnumerator

{#pointer *GFileEnumerator as FileEnumerator foreign newtype #} deriving (Eq,Ord)

mkFileEnumerator = (FileEnumerator, objectUnref)
unFileEnumerator (FileEnumerator o) = o

class GObjectClass o => FileEnumeratorClass o
toFileEnumerator :: FileEnumeratorClass o => o -> FileEnumerator
toFileEnumerator = unsafeCastGObject . toGObject

instance FileEnumeratorClass FileEnumerator
instance GObjectClass FileEnumerator where
  toGObject = GObject . castForeignPtr . unFileEnumerator
  unsafeCastGObject = FileEnumerator . castForeignPtr . unGObject

castToFileEnumerator :: GObjectClass obj => obj -> FileEnumerator
castToFileEnumerator = castTo gTypeFileEnumerator "FileEnumerator"

gTypeFileEnumerator :: GType
gTypeFileEnumerator =
  {# call fun unsafe g_file_enumerator_get_type #}

-- ********************************************************** FilenameCompleter

{#pointer *GFilenameCompleter as FilenameCompleter foreign newtype #} deriving (Eq,Ord)

mkFilenameCompleter = (FilenameCompleter, objectUnref)
unFilenameCompleter (FilenameCompleter o) = o

class GObjectClass o => FilenameCompleterClass o
toFilenameCompleter :: FilenameCompleterClass o => o -> FilenameCompleter
toFilenameCompleter = unsafeCastGObject . toGObject

instance FilenameCompleterClass FilenameCompleter
instance GObjectClass FilenameCompleter where
  toGObject = GObject . castForeignPtr . unFilenameCompleter
  unsafeCastGObject = FilenameCompleter . castForeignPtr . unGObject

castToFilenameCompleter :: GObjectClass obj => obj -> FilenameCompleter
castToFilenameCompleter = castTo gTypeFilenameCompleter "FilenameCompleter"

gTypeFilenameCompleter :: GType
gTypeFilenameCompleter =
  {# call fun unsafe g_filename_completer_get_type #}

-- ******************************************************************* FileIcon

{#pointer *GFileIcon as FileIcon foreign newtype #} deriving (Eq,Ord)

mkFileIcon = (FileIcon, objectUnref)
unFileIcon (FileIcon o) = o

class GObjectClass o => FileIconClass o
toFileIcon :: FileIconClass o => o -> FileIcon
toFileIcon = unsafeCastGObject . toGObject

instance FileIconClass FileIcon
instance GObjectClass FileIcon where
  toGObject = GObject . castForeignPtr . unFileIcon
  unsafeCastGObject = FileIcon . castForeignPtr . unGObject

castToFileIcon :: GObjectClass obj => obj -> FileIcon
castToFileIcon = castTo gTypeFileIcon "FileIcon"

gTypeFileIcon :: GType
gTypeFileIcon =
  {# call fun unsafe g_file_icon_get_type #}

-- ************************************************************** VolumeMonitor

{#pointer *GVolumeMonitor as VolumeMonitor foreign newtype #} deriving (Eq,Ord)

mkVolumeMonitor = (VolumeMonitor, objectUnref)
unVolumeMonitor (VolumeMonitor o) = o

class GObjectClass o => VolumeMonitorClass o
toVolumeMonitor :: VolumeMonitorClass o => o -> VolumeMonitor
toVolumeMonitor = unsafeCastGObject . toGObject

instance VolumeMonitorClass VolumeMonitor
instance GObjectClass VolumeMonitor where
  toGObject = GObject . castForeignPtr . unVolumeMonitor
  unsafeCastGObject = VolumeMonitor . castForeignPtr . unGObject

castToVolumeMonitor :: GObjectClass obj => obj -> VolumeMonitor
castToVolumeMonitor = castTo gTypeVolumeMonitor "VolumeMonitor"

gTypeVolumeMonitor :: GType
gTypeVolumeMonitor =
  {# call fun unsafe g_volume_monitor_get_type #}

-- **************************************************************** Cancellable

{#pointer *GCancellable as Cancellable foreign newtype #} deriving (Eq,Ord)

mkCancellable = (Cancellable, objectUnref)
unCancellable (Cancellable o) = o

class GObjectClass o => CancellableClass o
toCancellable :: CancellableClass o => o -> Cancellable
toCancellable = unsafeCastGObject . toGObject

instance CancellableClass Cancellable
instance GObjectClass Cancellable where
  toGObject = GObject . castForeignPtr . unCancellable
  unsafeCastGObject = Cancellable . castForeignPtr . unGObject

castToCancellable :: GObjectClass obj => obj -> Cancellable
castToCancellable = castTo gTypeCancellable "Cancellable"

gTypeCancellable :: GType
gTypeCancellable =
  {# call fun unsafe g_cancellable_get_type #}

-- ********************************************************** SimpleAsyncResult

{#pointer *GSimpleAsyncResult as SimpleAsyncResult foreign newtype #} deriving (Eq,Ord)

mkSimpleAsyncResult = (SimpleAsyncResult, objectUnref)
unSimpleAsyncResult (SimpleAsyncResult o) = o

class GObjectClass o => SimpleAsyncResultClass o
toSimpleAsyncResult :: SimpleAsyncResultClass o => o -> SimpleAsyncResult
toSimpleAsyncResult = unsafeCastGObject . toGObject

instance SimpleAsyncResultClass SimpleAsyncResult
instance GObjectClass SimpleAsyncResult where
  toGObject = GObject . castForeignPtr . unSimpleAsyncResult
  unsafeCastGObject = SimpleAsyncResult . castForeignPtr . unGObject

castToSimpleAsyncResult :: GObjectClass obj => obj -> SimpleAsyncResult
castToSimpleAsyncResult = castTo gTypeSimpleAsyncResult "SimpleAsyncResult"

gTypeSimpleAsyncResult :: GType
gTypeSimpleAsyncResult =
  {# call fun unsafe g_async_result_get_type #}

-- ******************************************************************* FileInfo

{#pointer *GFileInfo as FileInfo foreign newtype #} deriving (Eq,Ord)

mkFileInfo = (FileInfo, objectUnref)
unFileInfo (FileInfo o) = o

class GObjectClass o => FileInfoClass o
toFileInfo :: FileInfoClass o => o -> FileInfo
toFileInfo = unsafeCastGObject . toGObject

instance FileInfoClass FileInfo
instance GObjectClass FileInfo where
  toGObject = GObject . castForeignPtr . unFileInfo
  unsafeCastGObject = FileInfo . castForeignPtr . unGObject

castToFileInfo :: GObjectClass obj => obj -> FileInfo
castToFileInfo = castTo gTypeFileInfo "FileInfo"

gTypeFileInfo :: GType
gTypeFileInfo =
  {# call fun unsafe g_file_info_get_type #}

-- *********************************************************** AppLaunchContext

{#pointer *GAppLaunchContext as AppLaunchContext foreign newtype #} deriving (Eq,Ord)

mkAppLaunchContext = (AppLaunchContext, objectUnref)
unAppLaunchContext (AppLaunchContext o) = o

class FileInfoClass o => AppLaunchContextClass o
toAppLaunchContext :: AppLaunchContextClass o => o -> AppLaunchContext
toAppLaunchContext = unsafeCastGObject . toGObject

instance AppLaunchContextClass AppLaunchContext
instance FileInfoClass AppLaunchContext
instance GObjectClass AppLaunchContext where
  toGObject = GObject . castForeignPtr . unAppLaunchContext
  unsafeCastGObject = AppLaunchContext . castForeignPtr . unGObject

castToAppLaunchContext :: GObjectClass obj => obj -> AppLaunchContext
castToAppLaunchContext = castTo gTypeAppLaunchContext "AppLaunchContext"

gTypeAppLaunchContext :: GType
gTypeAppLaunchContext =
  {# call fun unsafe g_app_launch_context_get_type #}

-- *********************************************************************** Icon

{#pointer *GIcon as Icon foreign newtype #} deriving (Eq,Ord)

mkIcon = (Icon, objectUnref)
unIcon (Icon o) = o

class GObjectClass o => IconClass o
toIcon :: IconClass o => o -> Icon
toIcon = unsafeCastGObject . toGObject

instance IconClass Icon
instance GObjectClass Icon where
  toGObject = GObject . castForeignPtr . unIcon
  unsafeCastGObject = Icon . castForeignPtr . unGObject

castToIcon :: GObjectClass obj => obj -> Icon
castToIcon = castTo gTypeIcon "Icon"

gTypeIcon :: GType
gTypeIcon =
  {# call fun unsafe g_icon_get_type #}

-- ******************************************************************* Seekable

{#pointer *GSeekable as Seekable foreign newtype #} deriving (Eq,Ord)

mkSeekable = (Seekable, objectUnref)
unSeekable (Seekable o) = o

class GObjectClass o => SeekableClass o
toSeekable :: SeekableClass o => o -> Seekable
toSeekable = unsafeCastGObject . toGObject

instance SeekableClass Seekable
instance GObjectClass Seekable where
  toGObject = GObject . castForeignPtr . unSeekable
  unsafeCastGObject = Seekable . castForeignPtr . unGObject

castToSeekable :: GObjectClass obj => obj -> Seekable
castToSeekable = castTo gTypeSeekable "Seekable"

gTypeSeekable :: GType
gTypeSeekable =
  {# call fun unsafe g_seekable_get_type #}

-- ******************************************************************** AppInfo

{#pointer *GAppInfo as AppInfo foreign newtype #} deriving (Eq,Ord)

mkAppInfo = (AppInfo, objectUnref)
unAppInfo (AppInfo o) = o

class GObjectClass o => AppInfoClass o
toAppInfo :: AppInfoClass o => o -> AppInfo
toAppInfo = unsafeCastGObject . toGObject

instance AppInfoClass AppInfo
instance GObjectClass AppInfo where
  toGObject = GObject . castForeignPtr . unAppInfo
  unsafeCastGObject = AppInfo . castForeignPtr . unGObject

castToAppInfo :: GObjectClass obj => obj -> AppInfo
castToAppInfo = castTo gTypeAppInfo "AppInfo"

gTypeAppInfo :: GType
gTypeAppInfo =
  {# call fun unsafe g_app_info_get_type #}

-- ********************************************************************* Volume

{#pointer *GVolume as Volume foreign newtype #} deriving (Eq,Ord)

mkVolume = (Volume, objectUnref)
unVolume (Volume o) = o

class GObjectClass o => VolumeClass o
toVolume :: VolumeClass o => o -> Volume
toVolume = unsafeCastGObject . toGObject

instance VolumeClass Volume
instance GObjectClass Volume where
  toGObject = GObject . castForeignPtr . unVolume
  unsafeCastGObject = Volume . castForeignPtr . unGObject

castToVolume :: GObjectClass obj => obj -> Volume
castToVolume = castTo gTypeVolume "Volume"

gTypeVolume :: GType
gTypeVolume =
  {# call fun unsafe g_volume_get_type #}

-- **************************************************************** AsyncResult

{#pointer *GAsyncResult as AsyncResult foreign newtype #} deriving (Eq,Ord)

mkAsyncResult = (AsyncResult, objectUnref)
unAsyncResult (AsyncResult o) = o

class GObjectClass o => AsyncResultClass o
toAsyncResult :: AsyncResultClass o => o -> AsyncResult
toAsyncResult = unsafeCastGObject . toGObject

instance AsyncResultClass AsyncResult
instance GObjectClass AsyncResult where
  toGObject = GObject . castForeignPtr . unAsyncResult
  unsafeCastGObject = AsyncResult . castForeignPtr . unGObject

castToAsyncResult :: GObjectClass obj => obj -> AsyncResult
castToAsyncResult = castTo gTypeAsyncResult "AsyncResult"

gTypeAsyncResult :: GType
gTypeAsyncResult =
  {# call fun unsafe g_async_result_get_type #}

-- *************************************************************** LoadableIcon

{#pointer *GLoadableIcon as LoadableIcon foreign newtype #} deriving (Eq,Ord)

mkLoadableIcon = (LoadableIcon, objectUnref)
unLoadableIcon (LoadableIcon o) = o

class GObjectClass o => LoadableIconClass o
toLoadableIcon :: LoadableIconClass o => o -> LoadableIcon
toLoadableIcon = unsafeCastGObject . toGObject

instance LoadableIconClass LoadableIcon
instance GObjectClass LoadableIcon where
  toGObject = GObject . castForeignPtr . unLoadableIcon
  unsafeCastGObject = LoadableIcon . castForeignPtr . unGObject

castToLoadableIcon :: GObjectClass obj => obj -> LoadableIcon
castToLoadableIcon = castTo gTypeLoadableIcon "LoadableIcon"

gTypeLoadableIcon :: GType
gTypeLoadableIcon =
  {# call fun unsafe g_loadable_icon_get_type #}

-- ********************************************************************** Drive

{#pointer *GDrive as Drive foreign newtype #} deriving (Eq,Ord)

mkDrive = (Drive, objectUnref)
unDrive (Drive o) = o

class GObjectClass o => DriveClass o
toDrive :: DriveClass o => o -> Drive
toDrive = unsafeCastGObject . toGObject

instance DriveClass Drive
instance GObjectClass Drive where
  toGObject = GObject . castForeignPtr . unDrive
  unsafeCastGObject = Drive . castForeignPtr . unGObject

castToDrive :: GObjectClass obj => obj -> Drive
castToDrive = castTo gTypeDrive "Drive"

gTypeDrive :: GType
gTypeDrive =
  {# call fun unsafe g_drive_get_type #}

-- *********************************************************************** File

{#pointer *GFile as File foreign newtype #}

mkFile = (File, objectUnref)
unFile (File o) = o

class GObjectClass o => FileClass o
toFile :: FileClass o => o -> File
toFile = unsafeCastGObject . toGObject

instance FileClass File
instance GObjectClass File where
  toGObject = GObject . castForeignPtr . unFile
  unsafeCastGObject = File . castForeignPtr . unGObject

castToFile :: GObjectClass obj => obj -> File
castToFile = castTo gTypeFile "File"

gTypeFile :: GType
gTypeFile =
  {# call fun unsafe g_file_get_type #}

-- ********************************************************************** Mount

{#pointer *GMount as Mount foreign newtype #} deriving (Eq,Ord)

mkMount = (Mount, objectUnref)
unMount (Mount o) = o

class GObjectClass o => MountClass o
toMount :: MountClass o => o -> Mount
toMount = unsafeCastGObject . toGObject

instance MountClass Mount
instance GObjectClass Mount where
  toGObject = GObject . castForeignPtr . unMount
  unsafeCastGObject = Mount . castForeignPtr . unGObject

castToMount :: GObjectClass obj => obj -> Mount
castToMount = castTo gTypeMount "Mount"

gTypeMount :: GType
gTypeMount =
  {# call fun unsafe g_mount_get_type #}

