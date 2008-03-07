{-# OPTIONS -fffi -fglasgow-exts #-}
module FOV {-(
	Settings,
	Shape(..),
	OpaqueApply(..),
	Direction(..),
	) -}where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr

#include "fov.h"

type OpacityTestFn a = Ptr a -> Int -> Int -> IO Bool
foreign import ccall "wrapper" mkOpacityTestFn :: OpacityTestFn a -> IO (FunPtr (OpacityTestFn a))

type ApplyFn a b = Ptr a -> Int -> Int -> Int -> Int -> Ptr b -> IO ()
foreign import ccall "wrapper" mkApplyFn :: ApplyFn a b -> IO (FunPtr (ApplyFn a b))

data SettingsRaw
newtype Settings = Settings (ForeignPtr SettingsRaw)

data Shape = CirclePrecalculate | Square | Circle | Octagon
data OpaqueApply = OpaqueApply | OpaqueNoApply
data Direction = East | NorthEast | North | NorthWest | West | SouthWest | South | SouthEast

rawshape CirclePrecalculate = #const FOV_SHAPE_CIRCLE_PRECALCULATE
rawshape Square = #const FOV_SHAPE_SQUARE
rawshape Circle = #const FOV_SHAPE_CIRCLE
rawshape Octagon = #const FOV_SHAPE_OCTAGON

rawopaqueapply OpaqueApply = #const FOV_OPAQUE_APPLY
rawopaqueapply OpaqueNoApply = #const FOV_OPAQUE_NOAPPLY

rawdirection East = #const FOV_EAST
rawdirection NorthEast = #const FOV_NORTHEAST
rawdirection North = #const FOV_NORTH
rawdirection NorthWest = #const FOV_NORTHWEST
rawdirection West = #const FOV_WEST
rawdirection SouthWest = #const FOV_SOUTHWEST
rawdirection South = #const FOV_SOUTH
rawdirection SouthEast = #const FOV_SOUTHEAST

foreign import ccall unsafe new_fov_settings :: IO (Ptr SettingsRaw)
foreign import ccall unsafe fov_settings_set_shape :: Ptr SettingsRaw -> Int -> IO ()
foreign import ccall unsafe fov_settings_set_opaque_apply :: Ptr SettingsRaw -> Int -> IO ()
foreign import ccall unsafe fov_settings_set_apply_lighting_function :: Ptr SettingsRaw -> FunPtr (ApplyFn a b) -> IO ()
foreign import ccall unsafe fov_settings_set_opacity_test_function :: Ptr SettingsRaw -> FunPtr (OpacityTestFn a) -> IO ()
foreign import ccall unsafe "&fov_settings_free" fov_settings_free :: FunPtr (Ptr SettingsRaw -> IO ())
foreign import ccall fov_circle :: Ptr SettingsRaw -> Ptr map -> Ptr src -> Int -> Int -> Int -> IO ()
foreign import ccall fov_beam :: Ptr SettingsRaw -> Ptr map -> Ptr src -> Int -> Int -> Int -> Int -> Int -> IO ()

newSettings :: IO Settings
newSettings = do
	s <- new_fov_settings >>= newForeignPtr fov_settings_free
	return $ Settings s
