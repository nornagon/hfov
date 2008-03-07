{-# OPTIONS -fffi -fglasgow-exts #-}
module FOV (
	Settings,
	Shape(..),
	Direction(..),
	newSettings,    -- IO Settings
	setShape,       -- Settings -> Shape -> IO ()
	setOpaqueApply, -- Settings -> Bool -> IO ()
	fovCircle       -- Settings -> (Int, Int) -> Int -> (Int -> Int -> IO ()) -> (Int -> Int -> IO Bool) -> IO ()
	) where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr

#include "fov.h"

type OpacityTestFn a = Ptr a -> Int -> Int -> IO Bool
foreign import ccall "wrapper" mkOpacityTestFn :: OpacityTestFn a -> IO (FunPtr (OpacityTestFn a))

type ApplyFn map src = Ptr map -> Int -> Int -> Int -> Int -> Ptr src -> IO ()
foreign import ccall "wrapper" mkApplyFn :: ApplyFn a b -> IO (FunPtr (ApplyFn a b))

data SettingsRaw
newtype Settings = Settings (ForeignPtr SettingsRaw)

data Shape = CirclePrecalculate | Square | Circle | Octagon
data Direction = East | NorthEast | North | NorthWest | West | SouthWest | South | SouthEast

rawshape CirclePrecalculate = #const FOV_SHAPE_CIRCLE_PRECALCULATE
rawshape Square = #const FOV_SHAPE_SQUARE
rawshape Circle = #const FOV_SHAPE_CIRCLE
rawshape Octagon = #const FOV_SHAPE_OCTAGON

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
foreign import ccall fov_beam :: Ptr SettingsRaw -> Ptr map -> Ptr src -> Int -> Int -> Int -> Int -> Float -> IO ()

newSettings :: IO Settings
newSettings = do
	s <- new_fov_settings >>= newForeignPtr fov_settings_free
	return $ Settings s

setShape :: Settings -> Shape -> IO ()
setShape (Settings fps) sh = withForeignPtr fps $ \s -> do
	fov_settings_set_shape s (rawshape sh)

setOpaqueApply :: Settings -> Bool -> IO ()
setOpaqueApply (Settings fps) a = withForeignPtr fps $ \s -> do
	fov_settings_set_opaque_apply s (if a then (#const FOV_OPAQUE_APPLY) else (#const FOV_OPAQUE_NOAPPLY))

fovCircle :: Settings -> (Int,Int) -> Int -> (Int -> Int -> IO ()) -> (Int -> Int -> IO Bool) -> IO ()
fovCircle (Settings fps) (x,y) r apply opaque = withForeignPtr fps $ \s -> do
	fov_settings_set_apply_lighting_function s =<< mkApplyFn (\_ x y _ _ _ -> apply x y)
	fov_settings_set_opacity_test_function s =<< mkOpacityTestFn (\_ x y -> opaque x y)
	fov_circle s nullPtr nullPtr x y r
