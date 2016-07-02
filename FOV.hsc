{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CPP #-}
-- | HFOV is a library for calculating field of view in a 2D raster grid, such
-- as those found in roguelike games.
--
-- Thanks to Greg McIntyre for the original C library to which this Haskell
-- library binds. It can be found at <http://libfov.sourceforge.net/wiki>.
module FOV (
		Settings,
		Shape(..),
		Direction(..),
		newSettings,    -- IO Settings
		setShape,       -- Settings -> Shape -> IO ()
		setOpaqueApply, -- Settings -> Bool -> IO ()
		circle,
		beam
	) where

import Foreign.Ptr
import Foreign.ForeignPtr

#include "cfov.h"

type OpacityTestFn a = Ptr a -> Int -> Int -> IO Bool
foreign import ccall "wrapper" mkOpacityTestFn :: OpacityTestFn a -> IO (FunPtr (OpacityTestFn a))

type ApplyFn map src = Ptr map -> Int -> Int -> Int -> Int -> Ptr src -> IO ()
foreign import ccall "wrapper" mkApplyFn :: ApplyFn a b -> IO (FunPtr (ApplyFn a b))

data SettingsRaw

-- | Opaque data structure for holding information about FOV calculation.
newtype Settings = Settings (ForeignPtr SettingsRaw)

-- | Shape of the field.
data Shape
	-- | Limit the field of view to a circle radius R by precalculating the
	-- circle shape. This consumes memory at the rate of 4*(R+2) bytes per R
	-- used in calls to fovCircle. Each radius is only calculated once.
	= CirclePrecalculate
	-- | Limit the field to a circle shape calculated on the fly.
	| Circle
	-- | Limit the field to an octagon with maximum radius R.
	| Octagon
	-- | Limit the field to an RxR square.
	| Square

data Direction = East | NorthEast | North | NorthWest | West | SouthWest | South | SouthEast

rawshape :: (Num t) => Shape -> t
rawshape CirclePrecalculate = #const FOV_SHAPE_CIRCLE_PRECALCULATE
rawshape Square = #const FOV_SHAPE_SQUARE
rawshape Circle = #const FOV_SHAPE_CIRCLE
rawshape Octagon = #const FOV_SHAPE_OCTAGON

rawdirection :: (Num t) => Direction -> t
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

-- | Create a new FOV settings structure.
newSettings :: IO Settings
newSettings = do
	s <- new_fov_settings >>= newForeignPtr fov_settings_free
	return $ Settings s

-- | Set the shape of the field of view. The default is CirclePrecalculate.
setShape :: Settings -> Shape -> IO ()
setShape (Settings fps) sh = withForeignPtr fps $ \s -> do
	fov_settings_set_shape s (rawshape sh)

-- | Sets whether or not to apply lighting to opaque squares.
setOpaqueApply :: Settings -> Bool -> IO ()
setOpaqueApply (Settings fps) a = withForeignPtr fps $ \s -> do
	fov_settings_set_opaque_apply s (if a then (#const FOV_OPAQUE_APPLY) else (#const FOV_OPAQUE_NOAPPLY))

-- | Cast a 360 degree field of view.
circle :: Settings                -- ^The FOV settings structure to use
       -> (Int,Int)               -- ^The centre of the field
       -> Int                     -- ^The radius
       -> (Int -> Int -> IO ())   -- ^The function to be called in order to apply light to a certain position
       -> (Int -> Int -> IO Bool) -- ^The function to determine the opacity of a certain cell
       -> IO ()
circle (Settings fps) (x,y) r apply opaque = withForeignPtr fps $ \s -> do
	fov_settings_set_apply_lighting_function s =<< mkApplyFn (\_ x' y' _ _ _ -> apply x' y')
	fov_settings_set_opacity_test_function s =<< mkOpacityTestFn (\_ x' y' -> opaque x' y')
	fov_circle s nullPtr nullPtr x y r

-- | Cast a beam.
beam :: Settings                -- ^The FOV settings structure to use
     -> (Int,Int)               -- ^Origin of the beam
     -> Int                     -- ^Length (radius) of the beam
     -> Direction               -- ^Direction of the centre of the beam
     -> Float                   -- ^Width of the beam (in degrees)
     -> (Int -> Int -> IO ())   -- ^Light application function
     -> (Int -> Int -> IO Bool) -- ^Opacity test function
     -> IO ()
beam (Settings fps) (x,y) r dir angle apply opaque = withForeignPtr fps $ \s -> do
	fov_settings_set_apply_lighting_function s =<< mkApplyFn (\_ x' y' _ _ _ -> apply x' y')
	fov_settings_set_opacity_test_function s =<< mkOpacityTestFn (\_ x' y' -> opaque x' y')
	fov_beam s nullPtr nullPtr x y r (rawdirection dir) angle
