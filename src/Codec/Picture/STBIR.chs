{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Codec.Picture.STBIR
( -- * Resize function
  resize
  -- * Options
, Options(..)
, defaultOptions
, Flag
, flag_ALPHA_PREMULTIPLIED
, flag_ALPHA_USES_COLORSPACE
, Edge(..)
, Filter(..)
, Colorspace(..)
, Scale(..)
, Region(..)
  -- * Supported pixel types
, STBIRPixel(..)
, STBIRComponent(..)
, Datatype(..)
) where

import Foreign
import Foreign.C
import Codec.Picture
import Codec.Picture.Types
import qualified Data.Vector.Storable as V
import System.IO.Unsafe (unsafePerformIO)
import Data.Default.Class (Default(..))

#include "stb_image_resize.h"

newtype Flag = Flag { fromFlag :: CInt }
  deriving (Eq, Ord, Show, Read)

{- |
Set this flag if your texture has premultiplied alpha. Otherwise, `stbir` will
use alpha-weighted resampling (effectively premultiplying, resampling,
then unpremultiplying).
-}
flag_ALPHA_PREMULTIPLIED :: Flag
flag_ALPHA_PREMULTIPLIED = Flag 1 -- c2hs const has problems due to <<

{- |
The specified alpha channel should be handled as gamma-corrected value even
when doing sRGB operations.
-}
flag_ALPHA_USES_COLORSPACE :: Flag
flag_ALPHA_USES_COLORSPACE = Flag 2

{#enum stbir_edge as Edge {}
  with prefix = "STBIR_"
  deriving (Eq, Ord, Show, Read, Bounded)
#}

data Filter
  = FILTER_DEFAULT      -- ^ use same filter type that easy-to-use API chooses
  | FILTER_BOX          -- ^ A trapezoid w/1-pixel wide ramps, same result as box for integer scale ratios
  | FILTER_TRIANGLE     -- ^ On upsampling, produces same results as bilinear texture filtering
  | FILTER_CUBICBSPLINE -- ^ The cubic b-spline (aka Mitchell-Netrevalli with B=1,C=0), gaussian-esque
  | FILTER_CATMULLROM   -- ^ An interpolating cubic spline
  | FILTER_MITCHELL     -- ^ Mitchell-Netrevalli filter with B=1\/3, C=1\/3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- the separate types here are because c2hs doesn't let me insert haddocks
{#enum stbir_filter as C_Filter {}
  deriving (Eq, Ord, Show, Read, Bounded)
#}

cFilter :: Filter -> CInt
cFilter FILTER_DEFAULT      = fromIntegral $ fromEnum STBIR_FILTER_DEFAULT
cFilter FILTER_BOX          = fromIntegral $ fromEnum STBIR_FILTER_BOX
cFilter FILTER_TRIANGLE     = fromIntegral $ fromEnum STBIR_FILTER_TRIANGLE
cFilter FILTER_CUBICBSPLINE = fromIntegral $ fromEnum STBIR_FILTER_CUBICBSPLINE
cFilter FILTER_CATMULLROM   = fromIntegral $ fromEnum STBIR_FILTER_CATMULLROM
cFilter FILTER_MITCHELL     = fromIntegral $ fromEnum STBIR_FILTER_MITCHELL

{#enum stbir_colorspace as Colorspace {}
  omit (STBIR_MAX_COLORSPACES)
  with prefix = "STBIR_"
  deriving (Eq, Ord, Show, Read, Bounded)
#}

{#enum stbir_datatype as Datatype {}
  omit (STBIR_MAX_TYPES)
  with prefix = "STBIR_"
#}

-- | Specify scale explicitly for subpixel correctness
data Scale = Scale
  { x_scale :: Float
  , y_scale :: Float
  , x_offset :: Float
  , y_offset :: Float
  } deriving (Eq, Ord, Show, Read)

-- | Specify image source tile using texture coordinates
data Region = Region
  { region_s0 :: Float -- ^ x of top-left corner from 0 to 1
  , region_t0 :: Float -- ^ y of top-left corner from 0 to 1
  , region_s1 :: Float -- ^ x of bottom-right corner from 0 to 1
  , region_t1 :: Float -- ^ y of bottom-right corner from 0 to 1
  } deriving (Eq, Ord, Show, Read)

data Options = Options
  { flags :: [Flag]
  , edgeModeHorizontal :: Edge
  , edgeModeVertical :: Edge
  , filterHorizontal :: Filter
  , filterVertical :: Filter
  , colorspace :: Colorspace
  , transform :: Either Scale Region
  } deriving (Eq, Ord, Show, Read)

-- | These are the options that correspond to the \"Easy-to-use API\".
defaultOptions :: Options
defaultOptions = Options
  { flags = []
  , edgeModeHorizontal = EDGE_CLAMP
  , edgeModeVertical = EDGE_CLAMP
  , filterHorizontal = FILTER_DEFAULT
  , filterVertical = FILTER_DEFAULT
  , colorspace = COLORSPACE_LINEAR
  , transform = Right $ Region 0 0 1 1
  }

instance Default Options where
  def = defaultOptions

class STBIRComponent a where
  stbirType :: a -> Datatype
instance STBIRComponent Word8  where stbirType _ = TYPE_UINT8
instance STBIRComponent Word16 where stbirType _ = TYPE_UINT16
instance STBIRComponent Word32 where stbirType _ = TYPE_UINT32
instance STBIRComponent Float  where stbirType _ = TYPE_FLOAT

noAlpha :: CInt
noAlpha = {#const STBIR_ALPHA_CHANNEL_NONE #}

-- | All types currently covered by JP's 'Pixel' are supported.
class (Pixel a, STBIRComponent (PixelBaseComponent a)) => STBIRPixel a where
  alphaIndex :: a -> Maybe Int
instance STBIRPixel PixelRGBA16  where alphaIndex _ = Just 3
instance STBIRPixel PixelRGBA8   where alphaIndex _ = Just 3
instance STBIRPixel PixelCMYK16  where alphaIndex _ = Nothing
instance STBIRPixel PixelCMYK8   where alphaIndex _ = Nothing
instance STBIRPixel PixelYCbCr8  where alphaIndex _ = Nothing
instance STBIRPixel PixelRGBF    where alphaIndex _ = Nothing
instance STBIRPixel PixelRGB16   where alphaIndex _ = Nothing
instance STBIRPixel PixelYCbCrK8 where alphaIndex _ = Nothing
instance STBIRPixel PixelRGB8    where alphaIndex _ = Nothing
instance STBIRPixel PixelYA16    where alphaIndex _ = Just 1
instance STBIRPixel PixelYA8     where alphaIndex _ = Just 1
instance STBIRPixel PixelF       where alphaIndex _ = Nothing
instance STBIRPixel Pixel32      where alphaIndex _ = Nothing
instance STBIRPixel Pixel16      where alphaIndex _ = Nothing
instance STBIRPixel Pixel8       where alphaIndex _ = Nothing

{#fun stbir_resize_subpixel
  { id `Ptr ()' -- ^ const void *input_pixels
  , `CInt' -- ^ int input_w
  , `CInt' -- ^ int input_h
  , `CInt' -- ^ int input_stride_in_bytes
  , id `Ptr ()' -- ^ void *output_pixels
  , `CInt' -- ^ int output_w
  , `CInt' -- ^ int output_h
  , `CInt' -- ^ int output_stride_in_bytes
  , `Datatype' -- ^ stbir_datatype datatype
  , `CInt' -- ^ int num_channels
  , `CInt' -- ^ int alpha_channel
  , `CInt' -- ^ int flags
  , `Edge' -- ^ stbir_edge edge_mode_horizontal
  , `Edge' -- ^ stbir_edge edge_mode_vertical
  , cFilter `Filter' -- ^ stbir_filter filter_horizontal
  , cFilter `Filter' -- ^ stbir_filter filter_vertical
  , `Colorspace' -- ^ stbir_colorspace space
  , id `Ptr ()' -- ^ void *alloc_context
  , `CFloat' -- ^ float x_scale
  , `CFloat' -- ^ float y_scale
  , `CFloat' -- ^ float x_offset
  , `CFloat' -- ^ float y_offset
  } -> `CInt'
#}

{#fun stbir_resize_region
  { id `Ptr ()' -- ^ const void *input_pixels
  , `CInt' -- ^ int input_w
  , `CInt' -- ^ int input_h
  , `CInt' -- ^ int input_stride_in_bytes
  , id `Ptr ()' -- ^ void *output_pixels
  , `CInt' -- ^ int output_w
  , `CInt' -- ^ int output_h
  , `CInt' -- ^ int output_stride_in_bytes
  , `Datatype' -- ^ stbir_datatype datatype
  , `CInt' -- ^ int num_channels
  , `CInt' -- ^ int alpha_channel
  , `CInt' -- ^ int flags
  , `Edge' -- ^ stbir_edge edge_mode_horizontal
  , `Edge' -- ^ stbir_edge edge_mode_vertical
  , cFilter `Filter' -- ^ stbir_filter filter_horizontal
  , cFilter `Filter' -- ^ stbir_filter filter_vertical
  , `Colorspace' -- ^ stbir_colorspace space
  , id `Ptr ()' -- ^ void *alloc_context
  , `CFloat' -- ^ float s0
  , `CFloat' -- ^ float t0
  , `CFloat' -- ^ float s1
  , `CFloat' -- ^ float t1
  } -> `CInt'
#}

pixelProperty :: (a -> b) -> Image a -> b
pixelProperty f img = let
  getFakePixel :: Image a -> a
  getFakePixel = undefined
  in f $ getFakePixel img

-- | This function allows access to all \"API levels\" of the C library.
-- Pass 'defaultOptions' to use the easy API, or override whichever options
-- you need.
resize
  :: (STBIRPixel a)
  => Options
  -> Int -- ^ new width
  -> Int -- ^ new height
  -> Image a
  -> Image a
resize opts w' h' img@(Image w h v) = unsafePerformIO $ do
  V.unsafeWith v $ \p -> do
    let comps = pixelProperty componentCount img
    fp <- mallocForeignPtrArray $ w' * h' * comps
    let (cfun, f0, f1, f2, f3) = case transform opts of
          Left  (Scale  a b c d) -> (stbir_resize_subpixel, a, b, c, d)
          Right (Region a b c d) -> (stbir_resize_region  , a, b, c, d)
    res <- withForeignPtr fp $ \p' -> cfun
      (castPtr p)
      (fromIntegral w)
      (fromIntegral h)
      0
      (castPtr p')
      (fromIntegral w')
      (fromIntegral h')
      0
      (pixelProperty (stbirType . pixelOpacity) img)
      (fromIntegral comps)
      (maybe noAlpha fromIntegral $ pixelProperty alphaIndex img)
      (fromIntegral $ foldr (.|.) 0 $ map fromFlag $ flags opts)
      (edgeModeHorizontal opts)
      (edgeModeVertical opts)
      (filterHorizontal opts)
      (filterVertical opts)
      (colorspace opts)
      nullPtr
      (realToFrac f0)
      (realToFrac f1)
      (realToFrac f2)
      (realToFrac f3)
    if res == 0
      then error "Codec.Picture.STBIR.resize returned an error"
      else return $ Image w' h' $ V.unsafeFromForeignPtr0 fp $ w' * h' * comps
