{-# LANGUAGE TypeFamilies #-}
module Codec.Picture.STBIR
( stbir_resize_uint8
, stbir_resize_float
) where

import Foreign
import Foreign.C
import Codec.Picture
import qualified Data.Vector.Storable as V
import System.IO.Unsafe (unsafePerformIO)

#include "stb_image_resize.h"

imageComponentCount :: (Pixel a) => Image a -> Int
imageComponentCount img = let
  getFakePixel :: Image a -> a
  getFakePixel = undefined
  in componentCount $ getFakePixel img

{#fun stbir_resize_uint8 as c_stbir_resize_uint8
  { id `Ptr CUChar'
  , `CInt'
  , `CInt'
  , `CInt'
  , id `Ptr CUChar'
  , `CInt'
  , `CInt'
  , `CInt'
  , `CInt'
  } -> `CInt'
#}

stbir_resize_uint8
  :: (Pixel a, PixelBaseComponent a ~ Word8)
  => Int -> Int -> Image a -> Image a
stbir_resize_uint8 w' h' img@(Image w h v) = unsafePerformIO $ do
  V.unsafeWith v $ \p -> do
    let parts = imageComponentCount img
    fp <- mallocForeignPtrBytes $ w' * h' * parts
    let cast = castPtr :: Ptr Word8 -> Ptr CUChar
    res <- withForeignPtr fp $ \p' -> c_stbir_resize_uint8
      (cast p)
      (fromIntegral w)
      (fromIntegral h)
      0
      (cast p')
      (fromIntegral w')
      (fromIntegral h')
      0
      (fromIntegral parts)
    if res == 0
      then error "Codec.Picture.STBIR.stbir_resize_uint8 returned an error"
      else return $ Image w' h' $ V.unsafeFromForeignPtr0 fp $ w' * h' * parts

{#fun stbir_resize_float as c_stbir_resize_float
  { id `Ptr CFloat'
  , `CInt'
  , `CInt'
  , `CInt'
  , id `Ptr CFloat'
  , `CInt'
  , `CInt'
  , `CInt'
  , `CInt'
  } -> `CInt'
#}

stbir_resize_float
  :: (Pixel a, PixelBaseComponent a ~ Float)
  => Int -> Int -> Image a -> Image a
stbir_resize_float w' h' img@(Image w h v) = unsafePerformIO $ do
  V.unsafeWith v $ \p -> do
    let parts = imageComponentCount img
    fp <- mallocForeignPtrBytes $ w' * h' * parts
    let cast = castPtr :: Ptr Float -> Ptr CFloat
    res <- withForeignPtr fp $ \p' -> c_stbir_resize_float
      (cast p)
      (fromIntegral w)
      (fromIntegral h)
      0
      (cast p')
      (fromIntegral w')
      (fromIntegral h')
      0
      (fromIntegral parts)
    if res == 0
      then error "Codec.Picture.STBIR.stbir_resize_float returned an error"
      else return $ Image w' h' $ V.unsafeFromForeignPtr0 fp $ w' * h' * parts
