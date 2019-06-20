module Texture.DDS (
    DDS(..)
  ) where

import Graphics.Rendering.OpenGL
import Graphics.GL.EXT.TextureCompressionS3TC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Unsafe(unsafeUseAsCStringLen)
import Data.Binary.Get
import Data.Word
import Data.Bits
import Foreign.Ptr
import Foreign.C.Types
import Control.Monad(when, unless)
import Texture.Classes

data DDS = DDS

ddsSign :: Word32
ddsSign = 0x20534444 -- 'DDS '

formatDXT1, formatDXT3, formatDXT5 :: Word32
formatDXT1 = 0x31545844 -- 'DXT1'
formatDXT3 = 0x33545844 -- 'DXT3'
formatDXT5 = 0x35545844 -- 'DXT5'

data Header = Header 
  { sign :: !Word32
  -- size 4 bytes
  -- flags 4 bytes
  , height :: !Word32
  , width :: !Word32
  -- pitchOrLinearSize 4 bytes
  , depth :: !Word32
  , mipMapCount :: !Word32
  -- Reserved1 44 bytes
  -- size 4 bytes
  , flags :: !Word32
  , fourCC :: !Word32
  -- 40 bytes 
}

data Image = Image
  { header :: !Header
  , internalFormat :: !GLenum
  , blockSize :: !Word32 }

instance Texture2DFormat DDS where
  loadTexture2D _ t s = loadDDS2D t s

loadDDS2D :: (BindableTextureTarget t, TwoDimensionalTextureTarget t) => t -> BS.ByteString -> IO TextureObject
loadDDS2D t s = 
  let (hdBytes, body) = takeLazyHeader s in case runGetOrFail parseImage hdBytes of
  Left (_, _, err) -> fail err
  Right (_, _, image) -> do
    tex <- genObjectName
    textureBinding t $= Just tex
    let hd = header image
    unsafeUseAsCStringLen body $ \(ptr, size) ->
      loadMipMaps t 0 (fromIntegral $ mipMapCount hd) (fromIntegral $ blockSize image) (internalFormat image) (fromIntegral $ width hd) (fromIntegral $ height hd) ptr (plusPtr ptr size)
    textureBinding t $= Nothing
    return tex

loadMipMaps :: TwoDimensionalTextureTarget t => t -> GLint -> GLint -> GLsizei -> GLenum -> GLsizei -> GLsizei -> Ptr CChar -> Ptr CChar -> IO ()
loadMipMaps target level maxLevel blockSize internalFormat width height ptr maxPtr =
  when (level < maxLevel) $ do
    unless (ptr < maxPtr) $ fail "Wrong texture file size"
    let size = ((width+3) `div` 4) * ((height+3) `div` 4) * blockSize
        newWidth = max 1 (width `div` 2)
        newHeight = max 1 (height `div` 2)
    compressedTexImage2D target NoProxy level (TextureSize2D width height) 0 (CompressedPixelData (CompressedTextureFormat internalFormat) size ptr)
    loadMipMaps target (level+1) maxLevel blockSize internalFormat newWidth newHeight (plusPtr ptr $ fromIntegral size) maxPtr

parseImage :: Get Image
parseImage = parseHeader >>= \header ->
  if isBadSign header
     then fail "Invalid DDS signature"
     else flip (maybe $ fail "Unsupported DDS format") (getFormat header) $ \f ->
       let bs = getBlockSize f
        in return $ Image { header = header, internalFormat = f, blockSize = bs }

parseHeader :: Get Header
parseHeader = do
  sign <- getWord32le
  skip 8
  height <- getWord32le
  width <- getWord32le
  skip 4
  depth <- getWord32le
  mipMapCount <- getWord32le
  skip 48
  flags <- getWord32le
  fourCC <- getWord32le
  skip 40
  return $ Header sign height width depth mipMapCount flags fourCC

takeLazyHeader :: BS.ByteString -> (BL.ByteString, BS.ByteString)
takeLazyHeader s = let (hd, body) = BS.splitAt 128 s in (BL.fromStrict hd, body)

isBadSign :: Header -> Bool
isBadSign header = sign header /= ddsSign

getFormat :: Header -> Maybe GLenum
getFormat header
  | flags header .&. 0x4 == 0 = Nothing
  | otherwise = format $ fourCC header
  where format cc
          | cc == formatDXT1 = Just $ GL_COMPRESSED_RGBA_S3TC_DXT1_EXT
          | cc == formatDXT3 = Just $ GL_COMPRESSED_RGBA_S3TC_DXT3_EXT
          | cc == formatDXT5 = Just $ GL_COMPRESSED_RGBA_S3TC_DXT5_EXT
          | otherwise = Nothing

getBlockSize :: GLenum -> Word32
getBlockSize format
  | format == GL_COMPRESSED_RGBA_S3TC_DXT1_EXT = 8
  | otherwise = 16
