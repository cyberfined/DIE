module Texture.Utils (
    loadFromFileTexture2D,
    withTexture
  ) where

import Graphics.Rendering.OpenGL
import qualified Data.ByteString as BS
import Texture.Classes

withTexture :: BindableTextureTarget t => t -> TextureObject -> IO () -> IO ()
withTexture t o a = do
  textureBinding t $= Just o
  a
  textureBinding t $= Nothing

loadFromFileTexture2D :: (BindableTextureTarget t, TwoDimensionalTextureTarget t, Texture2DFormat f) => f -> t -> FilePath -> IO TextureObject
loadFromFileTexture2D f t p = BS.readFile p >>= loadTexture2D f t
