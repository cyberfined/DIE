module Texture.Classes (
    Texture2DFormat(..)
  ) where

import Graphics.Rendering.OpenGL
import Data.ByteString(ByteString) 

class Texture2DFormat f where
  loadTexture2D :: (BindableTextureTarget t, TwoDimensionalTextureTarget t) => f -> t -> ByteString -> IO TextureObject
