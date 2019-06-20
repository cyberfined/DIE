module Shader (
    loadShaders
  ) where

import Control.Exception(bracketOnError)
import Graphics.Rendering.OpenGL
import Control.Monad (unless)
import qualified Data.ByteString as BS

loadShaders :: [(ShaderType, FilePath)] -> IO Program
loadShaders shaders =
  createProgram `bracketOnError` deleteObjectName $ \program -> do
    mapM_ (uncurry $ attachAndCompile program) shaders
    check linkProgram linkStatus programInfoLog program
    return program

attachAndCompile :: Program -> ShaderType -> FilePath -> IO ()
attachAndCompile program t s = 
  createShader t `bracketOnError` deleteObjectName $ \shader -> do
    src <- BS.readFile s
    shaderSourceBS shader $= src
    check compileShader compileStatus shaderInfoLog shader
    attachShader program shader

check :: (a -> IO ())
      -> (a -> IO Bool)
      -> (a -> IO String)
      -> a
      -> IO ()
check action getStatus getLog obj = do
    action obj
    status <- getStatus obj
    unless status $ do
        log <- getLog obj
        fail $ log
