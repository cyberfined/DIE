module Vertices (
    storeBuffer,
    createVao,
    createVaoEbo
  ) where

import Graphics.Rendering.OpenGL
import Control.Monad (forM_)
import Foreign.Marshal.Array(withArray)
import Foreign.Ptr(Ptr,IntPtr(..), intPtrToPtr, nullPtr)
import Foreign.Storable(Storable(..))
import Utils

storeBuffer :: Storable a => BufferTarget -> BufferUsage -> [a] -> IO ()
storeBuffer t u d = withArray d $ \ptr -> do
    let size = fromIntegral $ length d * sizeOf(head d)
    bufferData t $= (size, ptr, u)

createVao :: Storable a => [a] -> [Int] -> DataType -> Int -> IO VertexArrayObject
createVao vertices attribs dt row = do
    vao <- genObjectName
    bindVertexArrayObject $= Just vao

    vbo <- genObjectName
    bindBuffer ArrayBuffer $= Just vbo
    storeBuffer ArrayBuffer StaticDraw vertices

    forM_ (zip3 (iterate (+1) 0) attribs (init $ scanl (+) 0 attribs)) $ \(i,s,p) -> do
        let loc = AttribLocation i
            num = fromIntegral s
            stride = fromIntegral $ row * sizeOf(head vertices)
            ptr = intToPtr $ fromIntegral $ p * sizeOf(head vertices)

        vertexAttribPointer loc $= (ToFloat, VertexArrayDescriptor num dt stride ptr)
        vertexAttribArray loc $= Enabled

    bindVertexArrayObject $= Nothing
    return vao

createVaoEbo :: Storable a => [a] -> [Int] -> [Int] -> DataType -> Int -> IO VertexArrayObject
createVaoEbo vertices indices attribs dt row = do
    vao <- createVao vertices attribs dt row
    bindVertexArrayObject $= Just vao
    
    ebo <- genObjectName
    bindBuffer ElementArrayBuffer $= Just ebo
    storeBuffer ElementArrayBuffer StaticDraw (map fromIntegral $ indices :: [GLuint])

    bindVertexArrayObject $= Nothing
    return vao
