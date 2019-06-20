module Linear.Storable (
    v2uniformi,
    v2uniformf,
    v2uniformd,
    v3uniformi,
    v3uniformf,
    v3uniformd,
    v4uniformi,
    v4uniformf,
    v4uniformd,
    m44uniformf,
    m44uniformd
  ) where

import Linear.Vector(V2(..), V3(..), V4(..))
import Linear.Matrix(M44(..))
import Graphics.GL.Core33( GLint(..)
                         , GLsizei(..)
                         , GLboolean(..)
                         , GLfloat(..)
                         , GLdouble(..)
                         , glUniform2i
                         , glUniform2f
                         , glUniform3i
                         , glUniform3f
                         , glUniform4i
                         , glUniform4f
                         , glUniformMatrix4fv )
import Graphics.GL.ARB.GPUShaderFP64( glUniform2d
                                    , glUniform3d
                                    , glUniform4d
                                    , glUniformMatrix4dv )
import Graphics.Rendering.OpenGL.GL.Shaders.Uniform(UniformLocation(..))
import Foreign.Ptr(Ptr, castPtr)
import Foreign.Marshal.Alloc(allocaBytes)
import Foreign.Storable

uniform2 :: (GLint -> a -> a -> IO ()) -> UniformLocation -> V2 a -> IO ()
uniform2 uniform (UniformLocation l) (V2 x y) = uniform l x y

v2uniformi = uniform2 glUniform2i
v2uniformf = uniform2 glUniform2f
v2uniformd = uniform2 glUniform2d

uniform3 :: (GLint -> a -> a -> a -> IO ()) -> UniformLocation -> V3 a -> IO ()
uniform3 uniform (UniformLocation l) (V3 x y z) = uniform l x y z

v3uniformi = uniform3 glUniform3i
v3uniformf = uniform3 glUniform3f
v3uniformd = uniform3 glUniform3d

uniform4 :: (GLint -> a -> a -> a -> a -> IO ()) -> UniformLocation -> V4 a -> IO ()
uniform4 uniform (UniformLocation l) (V4 x y z w) = uniform l x y z w

v4uniformi = uniform4 glUniform4i
v4uniformf = uniform4 glUniform4f
v4uniformd = uniform4 glUniform4d

uniform44 :: Storable a => (GLint -> GLsizei -> GLboolean -> Ptr a -> IO ()) -> UniformLocation -> M44 a -> IO ()
uniform44 uniform (UniformLocation l) m = allocaBytes (sizeOf m) $ \ptr -> do poke ptr m
                                                                              uniform l 1 0 (castPtr ptr)
m44uniformf = uniform44 glUniformMatrix4fv
m44uniformd = uniform44 glUniformMatrix4dv
