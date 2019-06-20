module Main where

import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL hiding(get, perspective, translate)
import Shader
import Vertices
import Camera

import Prelude hiding (init)
import System.Exit(exitSuccess)
import Control.Monad (guard)
import Data.Foldable (foldrM)

import Linear.Transform(perspective, translate, rotateY, degToRad)
import Linear.Matrix(identity, mult)
import Linear.Vector(V3(..), V4(..), vadd)
import Linear.Storable(m44uniformf)
import Linear.Transposable

import Texture.Classes
import Texture.DDS
import Texture.Utils

import Data.IORef

data AppState = AppState { prevMouse :: (Float, Float)
                         , deltaMouse :: (Float, Float)
                         , elapsedTime :: Float
                         , deltaTime :: Float
                         , camera :: Camera Float }

hints :: [WindowHint]
hints = [ WindowHint'DoubleBuffer True
        , WindowHint'ContextVersionMajor 3
        , WindowHint'ContextVersionMinor 3
        , WindowHint'OpenGLProfile OpenGLProfile'Core
        , WindowHint'Samples $ Just 4
        , WindowHint'Resizable False ]

shutdown :: Window -> IO ()
shutdown win = do
    destroyWindow win
    terminate
    exitSuccess
    return ()

vertices :: [Float]
vertices = [ -- Back side
             -0.5,  0.5, -0.5, 0, 0
           ,  0.5,  0.5, -0.5, 0, 1
           ,  0.5, -0.5, -0.5, 1, 1
           ,  0.5, -0.5, -0.5, 1, 1
           , -0.5, -0.5, -0.5, 1, 0
           , -0.5,  0.5, -0.5, 0, 0
             -- Front side
           , -0.5,  0.5,  0.5, 0, 0
           ,  0.5,  0.5,  0.5, 0, 1
           ,  0.5, -0.5,  0.5, 1, 1
           ,  0.5, -0.5,  0.5, 1, 1
           , -0.5, -0.5,  0.5, 1, 0
           , -0.5,  0.5,  0.5, 0, 0
             -- Left side
           , -0.5,  0.5, -0.5, 0, 0 
           , -0.5,  0.5,  0.5, 0, 1
           , -0.5, -0.5,  0.5, 1, 1
           , -0.5, -0.5,  0.5, 1, 1
           , -0.5, -0.5, -0.5, 1, 0
           , -0.5,  0.5, -0.5, 0, 0
             -- Right side
           ,  0.5,  0.5,  0.5, 0, 0
           ,  0.5,  0.5, -0.5, 0, 1
           ,  0.5, -0.5, -0.5, 1, 1
           ,  0.5, -0.5, -0.5, 1, 1
           ,  0.5, -0.5,  0.5, 1, 0
           ,  0.5,  0.5,  0.5, 0, 0
             -- Upper side
           ,  0.5,  0.5,  0.5, 0, 0
           , -0.5,  0.5,  0.5, 0, 1
           , -0.5,  0.5, -0.5, 1, 1
           , -0.5,  0.5, -0.5, 1, 1
           ,  0.5,  0.5, -0.5, 1, 0
           ,  0.5,  0.5,  0.5, 0, 0
             -- Down side
           , -0.5, -0.5,  0.5, 0, 0
           ,  0.5, -0.5,  0.5, 0, 1
           ,  0.5, -0.5, -0.5, 1, 1
           ,  0.5, -0.5, -0.5, 1, 1
           , -0.5, -0.5, -0.5, 1, 0
           , -0.5, -0.5,  0.5, 0, 0 ]

main :: IO ()
main = do
    setErrorCallback $ Just $ \e m -> putStrLn (show e ++ ": " ++ m)
    b <- init
    guard b

    mapM_ windowHint hints

    window <- createWindow 500 500 "k means" Nothing Nothing
    flip (maybe $ return ()) window $ \win -> do
        setWindowCloseCallback win $ Just shutdown
        makeContextCurrent $ Just win
        depthFunc $= Just Less
        clearColor $= Color4 0 0 0 1
        viewport $= (Position 0 0, Size 500 500)

        setCursorPos win 250 250
        initState <- newIORef $ AppState (250,250) (0,0) 0 0 (Camera (V3 0 0 0) (V3 0 1 0) (V3 0 0 0) (V3 0 0 0) 90 0 2 100)

        program <- loadShaders [(VertexShader, "Shaders/vertex.glsl"), (FragmentShader, "Shaders/fragment.glsl")]
        vao <- createVao vertices [3,2] Float 5

        uforms <- mapM (uniformLocation program) ["model", "view", "proj"]

        tex <- loadFromFileTexture2D DDS Texture2D "Textures/troll.dds"
        withTexture Texture2D tex $ textureFilter Texture2D $= ((Linear', Nothing), Linear')

        loop win program vao uforms tex initState

loop :: Window -> Program -> VertexArrayObject -> [UniformLocation] -> TextureObject -> IORef AppState -> IO ()
loop win program vao u@[um, uv, up] tex st = do
    updateTime st
    updateMouse win st
    moveCamera win st
    rotateCamera st

    app <- readIORef st

    clear [ColorBuffer, DepthBuffer]

    textureBinding Texture2D $= Just tex
    bindVertexArrayObject $= Just vao
    currentProgram $= Just program

    let model = rotateY (degToRad 50) `mult` translate (V3 0 0 (-5))
        view = toLookAt $ camera app
        proj = perspective (degToRad 70) 1.0 0.1 100.0

    m44uniformf um model
    m44uniformf uv view
    m44uniformf up proj

    drawArrays Triangles 0 (fromIntegral $ length vertices)

    currentProgram $= Nothing
    bindVertexArrayObject $= Nothing
    textureBinding Texture2D $= Nothing

    swapBuffers win
    pollEvents
    loop win program vao u tex st

updateTime :: IORef AppState -> IO ()
updateTime st = do
  Just curTime <- fmap (fmap realToFrac) getTime
  modifyIORef st $ \app -> app { deltaTime = curTime - elapsedTime app, elapsedTime = curTime }

updateMouse :: Window -> IORef AppState -> IO ()
updateMouse win st = do
  app <- readIORef st
  (cx, cy) <- fmap (\(x,y) -> (realToFrac x, realToFrac y)) $ getCursorPos win
  let (px, py) = prevMouse app
  writeIORef st $ app { deltaMouse = (cx-px, cy-py), prevMouse = (cx, cy) }

moveCamera :: Window -> IORef AppState -> IO ()
moveCamera win st = do
  app <- readIORef st
  let time = deltaTime app
  dv <- foldrM (\x acc -> fmap (acc.) $ x (return id) win) id
          [ keyPressed Key'W (return $ forward time)
          , keyPressed Key'A (return $ left time)
          , keyPressed Key'S (return $ backward time)
          , keyPressed Key'D (return $ right time) ]
  writeIORef st $ app { camera = dv $ camera app }

rotateCamera :: IORef AppState -> IO ()
rotateCamera st = do
  app <- readIORef st
  let (dx, dy) = deltaMouse app
      time = deltaTime app
  writeIORef st $ app { camera = calcVectors . addYaw time dx . addPitch time dy $ camera app }

keyEvent :: Key -> KeyState -> IO a -> IO a -> Window -> IO a
keyEvent k s t f w = do
  s' <- getKey w k
  if s == s' then t else f

keyPressed :: Key -> IO a -> IO a -> Window -> IO a
keyPressed k = keyEvent k KeyState'Pressed 
