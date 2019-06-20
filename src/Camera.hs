module Camera (
    Camera(..),
    forward,
    backward,
    left,
    right,
    addPitch,
    addYaw,
    calcVectors,
    toLookAt
  ) where

import Linear.Vector
import Linear.Matrix
import Linear.Transform(degToRad, lookAt)

data Camera a = Camera { pos :: V3 a
                       , up :: V3 a
                       , target :: V3 a
                       , side :: V3 a
                       , yaw :: a
                       , pitch :: a
                       , speed :: a
                       , rotSpeed :: a }

forward, backward, left, right :: Floating a => a -> Camera a -> Camera a
forward time cam = cam { pos = pos cam `vsub` ((speed cam * time) `smult` target cam) }
backward time cam = cam { pos = pos cam `vadd` ((speed cam * time) `smult` target cam) }
left time cam = cam { pos = pos cam `vadd` ((speed cam * time) `smult` side cam) }
right time cam = cam { pos = pos cam `vsub` ((speed cam * time) `smult` side cam) }

addYaw :: Floating a => a -> a -> Camera a -> Camera a
addYaw time dy cam = cam { yaw = yaw cam + dy * rotSpeed cam * time }

addPitch :: (Ord a, Floating a) => a -> a -> Camera a -> Camera a
addPitch time dp cam = cam { pitch = cons $ pitch cam + dp * rotSpeed cam * time }
  where cons y = if y > 90 then 90 else if y < (-90) then (-90) else y

calcVectors :: Floating a => Camera a -> Camera a
calcVectors cam = cam { target = tg, side = sd }
  where tg = normalize $ V3 (cos pitchR * cos yawR)
                            (sin pitchR)
                            (cos pitchR * sin yawR)
        sd = normalize $ tg `cross` up cam
        yawR = degToRad $ yaw cam
        pitchR = degToRad $ pitch cam

toLookAt :: Floating a => Camera a -> M44 a
toLookAt cam = lookAt (pos cam) (pos cam `vadd` target cam) (up cam)
