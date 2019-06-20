module Linear.Transform (
    translate,
    rotateX,
    rotateY,
    rotateZ,
    ortho,
    perspective,
    lookAt,
    degToRad
  ) where

import Linear.Vector
import Linear.Matrix
import Linear.Transposable

translate :: Num a => V3 a -> M44 a
translate (V3 x y z) = V4 (V4 1 0 0 0)
                          (V4 0 1 0 0)
                          (V4 0 0 1 0)
                          (V4 x y z 1)

rotateX :: Floating a => a -> M44 a
rotateX a = let c = cos a
                s = sin a
             in V4 (V4 1 0 0 0)
                   (V4 0 c s 0)
                   (V4 0 (-s) c 0)
                   (V4 0 0 0 1)

rotateY :: Floating a => a -> M44 a
rotateY a = let c = cos a
                s = sin a
             in V4 (V4 c 0 (-s) 0)
                   (V4 0 1 0 0)
                   (V4 s 0 c 0)
                   (V4 0 0 0 1)

rotateZ :: Floating a => a -> M44 a
rotateZ a = let c = cos a
                s = sin a
             in V4 (V4 c s 0 0)
                   (V4 (-s) c 0 0)
                   (V4 0 0 1 0)
                   (V4 0 0 0 1)

ortho :: Floating a => a -> a -> a -> a -> a -> a -> M44 a
ortho l r b t n f = V4 (V4 (2/(r-l)) 0 0 0)
                       (V4 0 (2/(t-b)) 0 0)
                       (V4 0 0 (2/(f-n)) 0)
                       (V4 ((l+r)/(l-r)) ((b+t)/(b-t)) ((n+f)/(n-f)) 1)

perspective :: Floating a => a -> a -> a -> a -> M44 a
perspective y a n f = let p = 1/tan(y/2)
                       in V4 (V4 (p/a) 0 0 0)
                             (V4 0 p 0 0)
                             (V4 0 0 ((f+n)/(n-f)) (-1))
                             (V4 0 0 (2*n*f/(n-f)) 0)

lookAt :: Floating a => V3 a -> V3 a -> V3 a -> M44 a
lookAt eye target up = let z = normalize $ target `vsub` eye
                           x = normalize $ up `cross` z
                           y = z `cross` x
                        in translate (vneg eye) `mult`
                           (transpose $ V4 (fromV3 x 0)
                                           (fromV3 y 0)
                                           (fromV3 z 0)
                                           (V4 0 0 0 1))

degToRad :: Floating a => a -> a
degToRad a = a*pi/180
