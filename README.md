# DIE is not engine
Haskell modern opengl demo

# Structure

<pre>
src
├── Main.hs
├── Camera.hs
├── Shader.hs
├── Vertices.hs
├── Utils.hs
├── Linear
│   ├── Matrix.hs
│   ├── Storable.hs
│   ├── Transform.hs
│   ├── Transposable.hs
│   ├── V2.hs
│   ├── V3.hs
│   ├── V4.hs
│   └── Vector.hs
└── Texture
    ├── Classes.hs
    ├── DDS.hs
    └── Utils.hs
</pre>

# Build and run

```bash
stack init
stack build
stack exec die
```
