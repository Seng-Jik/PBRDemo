open OpenTK
open OpenTK.Windowing
open OpenTK.Mathematics
open OpenTK.Graphics.OpenGL
open OpenTK.Graphics
open System


let windowSize = Vector2i (1280, 720)


let window = 
    let config = Desktop.NativeWindowSettings ()
    config.Size <- windowSize
    config.Title <- "PBR Demo"
    config.NumberOfSamples <- 4
    new Desktop.GameWindow (Desktop.GameWindowSettings.Default, config)


GL.ClearColor (Color4 (255uy, 0uy, 0uy, 0uy))
GL.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
window.Context.SwapBuffers ()


GL.ClearColor (Color4 (0uy, 0uy, 255uy, 0uy))
GL.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)
window.Context.SwapBuffers ()


let defTexOp = fun _ ->
    GL.TexParameter(
        TextureTarget.Texture2D, 
        LanguagePrimitives.EnumOfValue 34046,
        16)

    GL.GenerateMipmap GenerateMipmapTarget.Texture2D

    GL.TexParameter (
        TextureTarget.Texture2D, 
        TextureParameterName.TextureWrapS, 
        int TextureWrapMode.Repeat)
    GL.TexParameter (
        TextureTarget.Texture2D, 
        TextureParameterName.TextureWrapT, 
        int TextureWrapMode.Repeat)
    GL.TexParameter (
        TextureTarget.Texture2D, 
        TextureParameterName.TextureMinFilter, 
        int TextureMinFilter.LinearMipmapLinear)
    GL.TexParameter (
        TextureTarget.Texture2D, 
        TextureParameterName.TextureMagFilter, 
        int TextureMagFilter.Linear)
    

let loadTexture' texOp path =
    use img = IO.File.Open (path, IO.FileMode.Open)
    let image = StbImageSharp.ImageResult.FromStream (img, StbImageSharp.ColorComponents.RedGreenBlueAlpha)

    let tex = GL.GenTexture ()
    GL.BindTexture (TextureTarget.Texture2D, tex)
    GL.TexImage2D (
        TextureTarget.Texture2D, 
        0, 
        PixelInternalFormat.Rgba, 
        image.Width,
        image.Height, 
        0, 
        PixelFormat.Rgba, 
        PixelType.UnsignedByte, 
        image.Data)

    texOp tex

    GL.BindTexture (TextureTarget.Texture2D, 0)

    tex


let loadTexture = loadTexture' defTexOp


let createShaderProgram vsFileName gsFileName fsFileName =
    let buildShader shaderPath shaderType =
        let s = GL.CreateShader (shaderType)
        GL.ShaderSource (s, IO.File.ReadAllText shaderPath)
        GL.CompileShader (s)
        let mutable status = 0
        GL.GetShader(s, ShaderParameter.CompileStatus, &status)
        if status = 0 then raise <| exn(GL.GetShaderInfoLog(s))
        s

    let vs = buildShader vsFileName ShaderType.VertexShader
    let fs = buildShader fsFileName ShaderType.FragmentShader
    let gs = gsFileName |> Option.map (fun x -> buildShader x ShaderType.GeometryShader)
    let program = GL.CreateProgram ()
    GL.AttachShader (program, vs)
    GL.AttachShader (program, fs)
    Option.iter (fun gs -> GL.AttachShader (program, gs)) gs
    GL.LinkProgram program
    GL.DeleteShader fs
    GL.DeleteShader vs
    let mutable status = 0
    GL.GetProgram(program, GetProgramParameterName.LinkStatus, &status)
    if status = 0 then raise <| exn(GL.GetProgramInfoLog program)
    program


let vboFormatPosNormTexcoord = fun _ ->
    GL.EnableVertexAttribArray 0
    GL.VertexAttribPointer (
        0, 3, VertexAttribPointerType.Float, 
        false, 11 * sizeof<float32>, 0)
    
    GL.EnableVertexAttribArray 1
    GL.VertexAttribPointer (
        1, 3, VertexAttribPointerType.Float,
        false, 11 * sizeof<float32>, 3 * sizeof<float32>)
        
    GL.EnableVertexAttribArray 2
    GL.VertexAttribPointer (
        2, 2, VertexAttribPointerType.Float,
        false, 11 * sizeof<float32>, 6 * sizeof<float32>)

    GL.EnableVertexAttribArray 3
    GL.VertexAttribPointer (
        3, 3, VertexAttribPointerType.Float,
        false, 11 * sizeof<float32>, 8 * sizeof<float32>)


let createVAO moreOperations (buffer: float32[]) =
    let vao = GL.GenVertexArray()
    GL.BindVertexArray (vao)
    let vbo = GL.GenBuffer ()
    GL.BindBuffer (BufferTarget.ArrayBuffer, vbo)
    GL.BufferData (
        BufferTarget.ArrayBuffer, 
        sizeof<float32> * Array.length buffer, 
        buffer, 
        BufferUsageHint.StaticDraw)
    moreOperations vao
    GL.BindVertexArray 0
    vao


let makeTangents: float32[] -> _ =
    Seq.chunkBySize 8
    >> Seq.chunkBySize 3
    >> Seq.collect (fun x ->
        let pos = x |> Array.map (fun v -> Vector3 (v[0], v[1], v[2]))
        let normal = x |> Array.map (fun v -> Vector3(v[3], v[4], v[5])) |> Array.head
        let uv = x |> Array.map (fun v -> Vector2 (v[6], v[7]))

        let edge1 = pos[1] - pos[0]
        let edge2 = pos[2] - pos[0]
        let deltaUV1 = uv[1] - uv[0]
        let deltaUV2 = uv[2] - uv[0]

        let f = 1.0f / (deltaUV1.X * deltaUV2.Y - deltaUV2.X * deltaUV1.Y)
        let tangent =
            Vector3 (
                f * (deltaUV2.Y * edge1.X - deltaUV1.Y * edge2.X),
                f * (deltaUV2.Y * edge1.Y - deltaUV1.Y * edge2.Y),
                f * (deltaUV2.Y * edge1.Z - deltaUV1.Y * edge2.Z))
            |> Vector3.Normalize
        
        x |> Seq.collect (fun v -> 
            Seq.append v [| tangent.X; tangent.Y; tangent.Z |]))
    >> Array.ofSeq


let screenVAO =
    [|
        -1.0f;  1.0f;  0.0f; 1.0f;
        -1.0f; -1.0f;  0.0f; 0.0f;
         1.0f; -1.0f;  1.0f; 0.0f;

        -1.0f;  1.0f;  0.0f; 1.0f;
         1.0f; -1.0f;  1.0f; 0.0f;
         1.0f;  1.0f;  1.0f; 1.0f
    |]
    |> createVAO (fun _ ->
        GL.EnableVertexAttribArray 0
        GL.VertexAttribPointer (
            0, 2, VertexAttribPointerType.Float, 
            false, 4 * sizeof<float32>, 0)
        
        GL.EnableVertexAttribArray 1
        GL.VertexAttribPointer (
            1, 2, VertexAttribPointerType.Float,
            false, 4 * sizeof<float32>, 2 * sizeof<float32>))


let boxVAO = 
    createVAO vboFormatPosNormTexcoord <| makeTangents [|
        // Back face
        -0.5f; -0.5f; -0.5f; 0.0f; 0.0f; -1.0f; 0.0f; 0.0f; // Bottom-left
        0.5f; 0.5f; -0.5f; 0.0f; 0.0f; -1.0f; 1.0f; 1.0f; // top-right
        0.5f; -0.5f; -0.5f; 0.0f; 0.0f; -1.0f; 1.0f; 0.0f; // bottom-right         
        0.5f; 0.5f; -0.5f; 0.0f; 0.0f; -1.0f; 1.0f; 1.0f;  // top-right
        -0.5f; -0.5f; -0.5f; 0.0f; 0.0f; -1.0f; 0.0f; 0.0f;  // bottom-left
        -0.5f; 0.5f; -0.5f; 0.0f; 0.0f; -1.0f; 0.0f; 1.0f;// top-left
        // Front face
        -0.5f; -0.5f; 0.5f; 0.0f; 0.0f; 1.0f; 0.0f; 0.0f; // bottom-left
        0.5f; -0.5f; 0.5f; 0.0f; 0.0f; 1.0f; 1.0f; 0.0f;  // bottom-right
        0.5f; 0.5f; 0.5f; 0.0f; 0.0f; 1.0f; 1.0f; 1.0f;  // top-right
        0.5f; 0.5f; 0.5f; 0.0f; 0.0f; 1.0f; 1.0f; 1.0f; // top-right
        -0.5f; 0.5f; 0.5f; 0.0f; 0.0f; 1.0f; 0.0f; 1.0f;  // top-left
        -0.5f; -0.5f; 0.5f; 0.0f; 0.0f; 1.0f; 0.0f; 0.0f;  // bottom-left
        // Left face
        -0.5f; 0.5f; 0.5f; -1.0f; 0.0f; 0.0f; 1.0f; 0.0f; // top-right
        -0.5f; 0.5f; -0.5f; -1.0f; 0.0f; 0.0f; 1.0f; 1.0f; // top-left
        -0.5f; -0.5f; -0.5f; -1.0f; 0.0f; 0.0f; 0.0f; 1.0f;  // bottom-left
        -0.5f; -0.5f; -0.5f; -1.0f; 0.0f; 0.0f; 0.0f; 1.0f; // bottom-left
        -0.5f; -0.5f; 0.5f; -1.0f; 0.0f; 0.0f; 0.0f; 0.0f;  // bottom-right
        -0.5f; 0.5f; 0.5f; -1.0f; 0.0f; 0.0f; 1.0f; 0.0f; // top-right
        // Right face
        0.5f; 0.5f; 0.5f; 1.0f; 0.0f; 0.0f; 1.0f; 0.0f; // top-left
        0.5f; -0.5f; -0.5f; 1.0f; 0.0f; 0.0f; 0.0f; 1.0f; // bottom-right
        0.5f; 0.5f; -0.5f; 1.0f; 0.0f; 0.0f; 1.0f; 1.0f; // top-right         
        0.5f; -0.5f; -0.5f; 1.0f; 0.0f; 0.0f; 0.0f; 1.0f;  // bottom-right
        0.5f; 0.5f; 0.5f; 1.0f; 0.0f; 0.0f; 1.0f; 0.0f;  // top-left
        0.5f; -0.5f; 0.5f; 1.0f; 0.0f; 0.0f; 0.0f; 0.0f; // bottom-left     
        // Bottom face
        -0.5f; -0.5f; -0.5f; 0.0f; -1.0f; 0.0f; 0.0f; 1.0f; // top-right
        0.5f; -0.5f; -0.5f; 0.0f; -1.0f; 0.0f; 1.0f; 1.0f; // top-left
        0.5f; -0.5f; 0.5f; 0.0f; -1.0f; 0.0f; 1.0f; 0.0f;// bottom-left
        0.5f; -0.5f; 0.5f; 0.0f; -1.0f; 0.0f; 1.0f; 0.0f; // bottom-left
        -0.5f; -0.5f; 0.5f; 0.0f; -1.0f; 0.0f; 0.0f; 0.0f; // bottom-right
        -0.5f; -0.5f; -0.5f; 0.0f; -1.0f; 0.0f; 0.0f; 1.0f; // top-right
        // Top face
        -0.5f; 0.5f; -0.5f; 0.0f; 1.0f; 0.0f; 0.0f; 1.0f;// top-left
        0.5f; 0.5f; 0.5f; 0.0f; 1.0f; 0.0f; 1.0f; 0.0f; // bottom-right
        0.5f; 0.5f; -0.5f; 0.0f; 1.0f; 0.0f; 1.0f; 1.0f; // top-right     
        0.5f; 0.5f; 0.5f; 0.0f; 1.0f; 0.0f; 1.0f; 0.0f; // bottom-right
        -0.5f; 0.5f; -0.5f; 0.0f; 1.0f; 0.0f; 0.0f; 1.0f;// top-left
        -0.5f; 0.5f; 0.5f; 0.0f; 1.0f; 0.0f; 0.0f; 0.0f // bottom-left      
    |]
    

let mutable camera = 
    Camera.create (Vector3 (0f, 1f, 4f)) (-3.1415926f / 2.0F) <| 0.0f


let setUniformMat4 (prog: int) name (mat: Matrix4) =
    let mutable mat = mat
    GL.UniformMatrix4 (GL.GetUniformLocation (prog, name), false, &mat)


let setUniform3f (prog: int) name (v: Vector3) =
    let mutable v = v
    GL.Uniform3 (GL.GetUniformLocation (prog, name), &v)


let setUniform1i (prog: int) name (v: int) =
    GL.Uniform1 (GL.GetUniformLocation (prog, name), v)


let setUniform1f (prog: int) name (v: float32) =
    GL.Uniform1 (GL.GetUniformLocation (prog, name), v)


let mutable prevMousePostion = Vector2 ()


let processInput (e: Common.FrameEventArgs) =
    let speed = 4f
    let front = Camera.front camera

    if window.KeyboardState.IsKeyDown(GraphicsLibraryFramework.Keys.W) then
        camera.Position <- camera.Position + speed * float32 e.Time * front
    
    if window.KeyboardState.IsKeyDown(GraphicsLibraryFramework.Keys.S) then
        camera.Position <- camera.Position - speed * float32 e.Time * front

    let right = Vector3.Cross(front, camera.Up)

    if window.KeyboardState.IsKeyDown(GraphicsLibraryFramework.Keys.A) then
        camera.Position <- camera.Position - speed * float32 e.Time * right

    if window.KeyboardState.IsKeyDown(GraphicsLibraryFramework.Keys.D) then
        camera.Position <- camera.Position + speed * float32 e.Time * right

    if window.KeyboardState.IsKeyDown(GraphicsLibraryFramework.Keys.Q) then
        camera.Position <- camera.Position + speed * float32 e.Time * camera.Up

    if window.KeyboardState.IsKeyDown(GraphicsLibraryFramework.Keys.E) then
        camera.Position <- camera.Position - speed * float32 e.Time * camera.Up

    let mouseMovement = window.MousePosition - prevMousePostion

    prevMousePostion <- window.MousePosition

    camera.Yaw <- camera.Yaw + mouseMovement.X * 0.001f
    camera.Pitch <- camera.Pitch - mouseMovement.Y * 0.001f


let loadHDRSkybox path =
    use img = IO.File.Open (path, IO.FileMode.Open)
    let r = StbImageSharp.ImageResultFloat.FromStream (img, StbImageSharp.ColorComponents.RedGreenBlue)
    
    let tex = GL.GenTexture ()
    GL.BindTexture (TextureTarget.Texture2D, tex)
    GL.TexImage2D (TextureTarget.Texture2D, 0, PixelInternalFormat.Rgb16f, r.Width, r.Height, 0, PixelFormat.Rgb, PixelType.Float, r.Data)

    GL.TexParameter (TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
    GL.TexParameter (TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
    GL.TexParameter (TextureTarget.Texture2D, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToBorder)
    GL.TexParameter (TextureTarget.Texture2D, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToBorder)

    GL.BindTexture (TextureTarget.Texture2D, 0)
    tex


let createCubeMap width height =
    let cubeMap = GL.GenTexture ()
    GL.BindTexture (TextureTarget.TextureCubeMap, cubeMap)
    
    for i in 0..5 do
        GL.TexImage2D (
            enum <| int TextureTarget.TextureCubeMapPositiveX + i,
            0, 
            PixelInternalFormat.Rgb16f,
            width, 
            height, 
            0, 
            PixelFormat.Rgb,
            PixelType.Float, 0)

    [ TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge
      TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge
      TextureParameterName.TextureWrapR, int TextureWrapMode.ClampToEdge
      TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear
      TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear ]
    |> List.iter (fun (p, a) -> GL.TexParameter (TextureTarget.TextureCubeMap, p, a))

    GL.BindTexture (TextureTarget.TextureCubeMap, 0)
    cubeMap


let createFramebufferWithColorAttachment colorAttachment =
    let fbo = GL.GenFramebuffer ()
    GL.BindFramebuffer (FramebufferTarget.Framebuffer, fbo)

    GL.FramebufferTexture (
        FramebufferTarget.Framebuffer, 
        FramebufferAttachment.ColorAttachment0, 
        (colorAttachment: int), 
        0)

    GL.BindFramebuffer (FramebufferTarget.Framebuffer, 0)

    fbo
    

let prog = createShaderProgram "vs.glsl" None "fs.glsl"


let cubemapProg = 
    createShaderProgram 
        "directPosition_vs.glsl" 
        (Some "dispatchCubemap_gs.glsl") 


let captureTexToCubemapProg = cubemapProg "captureTexToCubemap_fs.glsl"


let skyboxProg = createShaderProgram "skybox_vs.glsl" None "skybox_fs.glsl"

type Texture = int


type PBRMaterial = 
    { Albedo: int
      AO: int
      Metallic: int
      Normal: int
      Roughness: int }


let loadPBRMaterial name =
    { Albedo = loadTexture $"materials/{name}/albedo.png"
      AO = loadTexture $"materials/{name}/ao.png"
      Metallic = loadTexture $"materials/{name}/metallic.png"
      Normal = loadTexture $"materials/{name}/normal.png"
      Roughness = loadTexture $"materials/{name}/roughness.png" }


let bindPBRMaterial mat =
    [ TextureUnit.Texture0, mat.Albedo 
      TextureUnit.Texture1, mat.AO
      TextureUnit.Texture2, mat.Metallic
      TextureUnit.Texture3, mat.Normal
      TextureUnit.Texture4, mat.Roughness ]
    |> List.iter (fun (texUnit, tex) ->
        GL.ActiveTexture texUnit
        GL.BindTexture (TextureTarget.Texture2D, tex))


let skyboxTex = loadHDRSkybox "skybox.hdr"
let material = 
    [ "grass"
      "gold"
      "plastic"
      "rusted_iron"
      "wall" ]
    |> List.map loadPBRMaterial


let makeCubemap' clear cubemap width height program configuare =
    let cubemapFbo = createFramebufferWithColorAttachment cubemap

    GL.BindFramebuffer (FramebufferTarget.Framebuffer, cubemapFbo)
     
    GL.Viewport (0, 0, width, height)

    if clear then
        GL.Clear ClearBufferMask.ColorBufferBit

    let projection = 
        Matrix4.CreatePerspectiveFieldOfView (3.1415926f / 2f, 1f, 0.1f, 10f)
    
    let viewMatrix =
        [ 1f, 0f, 0f, 0f, -1f, 0f
          -1f, 0f, 0f, 0f, -1f, 0f
          0f, 1f, 0f, 0f, 0f, 1f
          0f, -1f, 0f, 0f, 0f, -1f
          0f, 0f, 1f, 0f, -1f, 0f
          0f, 0f, -1f, 0f, -1f, 0f ]
        |> List.map (fun (vx, vy, vz, ux, uy, uz) -> 
            Matrix4.LookAt (
                Vector3 (),
                Vector3 (vx, vy, vz),
                Vector3 (ux, uy, uz)))

    GL.DepthMask false

    GL.UseProgram (program: int)

    setUniformMat4 program "projection" <| projection

    viewMatrix |> List.iteri (fun faceId ->
        setUniformMat4 program $"views[{faceId}]")

    configuare ()
    GL.BindVertexArray boxVAO
    GL.DrawArrays (PrimitiveType.Triangles, 0, 36)

    GL.UseProgram 0
    GL.DepthMask true

    GL.BindFramebuffer (FramebufferTarget.Framebuffer, 0)

    GL.DeleteFramebuffer cubemapFbo

    cubemap

let makeCubemap = makeCubemap' true


let skyboxCubemap =
    let cubemap =
        makeCubemap (createCubeMap 512 512) 512 512 captureTexToCubemapProg <|
            fun () ->
                setUniform1i captureTexToCubemapProg "equirectangularMap" 0 
                GL.ActiveTexture TextureUnit.Texture0
                GL.BindTexture (TextureTarget.Texture2D, skyboxTex)

    cubemap


let lambertianCubemapIBL =
    let p = cubemapProg "precompute_ibl_lambertian_fs.glsl"
    makeCubemap (createCubeMap 128 128) 128 128 p <|
        fun () ->
            setUniform1i p "environmentMap" 0
            GL.ActiveTexture TextureUnit.Texture0
            GL.BindTexture (TextureTarget.TextureCubeMap, skyboxCubemap)


let specularCubemapIBL =
    let p = cubemapProg "precompute_ibl_specular_fs.glsl"
    let cubemap = createCubeMap 128 128
    GL.BindTexture (TextureTarget.TextureCubeMap, cubemap)
    GL.TexParameter (
        TextureTarget.TextureCubeMap, 
        TextureParameterName.TextureMinFilter, 
        int TextureMinFilter.LinearMipmapLinear)
    GL.TexParameter (
        TextureTarget.TextureCubeMap, 
        TextureParameterName.TextureBaseLevel, 0)
    GL.TexParameter (
        TextureTarget.TextureCubeMap, 
        TextureParameterName.TextureMaxLevel, 4)
    GL.GenerateMipmap GenerateMipmapTarget.TextureCubeMap

    GL.Enable EnableCap.TextureCubeMapSeamless

    let maxMipLevels = 5
    for mip in 0..maxMipLevels - 1 do
        let mipWidth = 128.0 * pown 0.5 mip |> int
        let mipHeight = 128.0 * pown 0.5 mip |> int

        let roughness = float mip / float (maxMipLevels - 1)

        makeCubemap' false cubemap mipWidth mipHeight p <|
            fun () ->
                setUniform1i p "environmentMap" 0
                GL.ActiveTexture TextureUnit.Texture0
                GL.BindTexture (TextureTarget.TextureCubeMap, skyboxCubemap)

                setUniform1f p "roughness" <| float32 roughness

                GL.FramebufferTexture (
                    FramebufferTarget.Framebuffer,
                    FramebufferAttachment.ColorAttachment0,
                    cubemap,
                    mip)

                GL.Clear ClearBufferMask.ColorBufferBit
                    
                printfn "Build Mip: %d" mip
        |> ignore

    GL.Disable EnableCap.TextureCubeMapSeamless

    cubemap


let brdfLut =
    let p =
        createShaderProgram "precompute_brdf_lut_vs.glsl" None "precompute_brdf_lut_fs.glsl"

    let fbo = GL.GenFramebuffer ()
    GL.BindFramebuffer (FramebufferTarget.Framebuffer, fbo)

    GL.Viewport (0, 0, 512, 512)

    let tex = GL.GenTexture ()
    GL.BindTexture (TextureTarget.Texture2D, tex)
    GL.TexImage2D (
        TextureTarget.Texture2D, 
        0, 
        PixelInternalFormat.Rg16f, 
        512, 
        512, 
        0, 
        PixelFormat.Rgb, 
        PixelType.Float, 
        0)

    GL.TexParameter (
        TextureTarget.Texture2D,
        TextureParameterName.TextureMinFilter,
        int TextureMinFilter.Linear)

    GL.TexParameter (
        TextureTarget.Texture2D,
        TextureParameterName.TextureMagFilter,
        int TextureMagFilter.Linear)

    GL.TexParameter (
        TextureTarget.Texture2D,
        TextureParameterName.TextureWrapS,
        int TextureWrapMode.ClampToEdge)

    GL.TexParameter (
        TextureTarget.Texture2D,
        TextureParameterName.TextureWrapT,
        int TextureWrapMode.ClampToEdge)

    GL.FramebufferTexture (
        FramebufferTarget.Framebuffer,
        FramebufferAttachment.ColorAttachment0,
        tex,
        0)

    GL.UseProgram p

    GL.BindVertexArray screenVAO
    GL.DrawArrays (PrimitiveType.Triangles, 0, 6)

    GL.BindFramebuffer (FramebufferTarget.Framebuffer, 0)

    tex
   

let render () =
    GL.ClearColor (Color4 (0uy, 255uy, 0uy, 0uy))
    GL.Clear (ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)

    GL.Enable EnableCap.FramebufferSrgb

    GL.Viewport (0, 0, 1280, 720)

    GL.Disable EnableCap.DepthTest
    
    GL.Enable EnableCap.Multisample

    begin   // Draw Skybox
        GL.DepthMask false
        GL.UseProgram skyboxProg
        setUniformMat4 skyboxProg "view" <| Camera.getViewMatrix camera
        setUniformMat4 skyboxProg "projection" <| Camera.getProjectionMatrix camera
        setUniform1i skyboxProg "environmentMap" 0
        
        GL.ActiveTexture TextureUnit.Texture0
        GL.BindTexture (TextureTarget.TextureCubeMap, skyboxCubemap)

        

        GL.BindVertexArray boxVAO
        GL.DrawArrays (PrimitiveType.Triangles, 0, 36)

        GL.UseProgram 0
    end

    GL.Enable EnableCap.DepthTest

    material |> List.iteri (fun i material ->
        GL.DepthMask true
        GL.UseProgram prog

        setUniformMat4 prog "model" <| Matrix4.CreateTranslation (0f, 0f, 1.25f * float32 i)
        setUniformMat4 prog "view" <| Camera.getViewMatrix camera
        setUniformMat4 prog "projection" <| Camera.getProjectionMatrix camera

        setUniform3f prog "camPos" <| camera.Position

        bindPBRMaterial material

        setUniform1i prog "albedoTex" 0
        setUniform1i prog "aoTex" 1
        setUniform1i prog "metallicTex" 2
        setUniform1i prog "normalTex" 3
        setUniform1i prog "roughnessTex" 4
        setUniform1i prog "irradianceMap" 5
        setUniform1i prog "specularMap" 6
        setUniform1i prog "brdfLut" 7

        GL.ActiveTexture TextureUnit.Texture5
        GL.BindTexture (TextureTarget.TextureCubeMap, lambertianCubemapIBL)

        GL.ActiveTexture TextureUnit.Texture6
        GL.BindTexture (TextureTarget.TextureCubeMap, specularCubemapIBL)

        GL.ActiveTexture TextureUnit.Texture7
        GL.BindTexture (TextureTarget.Texture2D, brdfLut)

        [ Vector3 (-1f, -1f, 1f), Vector3.Zero
          Vector3 (-1f, 1f, 1f), Vector3.Zero
          Vector3 (1f, 1f, 1f), Vector3.Zero
          Vector3 (1f, -1f, 1f), Vector3.Zero
          Vector3 (-1f, -1f, -1f), Vector3.Zero
          Vector3 (-1f, 1f, -1f), Vector3.Zero
          Vector3 (1f, 1f, -1f), Vector3.Zero
          Vector3 (1f, -1f, -1f), Vector3.Zero ]
        |> List.iteri (fun i (pos, col) ->
            setUniform3f prog $"lights[{i}].WorldPosition" pos
            setUniform3f prog $"lights[{i}].Color" col)

        GL.BindVertexArray boxVAO
        GL.DrawArrays (PrimitiveType.Triangles, 0, 36)

        GL.UseProgram 0
    )

    GL.Disable EnableCap.DepthTest
    GL.Disable EnableCap.Multisample
    GL.Disable EnableCap.FramebufferSrgb

window.CursorGrabbed <- true
window.add_RenderFrame (fun e ->
    processInput e
    render ()
    window.Context.SwapBuffers ())
window.Run ()
