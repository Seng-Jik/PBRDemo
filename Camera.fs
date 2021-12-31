namespace global

open OpenTK.Mathematics


type Camera = 
    { mutable Position: Vector3
      mutable Up: Vector3
      mutable Zoom: float32
      mutable Aspect: float32
      mutable Near: float32
      mutable Far: float32
      
      mutable Yaw: float32
      mutable Pitch: float32 }


module Camera =

    let front camera =
        Vector3 (
            cos(camera.Yaw) * cos(camera.Pitch),
            sin(camera.Pitch),
            sin(camera.Yaw) * cos(camera.Pitch))

    let getViewMatrix camera =
        Matrix4.LookAt(
            camera.Position, 
            camera.Position + front camera, 
            camera.Up)

    let getProjectionMatrix camera =
        Matrix4.CreatePerspectiveFieldOfView(
            camera.Zoom, 
            camera.Aspect, 
            camera.Near,
            camera.Far)

    let create position yaw pitch =
        { Position = position
          Up = Vector3 (0f, 1f, 0f)
          Zoom = 3.1415926f / 4f
          Aspect = 1280f / 720f
          Near = 0.01f
          Far = 100.0f
          
          Yaw = yaw
          Pitch = pitch }