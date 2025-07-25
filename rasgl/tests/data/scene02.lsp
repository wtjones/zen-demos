(scene :name "poly"
  (models
    (model :name "tri" :path "./assets/models/tri.obj"))
  (objects
    (object
      :model_name "tri"
      :position (vec 0.0 0.0 -2.5)
      :orientation (vec 0.0 0.5 0.0)
      :animation (rotation :axis (vec 0.0 1.0 0.0) :speed 0.5)))
  (cameras
    (camera
      :name "main"
      :position (vec 0.0 0.0 2.5)
      :angle 180
      :fov 45.0
      :near 0.1
      :far 100.0
      :aspect 1.0
      :projection perspective)))
