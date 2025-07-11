(scene :name "map01"
  (models
    (model :name "map01" :path "./assets/maps/map01.obj"))
  (maps
    (map
      :model_name "map01"))
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
