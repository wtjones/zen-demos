(scene :name "quad"
  (models
    (model :name "quad" :path "./assets/models/quad.obj"))
(objects
    ;; bottom-left
    (object
      :model_name "quad"
      :position (vec 0.0 0.0 0.0)
      :orientation (vec 0.0 0.0 0.0))
    ;; bottom-right
    (object
      :model_name "quad"
      :position (vec 312.0 0.0 0.0)
      :orientation (vec 0.0 0.0 0.0))
    ;; top-right
    (object
      :model_name "quad"
      :position (vec 312.0 232.0 0.0)
      :orientation (vec 0.0 0.0 0.0))
    ;; top-left
    (object
      :model_name "quad"
      :position (vec 0.0 232.0 0.0)
      :orientation (vec 0.0 0.0 0.0))
    ;; middle
    (object
      :model_name "quad"
      :position (vec 160.0 120.0 0.0)
      :orientation (vec 0.0 0.0 0.0))
    (object
      :model_name "quad"
      :position (vec 170.0 120.0 0.0)
      :orientation (vec 0.0 0.0 0.0)))
  (cameras
    (camera
      :name "main"
      :position (vec 0.0 0.0 1.0)
      :angle 180
      :fov 45.0
      :near 0.1
      :far 100.0
      :aspect 1.0
      :projection perspective)))
