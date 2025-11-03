(scene :name "gmap01"
  (models
    (model :name "tri" :path "./assets/models/tri.obj"))
  (objects
    (object
      :model_name "tri"
      :position (vec 0.0 0.0 -2.5)
      :orientation (vec 0.0 0.0 0.0)))
  (gridmaps
    (gridmap
    :name "gmap01"
    :width 6
    :depth 5
    :cell_size 1.0
    :cells
      ((1 2 3 1 2 1 )
       (2 0 0 0 0 2)
       (3 0 1 0 3 1)
       (1 0 1 0 0 2)
       (1 2 1 3 1 1 ))))
  (cameras
    (camera
      :name "main"
      :position (vec 3.0 0.5 4.5)
      :angle 180
      :fov 45.0
      :near 0.1
      :far 100.0
      :aspect 1.0
      :projection perspective)))
