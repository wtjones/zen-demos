(in-package #:maze)

(defun setup-logging ()
  (log4cl-extras/config:setup
    '(:level :debug
             :appenders ((this-console :layout :plain
                                       :filter :warn)
                         (daily :layout :plain
                                :name-format "/tmp/cl_maze.log")))))
