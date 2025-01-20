(in-package #:maze)

(defvar *window-x*)
(defvar *window-y*)

(defun command-maze-tui (&rest args)
  (format t "Command: TUI with args: ~a~%" args)
  (with-open-file (*log-file* "/tmp/cl_maze.log" :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
    (format *log-file* "Log start...~%")
    (charms:with-curses ()
      (charms:disable-echoing)
      (charms:enable-raw-input)
      (multiple-value-bind (width height)
          (charms:window-dimensions charms:*standard-window*)
        (setf *window-x* width *window-y* height))
      (format *log-file* "X: ~a, Y: ~a~%" *window-x* *window-y*)
      (loop named hello-world
          with window =
            (charms:make-window 50 15 10 10)
          with window-x = 0
          with window-y = 0
            ;(charms:make-window *window-x* *window-y* 0 0)
          do (progn
              (multiple-value-bind (width height)
                  (charms:window-dimensions window)
                (setf window-x width window-y height))
              (charms:clear-window window)

              (loop for i from 0 to (- window-x 2)
                    do (charms:write-char-at-point
                         window #\# i 3))

              (loop for i from 0 to 10
                    do (charms:write-char-at-point
                         window #\# i 1))

              ;(format *log-file* "~a~%" (charms:window-dimensions charms:*standard-window*))
              ; (charms:write-string-at-point window (charms:window-dimensions charms:*standard-window*) 0 10)
              ;(break "Window dimensions: ~a" (charms:window-dimensions charms:*standard-window*))
              (charms:write-char-at-point
                window #\X 2 2)
              ; (charms:write-string-at-point window "Hello world!" 0 0)
              (charms:refresh-window window)
              ; (charms:refresh-window charms:*standard-window*)

              ;; Process input
              (when (eql (charms:get-char window) #\q)
                    (return-from hello-world))
              (sleep 0.1))))))