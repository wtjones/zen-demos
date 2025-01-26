(in-package #:maze)

(defparameter *cell-type* '(empty wall))
(defparameter *direction* '(up down left right))

(defstruct board
  (width 0 :type integer)
  (height 0 :type integer)
  (cells nil :type array))

(defun is-pillar (row col)
  (and (oddp row) (oddp col)))

(defun initial-num-pillars (width height)
  (* (/ (- width 1) 2)
     (/ (- height 1) 2)))

(defun initial-pillars (width height)
  (let ((pillars ()))
    (loop for row from 1 to (- height 2) by 2 do
            (loop for col from 1 to (- width 2) by 2 do
                    (setf pillars (cons (list row col) pillars))))
    pillars))

(defun in-bounds (board row col)
  (let ((dimensions (array-dimensions (board-cells board))))
    (and (< row (first dimensions))
         (< col (second dimensions))
         (>= row 0)
         (>= col 0))))

; Create list of all pillars
; Randomize list 

; Get a random pillar
; pick a direction
; remove each pillar encountered


(defun swap-elements (items index1 index2)
  (let ((temp (nth index1 items)))
    (setf (nth index1 items) (nth index2 items))
    (setf (nth index2 items) temp)))

(defun randomize (items)
  (dotimes (i (length items))
    (swap-elements
      items
      (random (length items))
      (random (length items)))))


; FIXME: not working because can't reassign the reference
(defun random-pop (items)
  (let* ((pos (random (length items)))
         (result (nth pos items)))
    (setf items
      (remove-if
          (lambda (item) (equal item result))
          items))
    result))

(defun random-direction ()
  (nth (random (length *direction*)) *direction*))

(defun generate-maze (width height)
  (format t "Gen maze of sizex ~a~%" width)
  (let ((board (make-board
                 :cells (make-array
                            (list width height)
                          :element-type *cell-type*
                          :initial-element 'empty)))
        (pillars (initial-pillars width height)))

    (alexandria:shuffle pillars)
    (format t "pillars: ~a~%" pillars)

    (loop while (> (length pillars) 0) do
            (let* ((pillar (pop pillars))
                   (direction (random-direction))
                   (row (first pillar))
                   (col (second pillar)))
              (format t "dir: ~a~%" direction)
              (format t "pillar: ~a~%" pillar)

              ; Pillar becomes a wall
              (setf
                (aref (board-cells board) row col)
                'wall)

              ; do
              ; set pos = next
              ; if oob: break
              ; if wall: break
              ; if pillar: pop pillar
              ; set wall

              (loop while t
                    do (format t "loop! ~a ~a~%" row col)

                      (setq row
                          (+ row
                             (cond
                              ((eq direction 'up) -1)
                              ((eq direction 'down) 1)
                              (t 0))))
                      (setq col
                          (+ col
                             (cond
                              ((eq direction 'left) -1)
                              ((eq direction 'right) 1)
                              (t 0))))
                      (format t "loop after! ~a ~a~%" row col)

                      (when (not (in-bounds board row col)) (return))
                      (when (is-pillar row col)
                            (format t "Removing pillar: before:~a~%" pillars)

                            (setq pillars
                                (remove-if
                                    (lambda (item) (equal (list row col) item))
                                    pillars))
                            (format t "Removing pillar: after:~a~%" pillars))

                      (setf
                        (aref (board-cells board) row col)
                        'wall))

              (setf)))
    board))
