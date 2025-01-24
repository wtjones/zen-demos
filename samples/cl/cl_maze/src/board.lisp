(in-package #:maze)

(defparameter *cell-type* '(empty wall))
(defparameter *direction* '(up down left right))

(defstruct board
  (width 0 :type integer)
  (height 0 :type integer)
  (cells nil :type array))

(defun is-pillar (row col)
  (and (evenp row) (evenp col)))

(defun initial-num-pillars (width height)
  (* (+ (/ (- width 1) 2) 1)
     (+ (/ (- height 1) 2) 1)))

(defun initial-pillars (width height)
  (let ((pillars ()))
    (loop for row from 0 to (- height 1) by 2 do
            (loop for col from 0 to (- width 1) by 2 do
                    (setf pillars (cons (list row col) pillars))))
    pillars))


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
                          :initial-element 'empty)))
        (pillars (initial-pillars width height)))

    (alexandria:shuffle pillars)
    (format t "pillars: ~a~%" pillars)

    (loop while (> (length pillars) 0) do
            (let ((pillar (pop pillars))
                  (direction (random-direction)))
              (format t "dir: ~a~%" direction)
              (format t "pillar: ~a~%" pillar)
              ; HERE: loop from pillar to first wall
                      ))
    board))