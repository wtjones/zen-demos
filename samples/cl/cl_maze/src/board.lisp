(in-package #:maze)

(defparameter *cell-type* '(empty wall))
(defparameter *direction* '(up right down left))

(defstruct board
  (cols 0 :type integer)
  (rows 0 :type integer)
  (cells nil :type array))

(defun is-size-valid (rows cols)
  (and (oddp rows) (oddp cols) (> rows 1) (> cols 1)))

(defun is-pillar (row col)
  (and (oddp row) (oddp col)))

(defun initial-num-pillars (rows cols)
  (* (/ (- cols 1) 2)
     (/ (- rows 1) 2)))

(defun initial-pillars (rows cols)
  (let ((pillars ()))
    (loop for row from 1 to (- rows 2) by 2 do
            (loop for col from 1 to (- cols 2) by 2 do
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

;; Returns (row col) one unit forward in direction 
(defun next-pos (row col direction)
  (format t "next-pos ~a ~a ~A ~S ~A ~S ~a~%"
    row col direction direction 'up 'up (eql direction 'up))
  (list (+ row
           (cond
            ((eql direction 'up) -1)
            ((eql direction 'down) 1)
            (t 0)))
        (+ col
           (cond
            ((eql direction 'left) -1)
            ((eql direction 'right) 1)
            (t 0)))))

(defun next-cursor-state (board cursor)

  ; get next pos
  (let ((new-pos (apply 'next-pos cursor))
        (clockwise-dir
         (nth
           (mod (+ 1 (position (nth 2 cursor) *direction*)) 4)
           *direction*))
        (bounds-pos '()))
    (format t "clockwise: ~a~%" clockwise-dir)
    (assert
        (in-bounds board (nth 0 new-pos) (nth 1 new-pos)))
    ; if wall, get next again
    (setq new-pos
        (if
         (eq
          (aref (board-cells board) (first new-pos) (second new-pos))
          'wall)
         (next-pos (nth 0 new-pos) (nth 1 new-pos) (nth 2 cursor))
         new-pos))
    (setq bounds-pos (next-pos
                       (nth 0 new-pos) (nth 1 new-pos) (nth 2 cursor)))
    ; if next is oob, rotate cursor clockwise
    (list (nth 0 new-pos) (nth 1 new-pos)
          (if
           (in-bounds board (nth 0 bounds-pos) (nth 1 bounds-pos))
           (nth 2 cursor)
           clockwise-dir))))

;; Return count of adjacent walls
(defun num-walls (board row col)
  (let ((result 0))
    (loop for r from (- row 1) to (+ row 1) do
            (loop for c from (- col 1) to (+ col 1) do
                    (incf result
                          (cond ((not (in-bounds board r c)) 1)
                                ((eq (aref (board-cells board) r c) 'wall) 1)
                                (t 0)))))
    result))


(defun generate-maze (rows cols)
  (format t "Gen maze of sizex ~a~%" cols)
  (when (not (is-size-valid rows cols))
        (format *error-output* "Maze dimensions must be odd and >= (3 3).~%")
        (return-from generate-maze))
  (let ((board (make-board
                 :cells (make-array
                            (list rows cols)
                          :element-type *cell-type*
                          :initial-element 'empty)))
        (pillars (initial-pillars rows cols)))

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
                      (when (equal (aref (board-cells board) row col) 'wall)
                            (return))
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
