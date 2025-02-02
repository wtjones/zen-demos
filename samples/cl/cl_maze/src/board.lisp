(in-package #:maze)

(defparameter *cell-type* '(empty wall))
(defparameter *direction* '(up right down left))

(defstruct board
  (cols 0 :type integer)
  (rows 0 :type integer)
  (entrance nil)
  (exit nil)
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
(defun count-walls (board row col)
  (let ((result 0))
    (loop for r from (- row 1) to (+ row 1) do
            (loop for c from (- col 1) to (+ col 1) do
                    (incf result
                          (cond ((not (in-bounds board r c)) 1)
                                ((eq (aref (board-cells board) r c) 'wall) 1)
                                (t 0)))))
    result))


;; Walk clockwise along border from start-cusor to end-cursor to find
;; first cell that is a leaf.
;; Leaf = cell with 7 walls that represents the end of a path.
;; If not found, end-cursor is returned.
(defun generate-goal (board start-cursor end-cursor)
  (let* ((cursor (copy-list start-cursor))
         (steps 0))
    (loop while
            (and
             (/= (count-walls board (nth 0 cursor) (nth 1 cursor)) 7)
             (not (equal cursor end-cursor)))
          do
            (setq cursor (next-cursor-state board cursor))
            (incf steps)
            (format t "cursor :~a is equal to ~a? ~a~%"
              cursor
              end-cursor
              (equal cursor end-cursor))
            ; sanity check
            (assert (< steps 10000)))
    (subseq cursor 0 2)))

;; Place entrance and exit cursors at opposite corners
(defun generate-goals (board)
  (let* ((dimensions (array-dimensions (board-cells board)))
         (top-left '(0 0))
         (bottom-right (list (- (nth 0 dimensions) 1)
                             (- (nth 1 dimensions) 1)))
         (start-cursor '(0 0 right))
         (end-cursor (list (- (nth 0 dimensions) 1)
                           (- (nth 1 dimensions) 1)
                           'left))
         (entrance '())
         (exit '()))
    (format t "gen goal: ~a ~a~%" start-cursor end-cursor)
    (setq entrance (generate-goal
                     board
                     start-cursor
                     end-cursor))
    (format t "Entrance: ~a~%" entrance)

    ; If entrance is bottom-right, start two spaces to the left
    (setq start-cursor
        (if
         (equal entrance bottom-right)
         (list (- (nth 0 dimensions) 1)
               (- (nth 1 dimensions) 3)
               'left)
         (list (- (nth 0 dimensions) 1)
               (- (nth 1 dimensions) 1)
               'left)))

    ; If entrance is top-left, end two spaces below
    (setq end-cursor
        (if
         (equal entrance top-left)
         '(2 0 up)
         '(0 0 right)))

    ; Seek from bottom-right to top-left
    (format t "gen goal2: ~a ~a~%" start-cursor end-cursor)
    (setq exit (generate-goal
                 board
                 start-cursor
                 end-cursor))

    (assert (not (equal entrance exit)))
    (values entrance exit)))

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
                        'wall))))
    (multiple-value-bind (entrance exit) (generate-goals board)
      (setf (board-entrance board) entrance)
      (setf (board-exit board) exit))
    board))
