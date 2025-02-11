(in-package #:maze)

(defstruct solution
  (search-steps 0 :type integer)
  (stack nil))

(defstruct solution-step
  (row 0 :type integer)
  (col 0 :type integer)
  (direction))

; Get initial solver direction of maze entrance
(defun goal-direction (board)

  (with-slots (entrance) board))

(defun is-solved (board solution)

  (with-slots (stack search-steps) solution
    (when (null stack) (return-from is-solved nil))
    (with-slots (row col) (nth 0 stack)
      (return-from is-solved (equal (list row col) (board-exit board))))))

(defun iterate-solution (board solution)
  (when (is-solved board solution) (return-from iterate-solution nil))

  (let ((entrance (board-entrance board))
        (exit (board-exit board)))

    (with-slots (stack search-steps) solution

      ; if stack empty:
      ;  row/col = entrance
      ;   next-dir = up
      ; else:
      ;   row/col = step
      ;   next-dir = step-direction + 1

      (let*
          ((is-first-step (null (nth 0 stack)))
           (row
            (if
             is-first-step
             (nth 0 entrance)
             (solution-step-row (nth 0 stack))))
           (col
            (if
             is-first-step
             (nth 1 entrance)
             (solution-step-col (nth 0 stack))))

           (next-dir (if is-first-step 'up
                         (next-direction (solution-step-direction (nth 0 stack))))))
        (format t "so far: row ~a col ~a next-dir ~a~%" row col next-dir)

        (when is-first-step
              (destructuring-bind (row col) entrance
                (setf
                  (aref (board-cells board) row col)
                  'visited)))

        (loop for dir in *direction*
              do

                (format t "Searching ~a ~a dir: ~a cell: ~a~%"
                  row col dir
                  (apply 'get-cell (cons board (next-pos row col dir))))

                ; if path valid:
                ;   push step
                ;   mark visit
                ;   if exit: return t
                ;   else: continue

                (when (eq (apply 'get-cell (cons board (next-pos row col dir))) 'empty)
                      (format t "Found empty~%")

                      (destructuring-bind (row col) (next-pos row col dir)
                        (push (make-solution-step
                                :row row
                                :col col
                                :direction dir)
                              stack)
                        ; Draw on board
                        (setf
                          (aref (board-cells board) row col)
                          'visited)

                        (format t "Comparing ~a with exit ~a~%"
                          (list row col) exit)
                        (when (equal (list row col) exit)
                              (format t "Exit found~%")
                              (return-from iterate-solution t)))

                      (return-from iterate-solution nil)))

        ; Valid stop not found, pop and retreat
        (format t "Retreating: ~a~%" (pop stack))
        (setf
          (aref (board-cells board) row col)
          'retreated))

      (format t "stack: ~a~%" stack)))
  nil)
