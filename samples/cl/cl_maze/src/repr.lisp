(in-package #:maze)

(defun print-maze (board)
  (let* ((dimensions (array-dimensions (board-cells board)))
         (row-max (- (first dimensions) 1))
         (col-max (- (second dimensions) 1)))

    ; top border
    (loop for col from 0 to (+ col-max 2) by 1 do
            (format t "~a~a" char-border char-border))
    (format t "~%")
    (loop for row from 0 to row-max by 1 do
            ; leading row border
            (format t "~a~a" char-border char-border)

            (loop for col from 0 to col-max by 1 do
                    (let* ((cell (aref (board-cells board) row col))
                           (char (if (equal cell 'wall)
                                     char-wall
                                     char-empty)))
                      (format t "~a~a" char char))
                    ; trailing row border
                    (when (= col col-max)
                          (format t "~a~a~%" char-border char-border))))
    ;bottom border
    (loop for col from 0 to (+ col-max 2) by 1 do
            (format t "~a~a" char-border char-border))
    (format t "~%")))
