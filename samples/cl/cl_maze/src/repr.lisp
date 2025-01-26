(in-package #:maze)

(defun repr-maze (board)
  (with-output-to-string (s)
    (let* ((dimensions (array-dimensions (board-cells board)))
           (row-max (- (first dimensions) 1))
           (col-max (- (second dimensions) 1)))

      ; top border
      (loop for col from 0 to (+ col-max 2) by 1 do
              (format s "~a~a" char-border char-border))
      (format s "~%")
      (loop for row from 0 to row-max by 1 do
              ; leading row border
              (format s "~a~a" char-border char-border)

              (loop for col from 0 to col-max by 1 do
                      (let* ((cell (aref (board-cells board) row col))
                             (char (if (equal cell 'wall)
                                       char-wall
                                       char-empty)))
                        (format s "~a~a" char char))
                      ; trailing row border
                      (when (= col col-max)
                            (format s "~a~a~%" char-border char-border))))
      ;bottom border
      (loop for col from 0 to (+ col-max 2) by 1 do
              (format s "~a~a" char-border char-border))
      (format s "~%"))))

(defun print-maze (board)
  (format t "~a" (repr-maze board)))
