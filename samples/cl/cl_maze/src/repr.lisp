(in-package #:maze)

(defun repr-maze (board)
  (with-output-to-string (s)
    (let* ((dimensions (array-dimensions (board-cells board)))
           (row-max (- (first dimensions) 1))
           (col-max (- (second dimensions) 1)))

      ; top border
      ; leading row border
      (format s "~a~a" char-border char-border)

      (loop for col from 0 to col-max by 1 do
              (format t "comparing ~a ~a ~%" (list 0 col) (board-entrance board))
              (cond
               ((equal (list 0 col) (board-entrance board))
                 (format s "~a~a" char-entrance char-entrance))
               ((equal (list 0 col) (board-exit board))
                 (format s "~a~a" char-empty char-empty))
               (t (format s "~a~a" char-border char-border))))

      ; trailing row border

      (format s "~a~a~%" char-border char-border)

      (format s "~%")
      (loop for row from 0 to row-max by 1 do
              ; leading row border
              ; favor extrances on top/bottom, so skip
              ; first and last rows in inner loop
              (if (and (> row 0) (< row row-max))
                  (cond
                   ((equal (list row 0) (board-entrance board))
                     ;(format s "~a~a" char-entrance char-entrance)
                     (format s "~a~a" #\% #\%))
                   ((equal (list row 0) (board-exit board))
                     (format s "~a~a" char-empty char-empty))
                   (t (format s "~a~a" char-border char-border)))
                  (format s "~a~a" char-border char-border))

              (loop for col from 0 to col-max by 1 do
                      (let* ((cell (aref (board-cells board) row col))
                             (char (if (equal cell 'wall)
                                       char-wall
                                       char-empty)))
                        (format s "~a~a" char char)))

              ; trailing row border
              ; favor extrances on top/bottom, so skip 
              ; first and last rows in inner loop 
              (if (and (> row 0) (< row row-max))
                  (cond
                   ((equal (list row col-max) (board-entrance board))
                     (format s "~a~a" char-entrance char-entrance))
                   ((equal (list row col-max) (board-exit board))
                     (format s "~a~a" char-empty char-empty))
                   (t (format s "~a~a" char-border char-border)))

                  (format s "~a~a" char-border char-border))
              (format s "~%"))

      ; bottom border
      (format s "~a~a" char-border char-border)
      (loop for col from 0 to col-max by 1 do
              (cond
               ((equal (list row-max col) (board-entrance board))
                 (format s "~a~a" char-entrance char-entrance))
               ((equal (list row-max col) (board-exit board))
                 (format s "~a~a" char-empty char-empty))
               (t (format s "~a~a" char-border char-border))))

      (format s "~a~a" char-border char-border)
      (format s "~%"))))

(defun print-maze (board)
  (format t "~a" (repr-maze board)))
