(defvar library-shelf `(elder-race ancient-world))
(defvar my-shelf `())

(defun print-stock ()
  (format t "Library shelf: ~a~%" library-shelf)
  (format t "My shelf: ~a~%" my-shelf))

(defun checkout-book (name)
  (if (member name library-shelf)
      (progn
       (setf library-shelf (remove name library-shelf :count 1))
       (setf my-shelf (cons name my-shelf))
       (format t "Checked out ~a~%" name))
      (format t "Sorry, ~a is not available~%" name)))


(defun stock-book (name)
  (if (listp name)
      (progn
       (setf library-shelf (append name library-shelf))
       (format t "Books ~a are now stocked~%" name)
       (length name))
      (progn
       (setf library-shelf (cons name library-shelf))
       (format t "Book ~a is now stocked~%" name)
       1)))

(defun return-book (name)
  (if (member name my-shelf)
      (progn
       (setf my-shelf (remove name my-shelf))
       (setf library-shelf (cons name library-shelf))
       (format t "Returned ~a~%" name))
      (format t "Sorry, ~a is not in your shelf~%" name)))

(defun test ()
  (setf library-shelf '())
  (setf my-shelf `())

  ; add a list
  (let ((count-1 (stock-book '(elder-race ancient-world))))
    (format t "Returned: ~a~%" count-1))

  ; add an atom
  (let ((count-1 (stock-book 'the-hobbit)))
    (format t "Returned: ~a~%" count-1))

  ; add a dupe
  (let ((count-1 (stock-book 'the-hobbit)))
    (format t "Returned: ~a~%" count-1))

  (print-stock)

  (checkout-book `money-ball)
  (checkout-book `the-hobbit)
  (checkout-book `ancient-world)
  (print-stock)

  (return-book `the-hobbit)

  (print-stock))