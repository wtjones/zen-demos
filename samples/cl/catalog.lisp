(defstruct catalog-book
  (id 0 :type integer)
  title)
(defstruct book-checkout
  ; (:constructor new-checkout
  ;               (patron book &optional (occurred-at 0)))
  patron-id
  book-id
  (occurred-at (get-universal-time) :type integer))

(defvar *catalog-books* nil)
(defvar *catalog-book-checkouts* nil)
(defvar *last-id* 0)
(defvar book-checkouts `())
(defvar library-shelf `(elder-race ancient-world))
(defvar my-shelf `())

(defun get-id ()
  (setf *last-id* (+ 1 *last-id*))
  *last-id*)


(defun print-stock ()
  (format t "Library shelf: ~a~%" library-shelf)
  (format t "My shelf: ~a~%" my-shelf))

(defun print-catalog ()
  (format t "Catalog: ~a~%" *catalog-books*))

(defun checkout-book (name)
  (if (member name library-shelf)
      (progn
       (setf library-shelf (remove name library-shelf :count 1))
       (setf my-shelf (cons name my-shelf))
       (format t "Checked out ~a~%" name))
      (format t "Sorry, ~a is not available~%" name)))

(defun intake-books (books)
  (setf *catalog-books* (append books *catalog-books*))
  (length books))

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
  (setf *catalog-books* '())

  ; (setf library-shelf '())
  ; (setf my-shelf `())

  ; (let ((count-1 (get-id)))
  ;   (format t "Returned: ~a~%" count-1))

  (let ((count-1 (intake-books (list (make-catalog-book :id (get-id) :title 'the-hobbit)
                                     (make-catalog-book :id (get-id) :title 'dune)
                                     (make-catalog-book :id (get-id) :title 'elder-race)
                                     (make-catalog-book :id (get-id) :title 'sapiens)))))
    (format t "Returned: ~a~%" count-1))

  ; ; add a list
  ; (let ((count-1 (stock-book '(elder-race ancient-world))))
  ;   (format t "Returned: ~a~%" count-1))

  ; ; add an atom
  ; (let ((count-1 (stock-book 'the-hobbit)))
  ;   (format t "Returned: ~a~%" count-1))

  ; ; add a dupe
  ; (let ((count-1 (stock-book 'the-hobbit)))
  ;   (format t "Returned: ~a~%" count-1))

  ; (print-stock)

  ; (checkout-book `money-ball)
  ; (checkout-book `the-hobbit)
  ; (checkout-book `ancient-world)
  ; (print-stock)

  ; (return-book `the-hobbit)

  (print-catalog))