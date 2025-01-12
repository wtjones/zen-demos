(defstruct catalog-book
  (id 0 :type integer)
  (title "" :type string)
  (isbn "" :type string)
  (author "" :type string)
  publication-year
  genre)
(defstruct book-checkout
  patron-id
  book-id
  (occurred-at (get-universal-time) :type integer))

(defstruct patron
  (id 0 :type integer)
  name
  (enrolled-at (get-universal-time) :type integer))

(defconstant max-checkouts 3)
(defvar *catalog-books* nil)
(defvar *catalog-book-checkouts* nil)
(defvar *patrons* nil)
(defvar *last-id* 0)

(defun get-id ()
  (setf *last-id* (+ 1 *last-id*))
  *last-id*)

(defun print-catalog ()
  (format t "Catalog:~% ~a~%" *catalog-books*))
(defun print-catalog-checkouts ()
  (format t "Checked out:~% ~a~%" *catalog-book-checkouts*))
(defun print-patrons ()
  (format t "Patrons:~% ~a~%" *patrons*))

(defun load-catalog-from-file (file-path)
  (with-open-file (stream file-path :direction :input)
    (let ((entries (read stream)))
      (mapcar (lambda (entry)
                (apply #'make-catalog-book entry))
          entries))))


(defun get-patron-checkouts (patron-id)
  (remove-if-not
      (lambda (co)
        (= (book-checkout-patron-id co) patron-id))
      *catalog-book-checkouts*))

; Get a book by id
(defun get-catalog-book (book-id)
  (find book-id *catalog-books* :key #'catalog-book-id))


(defun get-patron-books (patron-id)
  (let*
      ((book-ids (mapcar
                     'book-checkout-book-id
                   (get-patron-checkouts patron-id)))
       (books (mapcar `get-catalog-book book-ids)))
    books))

(defun get-patron-books-by-isbn (patron-id isbn)
  (remove-if-not
      (lambda (book)
        (string= isbn (catalog-book-isbn book)))
      (get-patron-books patron-id)))


(defun checkout-book (book-id patron-id)
  (if (not (find
               patron-id *patrons*
             :key #'patron-id))
      (progn
       (format t "Patron ~a not found.~%" patron-id)
       (return-from checkout-book)))
  (format t "Trying to check out ~a to ~a~%" book-id patron-id)

  (if (not (find book-id *catalog-books* :key #'catalog-book-id))
      (progn
       (format t "Book ~a not found~%" book-id)
       (return-from checkout-book)))

  (if (find book-id *catalog-book-checkouts* :key #'book-checkout-book-id)
      (progn
       (format t "Book ~a is already checked out~%" book-id)
       (return-from checkout-book)))

  (if
   (get-patron-books-by-isbn
     patron-id
     (catalog-book-isbn (get-catalog-book book-id)))
   (progn
    (format t "Patron already has ~a checked out~%"
      (catalog-book-isbn (get-catalog-book book-id)))
    (return-from checkout-book)))
  (format t "Checking out ~a to ~a~%" book-id patron-id)

  (setf *catalog-book-checkouts*
    (cons
      (make-book-checkout :patron-id patron-id :book-id book-id)
      *catalog-book-checkouts*)))

; TODO: validate id
(defun intake-books (books)
  (setf *catalog-books* (append books *catalog-books*))
  (length books))

; TODO: validate id
(defun enroll-patrons (patrons)
  (setf *patrons* (append patrons *patrons*)))

(defun test ()
  (setf *catalog-books* (load-catalog-from-file "catalog.sexp"))
  (setf *catalog-book-checkouts* '())
  (setf *patrons* '())

  (enroll-patrons (list
                   (make-patron :id (get-id) :name "Abe")
                   (make-patron :id (get-id) :name "Brine")))

  (checkout-book
    (catalog-book-id
      (find "The Martian" *catalog-books*
        :key #'catalog-book-title :test #'string-equal))
    (patron-id
      (second *patrons*)))

  (checkout-book
    (catalog-book-id
      (find "the hobbit" *catalog-books*
        :key #'catalog-book-title :test #'string-equal))
    (patron-id
      (first *patrons*)))

  (checkout-book
    (catalog-book-id
      (find "Dune" *catalog-books*
        :key #'catalog-book-title :test #'string-equal))
    (patron-id
      (first *patrons*)))

  (checkout-book
    (catalog-book-id
      (find "1984" *catalog-books* :key #'catalog-book-title :test #'string-equal))
    (patron-id
      (second *patrons*)))

  (print-patrons)
  (print-catalog)
  (print-catalog-checkouts))
