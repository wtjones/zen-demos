(in-package #:maze)

(defconstant char-full-block #\█)
(defconstant char-light-shade #\░)
(defconstant char-medium-shade #\▒)
(defconstant char-dark-shade #\▓)
(defconstant char-two-dots #\⠅)

(defconstant char-wall char-light-shade)
(defconstant char-border char-dark-shade)
(defconstant char-empty #\Space)
(defconstant char-visited char-two-dots)
(defconstant char-entrance #\E)
