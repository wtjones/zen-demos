# Common Lisp

## Project structure examples

### Package def

Usage of `packages.lisp]: [drakma](https://github.com/edicl/drakma/blob/master/packages.lisp)

### TUI

[Charms](https://turtleware.eu/posts/cl-charms-crash-course.html

## Constants

```
(defmacro defconst (name value &optional docstring)
  `(progn
     (if (boundp ',name)
         (format *error-output* "Warning: Redefining constant ~A~%" ',name))
     (defparameter ,name ,value ,@(when docstring (list docstring)))
     ',name))


(defconst +my-const+ 42 "A test constant.")
(format t "Value: ~A~%" +my-const+)

(defconst +my-const+ 99) ;; Will print a warning
(format t "New Value: ~A~%" +my-const+)

```

## Naming conventions


    +MY-CONST+: Used for constants (like defconstant values).
    *my-var*: Used for special (global) variables (defparameter or defvar).
    my-var: Used for lexical variables (local variables in functions).
    isit?: Boolean return values

