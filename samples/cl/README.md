# Common Lisp Notes and Observations

## Language review

### Things I like

### Things I don't like

#### defconstant is not idempotent

In most languages I would expect the following to reinitialized upon build/run:

`(defconstant char-full-block #\█)`

In Lisp, an error is raised if the constant already exist. Workarounds that I am aware of:

* Keep constants in a separate file
    * This avoids errors when working in other files. However, redefinition of a const requires the repl to be restarted.
* Don't use defconstant.
    * Mutuable can be redefined without errors.
    * Not really a fix.
