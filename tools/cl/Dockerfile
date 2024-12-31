FROM ubuntu:24.04
  RUN apt-get update -y
  RUN apt-get upgrade -y
  RUN apt-get install -y curl sbcl rlwrap build-essential libncursesw5-dev 
  
  RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
  RUN sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(with-open-file (out \"~/.sbclrc\" :direction :output :if-exists :append :if-does-not-exist :create) (format out \"~a~%\" \"(load (merge-pathnames \\\"quicklisp/setup.lisp\\\" (user-homedir-pathname)))\"))"
  
  RUN sbcl --eval "(ql:quickload \"cl-charms\")"

