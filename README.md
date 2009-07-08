Overview
=============
add support for multiple values on setf.

this extension re-define some functions in lisp/setf.l to make setf
support multiple values.

- lisp::optimize-setf-method
- lisp::setf-expand-1
- lisp::get-setf-method

and define-setf-method for values.


Usage
=======
just put setf-values.l somewhene in *load-path* and load it.

(require "setf-values")


Notes
======

when you need original implementation of setf for some reasons, they
are stored in lisp::+original-<function-name>+. so you can restore
them by eval following.

;;; restoring original functions
(setf (symbol-function lisp::get-setf-method)
      lisp::+original-get-setf-method
      (symbol-function lisp::optimize-setf-method)
      lisp::+original-optimize-setf-method
      (symbol-function lisp::setf-expand-1)
      lisp::+original-setf-expand-1)
