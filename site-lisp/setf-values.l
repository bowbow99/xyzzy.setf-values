;;;; -*- mode: lisp; package: lisp -*-
;;;;
;;;; setf-values.l --- make setf support multiple values.
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;; Created: 2009-07-09 04:29:43
;;;; Updated: 2009-07-17 04:22:48
;;;;
;;;; License: The MIT License (http://www.opensource.org/licenses/mit-license.php)
#|
Copyright (c) 2009 Tohru Kitamura

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
|#


(in-package "lisp")

;;;;
;;;; setf related functions modified for support multiple value.
;;;;

(defun get-setf-method (form &optional env)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method-multiple-value form env)
    (values vars vals stores store-form access-form)))

(defun optimize-setf-method (vars vals stores store-form access-form newvalues)
  (let ((new-vars '())
        (new-vals '())
        (new-stores stores)
        (new-newvalues newvalues))
    (do ((var vars (cdr var))
         (val vals (cdr val)))
        ((endp var))
      (cond ((or (constantp (car val))
                 (symbolp (car val)))
             (setq store-form (nsubst (car val) (car var) store-form
                                      :test #'eq)
                   access-form (nsubst (car val) (car var) access-form
                                       :test #'eq)))
            (t (push (car var) new-vars)
               (push (car val) new-vals))))
    (when (and (> (length stores) 1)
               (car newvalues)
               (eq (caar newvalues) 'values))
      (setf new-newvalues '((values))
            new-stores '())
      (do ((store stores (cdr store))
           (newval (cdar newvalues) (cdr newval)))
          ((endp store))
        (if (or (constantp (car newval))
                (symbolp (car newval)))
            (setq store-form (nsubst (car newval) (car store) store-form
                                     :test #'eq))
          (progn
            (push (car store) new-stores)
            (push (car newval) (cdar new-newvalues))))))
    (values new-vars new-vals new-stores store-form access-form new-newvalues)))


(defun setf-expand-1 (place newvalue env)
  (if (atom place)
    `(setq ,place ,newvalue)
    (let (f)
      (when (and (null (get (car place) 'setf-update))
                 (null (get (car place) 'setf-lambda)))
        (multiple-value-setq (place f) (macroexpand-1 place env))
        (when f
          (return-from setf-expand-1 (setf-expand-1 place newvalue env))))
      (when (setq f (get (car place) 'setf-update))
        (return-from setf-expand-1 `(,f ,@(cdr place) ,newvalue)))
      (multiple-value-bind (vars vals stores store-form access-form)
          (get-setf-method place env)
        (multiple-value-setq
            (vars vals stores store-form access-form newvalue)
            (optimize-setf-method vars vals stores store-form
                                  access-form (list newvalue)))
        (setf vars (append vars stores)
              vals (append vals (if (= (length stores) 1)
                                  newvalue
                                  (mapcar (lambda (ignore) nil) stores))))
        (cond ((null vars) store-form)
              ((> (length stores) 1)
               `(let* ,(mapcar #'list vars vals)
                  (multiple-value-setq ,stores ,(car newvalue))
                  ,store-form))
              (t `(let* ,(mapcar #'list vars vals)
                    ,store-form)))))))

;;;;
;;;; (setf (value ...) ...)

(define-setf-method values (&environment env &rest places)
  (labels ((mappend (fn &rest lists)
             (apply #'append (apply #'mapcar fn lists))))
    (let ((expansions (mapcar (lambda (place)
                                (multiple-value-list
                                 (get-setf-method-multiple-value place env)))
                        places)))
      (values (mappend #'first  expansions)
              (mappend #'second expansions)
              (mappend #'third  expansions)
              `(values ,@(mapcar #'fourth expansions))
              `(values ,@(mapcar #'fifth  expansions))))))


(provide "setf-values")
;;;; setf-values.l ends here
