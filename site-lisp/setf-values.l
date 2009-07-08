;;;; -*- mode: lisp; package: lisp -*-
;;;;
;;;; setf-values --- make setf support multiple values.
;;;;
;;;; Author: bowbow99 <bowbow99@gmail.com>
;;;; Created: 2009-07-09 04:29:43
;;;; Updated: 2009-07-09 05:12:12
;;;;


(in-package "lisp")

;;;;
;;;; modified version of setf related functions
;;;;

(defconstant +original-get-setf-method+ #'get-setf-method)
(defconstant +original-optimize-setf-method #'optimize-setf-method)
(defconstant +original-setf-expand-1 #'setf-expand-1)

(defun get-setf-method (form &optional env)
  (multiple-value-bind (vars vals stores store-form access-form)
      (get-setf-method-multiple-value form env)
    (values vars vals stores store-form access-form)))

(defun optimize-setf-method (vars vals stores store-form access-form newvalues)
  (let ((new-vars '())
        (new-vals '()))
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
    (values new-vars new-vals stores store-form access-form newvalues)))


(defun setf-expand-1 (place newvalue env)
  (if (atom place)
      `(setq ,place ,newvalue)
    (let (f)
      (when (and (null (get (car place) 'setf-update))
                 (null (get (car place) 'setf-lambda)))
        (multiple-value-setq (place f) (macroexpand-1 place env)
          (when f
            (return-from setf-expand-1 (setf-expand-1 place newvalue env)))))
      (when (setq f (get (car place) 'setf-update))
        (return-from setf-expand-1 `(,f ,@(cdr place) ,newvalue)))
      (multiple-value-bind (vars vals stores store-form access-form)
          (get-setf-method place env)
        (multiple-value-setq
            (vars vals store store-form access-form newvalue)
            (optimize-setf-method vars vals stores store-form
                                  access-form (list newvalue)))
        (if (or vars stores)
            `(let* ,(mapcar #'list vars vals)
               (multiple-value-bind ,stores ,(car newvalue)
                 ,store-form))
          store-form)))))

;;;;
;;;; setf values
;;;;

(define-setf-method values (&rest places)
  (let ((syms (mapcar #'gensym places)))
    (values nil nil syms
            `(setf ,@(mapcan #'list places syms)))))

(provide "setf-values")
;;;; setf-values.l ends here
