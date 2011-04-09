;;; -*- mode: lisp; package: lisp -*-
;;;
;;; setf-values.l --- setf ‚Å‘½’l‚ðˆµ‚¦‚é‚æ‚¤‚É‚·‚é
;;;
;;; Author:     bowbow99  <bowbow99@gmail.com>
;;; Version:    0.01.00
;;; Category:   Lisp Libraries
;;; License:    MIT (see COPYING.mit)
;;; Commentary: (see README.md)
;;; ChangeLog:  (see ChangeLog)

(in-package "lisp")

;;;;
;;;; * setf related functions modified for support multiple value.

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
    (multiple-value-bind (vars vals stores store-form access-form)
        (get-setf-method place env)
      (multiple-value-setq
          (vars vals stores store-form access-form newvalue)
          (optimize-setf-method vars vals stores store-form
                                access-form (list newvalue)))
      (multiple-value-bind (bindings store-from)
          (if (= (length stores) 1)
            (values (mapcar #'list
                      (append vars stores)
                      (append vals newvalue))
                    store-from)
            (values (mapcar #'list vars vals)
                    `(multiple-value-bind (,@stores)
                         ,(car newvalue)
                       store-form)))
        (if (null bindings)
          store-form
          `(let* (,@bindings) ,store-form))))))


;;;;
;;;; * values as place

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
