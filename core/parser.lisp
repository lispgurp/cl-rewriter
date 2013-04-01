(ql:quickload "cl-ppcre")

;(defpackage :cl-rewriter
;  (:use :common-lisp :cl-ppcre))

;(in-package :cl-rewriter)

;; tokenizer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; data structure 
 
; note - all patterns are escaped first from the common lisp reader and then for the cl-ppcre parser
(defun make-tokenization-precedence ()
  "return plist declaring tokenization precedence
   rule := (list <rule-name> <rule-content>)
   rule-content := (list <rule>) | <pattern> 
   pattern := (list <key> <string>)"
  '(:tokenization-precedence
    (:whitespace "\\s+")
    (:curly-braces 
     (:open-curly "({)")
     (:closed-curly "(})"))
    (:parenthesis 
     (:open-paren "(\()")
     (:closed-paren "(\))"))
    (:simple-multiple-character-groups
     (:arrow-derference "(->)")
     (:open-comment "(\\'*')")
     (:closed-comment "('*'/)")
     (:single-line-comment "(\\)")
     (:scope-of-resolution "(::)")
     (:character "('.')")
     (:string "(\".*\")"))
    (:simple-single-character-groups
     (:semi-colon "(;)")
     (:colon "(:)")
     (:star "(\\*)")   
     (:dot "(\\.)")
     (:pound "(\#)"))))

(defvar *tp* nil)

(setf *tp*
      (make-tokenization-precedence))

; data structure accessors

(defun rule-name (rule)
  (first rule))

(defun rule-content (rule)
  (rest rule))

(defun is-pattern? (obj)
  "Right now primitive string comp, but maybe in the future might want to verify 
   whether this is a genuine pattern or not via cl-ppcre utility"
  (and (listp obj)
       (keywordp (first obj))
       (stringp  (second obj))))

(defun is-rule? (obj)
  (and (listp obj)
       (keywordp (first obj))
       (listp (second obj))))

(defun rule-pattern (obj)
  (second obj))

; main functions 

(defun apply-rule-to-line (name-of-rule ln file-path)
  (let ((r (find-rule name-of-rule *tp*)))
    (with-open-file (s file-path)
      (loop 
         for line = (read-line s nil 'eof)
         for i from 1
         until (eq line 'eof)
         do (when (eq i ln)
              (apply-rule-to-string r line)
              (return))))))
 
(defun find-rule (name r)
  (recur-rule 
   r
   :child-fn #'(lambda (rule)
                   (if (eq (rule-name rule) name) rule nil))
   :pattern-fn #'(lambda (rule) 
                   (if (eq (rule-name rule) name) rule nil))
   :recur-fn #'(lambda (rule)
                 (let ((result (find-rule name rule)))
                   (when (not (null result))
                     (return-from find-rule result))))
   :iter-type 'do))

(defun apply-rule-to-string (r str)
  (format t "applying rule(~A), ~A ~%" (rule-name r) str)
  (recur-rule
   r
   :pattern-fn #'(lambda (rule)
                   (break-apart-string str :based-on (rule-pattern rule)))
   :recur-fn #'(lambda (rule)
                 (list (apply-rule-to-string rule str)))
   :iter-type 'append))

(defun recur-rule (r &key child-fn pattern-fn recur-fn iter-type)
  (cond
    ((is-pattern? r)
     (funcall pattern-fn r))
    ((is-composite-rule? r)
     (process-child-rules r child-fn recur-fn iter-type))))

; want it in both cases
(defun process-child-rules (r child-fn recur-fn iter-type)
  (when (not (null child-fn))
      (funcall child-fn r))
  (cond ((eq iter-type 'do)
         (loop for rule in (rule-content r)
            do (funcall recur-fn rule)))
        ((eq iter-type 'append)
         (loop for rule in (rule-content r)
            append (funcall recur-fn rule)))))
             
; main function utils
          
(defun break-apart-string (str &key based-on)
  (format t "  :applying-pattern(~A)~%" based-on)
  (let ((results 
         (cl-ppcre:split based-on str)))
    (format t "  -> ~S ~%" results)
    results))

