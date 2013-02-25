; parser.lisp - Gobbles up blub code (e.g. known as snarfing) and generates a Lisp AST as an s-expression. 
; The idea is that noisy blub code (even if it is well factored) 
; is much easier to grok and refactor inside a interactive reprogrammable environment like a REPL 
; instead of the traditional "manual" ways that developers try to understand/refactor code in a blub 
; context.
;
; *assumption: code being read in compiles (e.g. syntactically valid statements!). if there is serious errors (to define)
; then tell the user, from the prespective of snarfing, what the problem is. But don't try to validate upfront)
;
; *I reserve the right to change the parsing technique - right now I am going to iteratively use regexp and stream
; reading. If this is too much of a pain/boring.  I can use an open source or proprietary parsing engine.
; for the moment this is fun enough! :). Some possibilities 
;   Proprietary "Kitchen Sink" implementor 
;   Roll my own "Natural Language" Parser as per Kitchen Sink
;   FFI/Interop with  
;       1. some welll known C library used for parsing (lexx/yacc)
;       2. easy to use .net library F# (Irony)
;       3. easy to use java library Clojure (ANTLR)

(ql:quickload "cl-ppcre")

;(defpackage :cl-rewriter
;  (:use :common-lisp :cl-ppcre))

;(in-package :cl-rewriter)

;; tokenizer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; data structure  ;;;
 
; note - all patterns are escaped first from the common lisp reader and then for the cl-ppcre parser
(defun make-tokenization-precedence ()
  "return plist declaring tokenization precedence
   rule := (list <rule-name> <rule-content>)
   rule-content := <complex-rule> | <pattern-rule> 
   <complex-rule> := rule
   <pattern-rule> := (list <key> <string>)"
  '(:tokenization-precedence
    (:whitespace "\\s+")
    (:curly-braces 
     (:open-curly "(\{)")
     (:closed-curly "(\})"))
    (:parenthesis 
     (:open-paren "(\\()")
     (:closed-paren "(\\))"))
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

(defun rule-name (rule)
  (first rule))

(defun rule-content (rule)
  (rest rule))

(defun rule-pattern (pattern-rule)
  (second pattern-rule))

(defun is-pattern-rule? (obj)
  "Right now primitive string comp, but maybe in the future might want to verify 
   whether this is a genuine pattern or not via cl-ppcre utility"
  (and (listp obj)
       (keywordp (first obj))
       (stringp (first (rest obj)))))

; core tokenization functions ;;;

(defun apply-rule-to-line (name-of-rule ln file-path)
  (let ((r (find-rule name-of-rule *tp*)))
    (with-open-file (s file-path)
      (loop 
         for line = (read-line s nil 'eof)
         for i from 1
         until (eq line 'eof)
         do (when (eq i ln)
              (return (apply-rule-to-string r line)))))))

(defun find-rule (name r)
  (recur-rule
   r
   :pattern-rule-fn #'(lambda (rule)
                        (when (eq (rule-name rule) name)
                          (return-from find-rule rule)))
   :complex-rule-fn #'(lambda (rule)
                        (when (eq (rule-name rule)  name)
                          (return-from find-rule rule)))
   :rule-iter-fn #'(lambda (ith-rule)
                     (let ((result (find-rule name ith-rule)))
                       (when (not (null result))
                         (return-from find-rule result))))
   ))

(defun apply-rule-to-string (r str)
  (setf *rule-acc* (list str))
  (setf *rule-acc* (recur-rule-to-strings r *rule-acc*))
  *rule-acc*)

(defun recur-rule-to-strings (r strs)
  (recur-rule
   r 
   :pattern-rule-fn #'(lambda (rule)
                        (apply-pattern-to-strings
                               (rule-pattern rule)
                               *rule-acc*))
   :complex-rule-fn #'identity
   :rule-iter-fn #'(lambda (ith-rule)
                     (setf *rule-acc* 
                           (recur-rule-to-strings ith-rule *rule-acc*)))))
      
; utils ;;;

; global accumulator for recur-rule, to be reset at every user level call
; (e.g. see apply-rule-to-string, recur-rule-to-strings)
(defvar *rule-acc*)

; rule traversal
(defun recur-rule (r &key pattern-rule-fn complex-rule-fn rule-iter-fn)
  (if (is-pattern-rule? r)
      (funcall pattern-rule-fn r)
      (progn
        (funcall complex-rule-fn r)
        (loop for ele in (rule-content r)
           do (funcall rule-iter-fn ele)))))


; pattern matching
(defun apply-pattern-to-strings (patt strs)  
  (let ((result (mapcar #'(lambda (str)
                           (cl-ppcre:split patt str 
                              :with-registers-p t
                              :omit-unmatched-p t))
                        strs)))
    (reduce #'(lambda (l r)
                (append l r))
            result)))
                
              
