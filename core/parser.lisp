; parser.lisp - Gobbles up blub code (e.g. known as snarfing) and generates a Lisp AST as an s-expression. 
; The idea is that noisy blub code (even if it is well factored) 
; is much easier to grok and refactor inside a interactive reprogrammable environment like a REPL 
; instead of the traditional "manual" ways that developers try to understand/refactor code in a blub 
; context.
;
; *assumption: code being read in compiles (e.g. syntactically valid statements!). if there is serious errors (to define)
; then tell the user, from the prespective of snarfing, what the problem is. But don't try to validate upfront
;
; *I reserve the right to change the parsing technique - right now I am going to iteratively use regexp and stream
; reading. If this is too much of a pain/boring.  I can use an open source or proprietary parsing engine.
; for the moment this is fun enough! :). Some possibilities 
;   ANTLR/Clojure
;   Proprietary "Kitchen Sink" implementor 
;   Roll my own "Natural Language" Parser as per Kitchen Sink
;   FFI into some welll known C library used for parsing
;   FFI into .net or java binary that is easy to use 

(ql:quickload "cl-ppcre")

(defpackage :cl-rewriter
  (:use :common-lisp :cl-ppcre))

(in-package :cl-rewriter)

;; tokenizer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; data structure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun make-tokenization-precedence ()
  "return plist declaring tokenization precedence
   note - all patterns are escaped first from the common lisp reader and then for the cl-ppcre parser
   rule := (list <key> <content>) | <pattern>
   content := (list <rule>*)
   pattern = cl-ppcre regexp string"
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

(defvar *tokenization-precedence* nil)

(setf *tokenization-precedence*
      (make-tokenization-precedence))

(defun rule-name (rule)
  (first rule))

(defun rule-content (rule)
  (rest rule))

(defun is-pattern? (obj)
  "Right now primitive string comp, but maybe in the future might want to verify 
   whether this is a genuine pattern or not"
  (stringp obj))

;;; main functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apply-rule-at-line (file rule-name ln)
  (let ((r 
         (find-rule 
            rule-name 
            *tokenization-precedence*)))
    (with-open-file (s file)
      (loop 
         for line = (read-line s nil 'eof)
         for i from 0  
         until (eq line 'eof)
         do (when (eq i ln)
              (apply-rule-at-string r line)
              (return))))))

(defun apply-rule-at-string (r str)
  (format t "applying rule(~A), ~A ~%" (rule-name r) str)
  (cond   
    ((null r) nil)
    ((is-pattern? r)
     (break-apart-string str :based-on r))
    (t
     (loop for sub-rule in (rule-content r) 
        append
          (list (apply-rule-at-string sub-rule str))))))

; utilities
          
(defun break-apart-string (str &key based-on)
  (format t "  :applying-pattern(~A)~%" based-on)
  (let ((results 
         (cl-ppcre:all-matches-as-strings based-on str)))
    (format t "  -> ~A ~%" (into-single-str results))))

(defun into-single-str (lst-of-strs)
  (reduce #'(lambda (l r) 
              (concatenate 'string l r))
          lst-of-strs))

(defun find-rule (name r)
  "traverses the hierarchy, returning the target"
  (cond   
    ((or (is-pattern? r) (null r)) nil)
    ((eq (rule-name r) name) r)
    (t
     (loop for rule in (rule-content r) 
        do 
          (let ((result (find-rule name rule)))
            (when (not (null result))
              (return result)))))))


  
