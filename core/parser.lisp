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


;(defpackage :cl-rewriter)
;  (:use :common-lisp :cl-ppcre))

;(in-package :cl-rewriter)

;; Tokenizer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun tokenize-file (file-name path)
  (let ((full-name (format nil "~A\\~A" path file-name)))
	(with-open-file (s full-name)
		  (tokenize-stream s))))

(defun tokenize-stream (s) 
  (loop for ln = (read-line s nil 'EOF)
     counting n 
	 until (eq ln 'EOF)
	 collect (tokenize-line ln n)))


;"all patterns are escaped first from the common lisp reader and then for the cl-ppcre parser"        
; tokenization-precedence
;   whitespace
;   curly braces 
;   parenthesis
;   keywords TODO
;   simple-multiple-character-groups TODO still:  == != >= <=
;   simple-single-character-groups   TODO still:  >,<,! 
;   

;
; plist declaring tokenization precedence
; rule = (list <name> <content>)
; content = (list <rule>*) | <string>
; <name> = <keyword>
; <keyword> = a lisp keyword
; <string> = a lisp string
(defun make-tokenization-precedence ()
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

(defun rule-name (rule)
  (first rule))

(defun rule-content (rule)
  (rest rule))

(defun find-rule (name r)
  "traverses the hierarchy, returning the target"
  (cond   
    ((or (stringp r) (null r)) nil)
    ((eq (rule-name r) name) r)
    (t
     (loop for kid in (rule-content r) 
        do 
          (let ((result (find-rule name kid)))
            (when (not (null result))
              (return result)))))))





; want to be able to break apart string at each level of the hierarchy
; and trace what the result will be (strikes me as a combinator/mapping 
; operation of some sort) ???
  
(defun break-apart-string (str &key based-on)
  (format t "apply pattern ~A to ~A ~%" based-on str)
  (let ((results (cl-ppcre:all-matches-as-strings based-on str)))
    (format t "results ~S" results)
    results))

(defun tokenize-line (ln line-number)
  (format t "LINE ~A IS ~A~%" line-number ln))
