; parser.lisp - gobbles up code to be played with in Lisp Macros
; MAJOR ASSUMPTION: Code being read in compiles (e.g. syntactically valid statements!)
; so we can assume that the code reads as though everything we see is a valid language element
; unless something goes seriously wrong (TODO: we have to figure that out) 
; 2. Creating an AST out of a blub language is not hard (logically) especially in Lisp! (This is the hope anyways)

;; For unit testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cl-ppcre functions used by cl-lex:*allow-named-registers*, cl-ppcre:create-scanner, cl-ppcre:scan

;; Basic unit test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIME compile file
;; run (in-package :cl-rewriter)
;      (ql:quickload "cl-ppcre") 
;;  (tokenize-file *file-name* *code-path*)

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

; plist declaring tokenization precedence
(defglobal tokenization-precedence)

(setf tokenization-precedence
      (:tokenization-precedence
       ((:whitespace "\\s+")
        (:curly-braces (:open-curly "({)" :closed-curly "(})"))
        (:parenthesis '(:open-paren "(\()" :closed-paren "(\))"))
        (:simple-multiple-character-groups
         ((:arrow-derference "(->)")
          (:open-comment "(\\'*')")
          (:closed-comment "('*'/)")
          (:single-line-comment "(\\)")
          (:scope-of-resolution "(::)")
          (:character "('.')")
          (:string "(\".*\")")))
        (:simple-single-character-groups
         ((:semi-colon "(;)")
          (:colon "(:)")
          (:star "(\\*)")   
          (:dot "(\\.)")
          (:pound "(\#)"))))))
  
(defun break-apart-string (str &key based-on)
  (format t "apply pattern ~A to ~A ~%" based-on str)
  (let ((results (cl-ppcre:all-matches-as-strings based-on str)))
    (format t "results ~S" results)
    results))

(defun tokenize-line (ln line-number)
  (format t "LINE ~A IS ~A~%" line-number ln))
