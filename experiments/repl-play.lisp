; Two things I need to verify
; 1. Is my theory of using nested p-lists correct?
; 2. Does the "hierarchical" initial phases of the parser actually work


;;;; cl-ppcre semantics  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For unit testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cl-ppcre functions used by cl-lex:*allow-named-registers*, cl-ppcre:create-scanner, cl-ppcre:scan

;; Basic unit test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SLIME compile file
;; run (in-package :cl-rewriter)
;      (ql:quickload "cl-ppcre") 
;;  (tokenize-file *file-name* *code-path*)


;;  first pass: wordify the string = any non whitespace character
;(ql:quickload "cl-ppcre")
(split "\\s+" *test-string*)

;; second pass: for each wordification match against the following "grammar" 
; = <C++ comment> | <C Comment> | <classic-C-tokens> | <character> | <string> | <preprocessor directive> | <word>

;; <classic-C-tokens> = :,::,*,.->,{,},(,)
(split "(->)|(;)|(::)|(:)|(\\*)|(\\.)|({)|(})" *test-string* :with-registers-p t :omit-unmatched-p t)

;; <character> = '<anycharcter>'
(all-matches-as-strings "'.'" *test-string*)

;; <string> = "<anycharacter>"
(all-matches-as-strings "\".*\"" *test-string*)

;; <preprocessor directive> = #<any-character-up-to-a-space>

;; <C-Comment> = /*<anycharacter>*/
;; <C++-Comment> = //<anycharacter-up-to-end-of-line>


(defun make-play-p-list ()
  '(:a 1 :b 2))

(defun make-play-p-list-nested ()
  '(:pansy-pig-likes
    (:mud a-lot
     :splashing a-little)))

;;; Common Lisp Semantics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-splicing ()
  (let ((lst '(1 2 3)))
    `(,(rest lst))))

(defun do-canonical-example ()    
  "keep this"  
  (do ((n 0 (1+ n))
       (cur 0 next)
       (next 1 (+ cur next))) ;list of variable declaration lists (name initial-value step-form) note step-form is at end of iteration
      ((= 10 n) cur)          ;list of termination declaration lists
    (format t "n = ~A, cur = ~A next = ~A ~%" n cur next)))


; do binding semantics: assign values based on previous iteration
(defun do-binding-semantics ()
    (do ((cur 0 (1+ cur))
         (a 0 cur)
         (b 0 a))
        ((>= cur 10))
      (format t "do -- current:~A a:~A b:~A ~%" cur a b)))


; do* binding semantics: assign variables in order immediately (like let*)
(defun do*-binding-semantics ()
    (do* ((cur 0 (1+ cur))
          (a 0 cur)
          (b 0 a))
        ((>= cur 10))
      (format t "do* -- current:~A a:~A b:~A ~%" cur a b)))



(defun scan-test (pattern s)
  (multiple-value-list (scan pattern s)))

(defun let-progn-semantics ()
  (let ((v1 "do-this")
		(v2 "do-that"))
	(format t "~A ~%" v1)
	(format t "~A ~%" v2)))

;  (with-open-file (s "C:/Users/eyynhvu/Desktop/IntBasePP.cpp")

(defun EOF-Loop-semantics (s)
   "I was wondering if I call read-line repeatedly at the end will it always return 'EOF???"
	(loop for l = (read-line s nil 'EOF)
		 until (eq l 'EOF)
		 do (format t "~A ~%" l)))

(setf tst-file "c:/Lisp/Thread.h")

(defun counting-and-file-loop-semantics (path)
  (with-open-file (s path)
    (loop for l = (read-line s nil 'EOF)
          for i from 0
       until (eq l 'EOF)
       do (format t "~A: ~A ~%" i l))))   

(defun cond-semantics (tst-val)
  (cond ((eq tst-val 'first-tst)
         (format t "we have hit the first test"))
        ((eq tst-val 'second-tst)
         (format t "we have hit the second test"))
        ((eq tst-val 'third-tst)
         (format t "we have hit the third test"))))

(defun try-optional-keyword (&key (a-kwd-arg 'default-value))
  (format t "This is was what passed in: ~A" a-kwd-arg))

; reduce semantics
(defun into-single-str (lst-of-strs)
  (reduce #'(lambda (l r) 
              (concatenate 'string l r))
          lst-of-strs))

(defun test-if-semantics (x)
  "no else in this case"
  (if (eq x 1) (format t "we have one")))

(defun test-when-semantics (x)
  (when (eq x 1)
    (format t "we have one")))
					
