
;;;; cl-ppcre semantics  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  first pass: wordify the string = any non whitespace character
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


(defun (get-one-token statement)
	""
  ())

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
					
