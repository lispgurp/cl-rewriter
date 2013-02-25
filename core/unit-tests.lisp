;;; parser.lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(find-rule :whitespace *tp*)     
(find-rule :parenthesis *tp*)


(setf tst-file "c:/users/gurp/desktop/thread.h")  
(apply-rule-to-line :whitespace 20 tst-file)
(apply-rule-to-line :parenthesis 94 tst-file)
(setf tv (apply-rule-to-line :open-paren 79 tst-file))

