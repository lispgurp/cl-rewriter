;;; rule tree traversal experimentation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun find-rule2 (name r)
  (recur-rule 
   name
   r
   :pattern-fn #'(lambda (rule) nil) 
   :name-fn #'(lambda (rule) (rule))
   :child-fn #'(lambda (rule)
                 (let ((result (find-rule2 name rule)))
                   (when (not (null result))
                     (return-from find-rule2 result))))
   :child-iter 'do))

(defun apply-rule-to-string (name r str)
  (format t "applying rule(~A), ~A ~%" (rule-name r) str)
  (recur-rule
   name
   r
   :pattern-fn #'(lambda (rule)
                   (let ((results
                          (break-apart-string str :based-on rule)))
                     results))
   :name-fn #'(lambda (rule) nil)
   :child-fn #'(lambda (rule)
                 (list apply-rule-to-string name rule str))
   :child-iter 'append))

(defun recur-rule (name r &key pattern-fn name-fn child-fn child-iter)
  (cond
    ((null r) nil)
    ((is-pattern? r)
     (funcall pattern-fn r))
    ((eq (rule-name r) name)
     (funcall name-fn r))
    ((eq child-iter-type 'do)
     (loop for rule in (rule-content r)
        do (funcall child-fn r)))
    ((eq child-iter-type 'append)
     (loop for rule in (rule-content r)
        append (funcall child-fn r)))))
             
