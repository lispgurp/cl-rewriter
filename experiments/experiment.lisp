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

(defun find-rule2 (name hierarchy)
  (recur-rule 
   hierarchy
   :pattern-fn #'(lambda (rule) (nil)) 
   :name-fn #'(lambda (rule) (r))
   :child-fn #'(lambda (rule)
                 ;result = recur-rule with this specification
                 ;when the result is not null, return from the recur-rule
                 )))

(defun recur-rule (r &key pattern-fn name-fn child-fn)
  (cond
    ((null r) nil)
    ((is-pattern? r)
     (funcall pattern-fn r))
    ((eq (rule-name r) r)
     (funcall name-fn r))
    (t
     (loop for rule in (rule-content r)
          do (funcall child-fn r)))))
          
          
          
         
