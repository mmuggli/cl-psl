;;;; cl-psl.lisp

(in-package #:cl-psl)

;;; "cl-psl" goes here. Hacks and glory await!

(defclass queryresult ()
  (id seq-len hits))

(defclass hit ()
  (id seq-len hsps))

(defclass hsp ()
  (hit-end 
   hit-gap-num
   hit-gapopen-num
   hit-span-all
   hit-start
   hit-start-all
   match-num
   mismatch-num
   match-rep-num
   n-num
   query-end
   query-gap-num
   query-gapopen-num
   query-span-all
   query-start
   query-start-all
   hspfragments))

(defclass hspfragment ()
  (hit
   hit-strand
   query
   query-strand))

(defun loadpsl (fname)
  (with-open-file (stream fname)
    (do ((hsps)
	 (lineno 1 (incf lineno))
	 (line (read-line stream nil)
               (read-line stream nil)))
        ((null line) hsps)
      (if (> lineno 5)
	  (push (parse-line line) hsps)))))
	  

(defun parse-line (line)
  (let ((fields (split-sequence:split-sequence #\tab line)))
    (dolist (field-num '(18 19 20))
      (setf (nth field-num fields) (mapcar #'(lambda (s) (parse-integer s)) (remove "" (split-sequence:split-sequence #\, (nth field-num fields)) :test #'equal))))
    (do ((i 0 (incf i))
	 (lst fields (setf lst (cdr lst))))
	( (null lst) fields)
      (if (not (member i '(8 9 13 18 19 20))) 
	  (setf (car lst) (parse-integer (car lst)))))))

    
