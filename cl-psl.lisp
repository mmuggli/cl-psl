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

(defstruct (hsp (:constructor make-hsp 
(matches  ; Number of bases that match that aren't repeats
misMatches ; Number of bases that don't match
repMatches ; Number of bases that match but are part of repeats
nCount ; Number of 'N' bases
qNumInsert ; Number of inserts in query
qBaseInsert ; Number of bases inserted in query
tNumInsert ; Number of inserts in target
tBaseInsert ; Number of bases inserted in target
strand ; '+' or '-' for query strand. For translated alignments, second '+'or '-' is for genomic strand
qName ; Query sequence name
qSize ; Query sequence size
qStart ; Alignment start position in query
qEnd ; Alignment end position in query
tName ; Target sequence name
tSize ; Target sequence size
tStart ; Alignment start position in target
tEnd ; Alignment end position in target
blockCount ; Number of blocks in the alignment (a block contains no gaps)
blockSizes ; Comma-separated list of sizes of each block
qStarts ; Comma-separated list of starting positions of each block in query
tStarts) ; Comma-separated list of starting positions of each block in target
))
matches  ; Number of bases that match that aren't repeats
misMatches ; Number of bases that don't match
repMatches ; Number of bases that match but are part of repeats
nCount ; Number of 'N' bases
qNumInsert ; Number of inserts in query
qBaseInsert ; Number of bases inserted in query
tNumInsert ; Number of inserts in target
tBaseInsert ; Number of bases inserted in target
strand ; '+' or '-' for query strand. For translated alignments, second '+'or '-' is for genomic strand
qName ; Query sequence name
qSize ; Query sequence size
qStart ; Alignment start position in query
qEnd ; Alignment end position in query
tName ; Target sequence name
tSize ; Target sequence size
tStart ; Alignment start position in target
tEnd ; Alignment end position in target
blockCount ; Number of blocks in the alignment (a block contains no gaps)
blockSizes ; Comma-separated list of sizes of each block
qStarts ; Comma-separated list of starting positions of each block in query
tStarts) ; Comma-separated list of starting positions of each block in target


(defun parse-line (line)
  (let ((fields (split-sequence:split-sequence #\tab line)))
    (dolist (field-num '(18 19 20))
      (setf (nth field-num fields) (mapcar #'(lambda (s) (parse-integer s)) (remove "" (split-sequence:split-sequence #\, (nth field-num fields)) :test #'equal))))
    (do ((i 0 (incf i))
	 (lst fields (setf lst (cdr lst))))
	( (null lst) (apply #'make-hsp fields))
      (if (not (member i '(8 9 13 18 19 20))) 
	  (setf (car lst) (parse-integer (car lst)))))))

    
