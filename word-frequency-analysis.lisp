;;;; Problem: talking a list of words, and producing a list of how common they are

(asdf:load-system "alexandria")
(asdf:load-system "cl-ppcre")

(defparameter wordlist '(ekollon ekollon ek eklestiastikminster ekologi))

(defun pprint-table (data amount)
  (loop :for word :being :each :hash-key :of data
	:for count from 1 to amount
	:do (format t "~A: ~A~%" word (gethash word data))))

;;; In order to sort the tables for backward and forward searching
;;; we want to build some sublists and lookup lists
;;;
;;; (sort (alexandria:hash-table-alist apa) #'> :key #'cdr)
;;; this is the effect we want to get, a sorted list on values

(defun generate-wordhash (file)
  (let ((table (make-hash-table :test #'equal))
	(word-line))
    (with-open-file (s file :direction :input)
      (do ((line (read-line s nil nil)
		 (read-line s nil nil)))
	  ((null line))
;; wash the line from junk so it becomes just the words
	(setf line (uiop/utility:stripln line))
 	(setf line (cl-ppcre:regex-replace-all "--" line " "))
 	(setf line (cl-ppcre:regex-replace-all "," line ""))
 	(setf line (cl-ppcre:regex-replace-all ";" line ""))
;; now split the line on white space
	(setf word-line (uiop/utility:split-string line))
	(dolist (w word-line)
	  (if (nth-value 0 (gethash w table))
	      (incf (gethash w table))
	      (setf (gethash w table) 1)))))
    table))

;; helper function to make a top list of values
(defun generate-sorted-keys (table)
  (sort (loop :for key :being :each :hash-key :of table
	      :collecting key) #'string-lessp))

;; helper function to get the values from sorted keys
(defun generate-matching-values (keys table)
  (let ((values ))
    (loop :for key :in keys
	  :do (push (gethash key table) values))
    (nreverse values)))

;; helper function to sequencially grab the biggest values
;; to be used to construct a sorted top list
(defun find-max (lst)
  (let ((tmp 0))
    (labels ((helper (next tmp)
	       (cond
		 ((null next)
		  tmp)
		 ((>= (car next) tmp)
		  (progn
		    (setf tmp (car next))
		    (helper (cdr next) tmp)))
		 ((<= (car next) tmp)
		  (progn
		  (helper (cdr next) tmp)))
		 (t (print "this should not happen")))))
      (helper lst tmp))))

;; loop doing this...

(defun generate-top-table (keys vals)
  (loop :for p :from 1 :to (length vals)
	:do 
	   (let* ((max (find-max vals))
		  (pos (position max vals)))
	     (format t "key:~A value:~A~%" (nth pos keys) (nth pos vals))
	     (setf keys (remove (nth pos keys) keys :count 1))
	     (setf vals (remove (nth pos vals) vals :count 1)))))

(defun generate-top-table (keys vals)
  (let ((table ))
    (loop :for p :from 1 :to (length vals)
	  :do 
	     (let* ((max (find-max vals))
		    (pos (position max vals)))
	       (push (cons (nth pos keys) (nth pos vals)) table)
	       (setf keys (remove (nth pos keys) keys :count 1))
	       (setf vals (remove (nth pos vals) vals :count 1))))
    (nreverse table)))

(defun tab ()
  (defparameter wh (generate-wordhash "/home/ante/src/wordstats/testwords.txt"))
  (defparameter wh (generate-wordhash "/home/ante/src/wordstats/emma.txt"))
(defparameter keys (generate-sorted-keys wh))
(defparameter vals (generate-matching-values keys wh))
(generate-top-table keys vals)
)
