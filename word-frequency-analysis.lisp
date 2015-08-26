;;;; Problem: talking a list of words, and producing a list of how common they are

(asdf:load-system "alexandria")

(defparameter wordlist '(ekollon ekollon ek eklestiastikminster ekologi))

(defun pprint-table (data amount)
  (loop :for word :being :each :hash-key :of data
	:for count from 1 to amount
	:do (format t "~A: ~A~%" word (gethash word data))))
		(print word)))

;;; In order to sort the tables for backward and forward searching
;;; we want to build some sublists and lookup lists
;;;
;;; (sort (alexandria:hash-table-alist apa) #'> :key #'cdr)
;;; this is the effect we want to get, a sorted list on values

(defun generate-wordhash (file)
  (let ((table (make-hash-table :test #'equal)))
    (with-open-file (s file :direction :input)
      (do ((line (read-line s nil nil)
		 (read-line s nil nil)))
	  ((null line))
	;;	(count-words (uiop/utility:split-string line))))
	(if (nth-value 0 (gethash line table))
	    (incf (gethash line table))
	    (setf (gethash line table) 1))))
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
