;;;; Problem: talking a list of words, and producing a list of how common they are

(asdf:load-system "alexandria")

(defparameter wordlist '(ekollon ekollon ek eklestiastikminster ekologi))

(defun count-words (words the-table)
  (if (nth-value 0 (gethash words the-table))
      (incf (gethash words the-table))
      (setf (gethash words the-table) 1)))

(defun pprint-table (data amount)
  (loop :for word :being :each :hash-key :of data
	:for count from 1 to amount
	:do (format t "~A: ~A~%" word (gethash word data))))
		(print word)))

(defun create-frequency-table (file)
  (let ((table (make-hash-table :test #'equal)))
    (with-open-file (s file :direction :input)
      (do ((line (read-line s nil nil)
		 (read-line s nil nil)))
	  ((null line))
;;	(count-words (uiop/utility:split-string line))))
	(count-words line table)))
    (pprint-table table 5)))

;;; some notes about hash tables
;;; CL-USER> (alexandria:hash-table-values table)
;;; (2 2 5 4)
;;; CL-USER> (alexandria:hash-table-keys table)
;;; (EKOLOGI EKLESTIASTIKMINSTER EK EKOLLON)
;;; CL-USER> (alexandria:hash-table-alist table)
;;; ((EKOLOGI . 2) (EKLESTIASTIKMINSTER . 2) (EK . 5) (EKOLLON . 4))

