;;;; Problem: talking a list of words, and producing a list of how common they are

(defparameter wordlist '(ekollon ekollon ek eklestiastikminster ekologi))

(defun count-words (words)
  (if (nth-value 0 (gethash words table))
      (incf (gethash words table))
      (setf (gethash words table) 1)))

(defun pprint-table (data)
;;  (describe table)
  (loop :for word :being :each :hash-key :of data
	:do (format t "~A: ~A~%" word (gethash word data))))
		(print word)))

(defun create-frequency-table (file)
  (let ((table (make-hash-table :test #'equal)))
    (with-open-file (s file :direction :input)
      (do ((line (read-line s nil nil)
		 (read-line s nil nil)))
	  ((null line))
;;	(count-words (uiop/utility:split-string line))))
	(count-words line)))
    (pprint-table table)))
