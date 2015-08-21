;;;; Problem: talking a list of words, and producing a list of how common they are

(asdf:load-system "alexandria")

(defparameter wordlist '(ekollon ekollon ek eklestiastikminster ekologi))

(defun count-words (words keys values)
;;  (format t "count-words: words - ~A~%" words)
  (if (nth-value 0 (gethash words keys))
      (progn
	(incf (gethash words keys))
	(setf (gethash (+ 1 (gethash words keys)) values)
	      (gethash words keys)))
      (progn
	(setf (gethash words keys) 1)
	(setf (gethash 1 values) words))))

;;; something is wrong with the updating of the reverse hash table
;;; se below

;; (defparameter valuetable (create-frequency-table "/home/ante/src/wordstats/testwords.txt"))
;; 
;; (alexandria:hash-table-alist valuetable)
;; ((8 . 7) (7 . 6) (6 . 5) (5 . 4) (4 . 3) (3 . 2) (1 . "earfuls"))

;; (alexandria:hash-table-alist table)
;; (("earfuls" . 1) ("earflaps" . 1) ("earflap" . 1) ("eared" . 1)
;;  ("eardrums" . 1) ("earful" . 5) ("eardrum" . 1) ("eardrops" . 1)
;;  ("eardrop" . 4) ("earaches" . 1) ("earache" . 1) ("ear" . 1) ("eaglets" . 1)
;;  ("eaglet" . 1) ("eagles" . 1) ("eagle" . 1) ("eagers" . 1) ("eagerness" . 7)
;;  ("eagerly" . 1) ("eagerest" . 1) ("eagerer" . 1) ("earing" . 5) ("eager" . 1)
;;  ("each" . 1))

(defun pprint-table (data amount)
  (loop :for word :being :each :hash-key :of data
	:for count from 1 to amount
	:do (format t "~A: ~A~%" word (gethash word data))))
		(print word)))

(defun create-frequency-table (file)
  (let ((key-table (make-hash-table :test #'equal))
	(value-table (make-hash-table :test #'equal)))
    (with-open-file (s file :direction :input)
      (do ((line (read-line s nil nil)
		 (read-line s nil nil)))
	  ((null line))
;;	(count-words (uiop/utility:split-string line))))
	(count-words line key-table value-table)))
    (return-from create-frequency-table value-table)))

    (print key-table)
    (print value-table)
    (format t "~%---~%")
    (print (gethash 'earing key-table))
    (print (gethash '5 value-table))
    (format t "~%---~%")
    (pprint-table key-table 5)))

;;; some notes about hash tables
;;; CL-USER> (alexandria:hash-table-values table)
;;; (2 2 5 4)
;;; CL-USER> (alexandria:hash-table-keys table)
;;; (EKOLOGI EKLESTIASTIKMINSTER EK EKOLLON)
;;; CL-USER> (alexandria:hash-table-alist table)
;;; ((EKOLOGI . 2) (EKLESTIASTIKMINSTER . 2) (EK . 5) (EKOLLON . 4))

