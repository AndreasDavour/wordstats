;;;; Problem: talking a list of words, and producing a list of how common they are

(defparameter wordlist '(ekollon ekollon ek eklestiastikminster ekologi))

(defun count-words (words)
;;   (dolist (w words)
;;     ;; if there's no hash value, put in 1, else add one
;;     (print (nth-value 0 (gethash w table)))))

    (cond 
      
      ((numberp (nth-value 0 (gethash words table)))
       (setf (gethash words table) (+ (nth-value 0 (gethash words table))  1)))
((null (nth-value 0 (gethash words table)))
       (setf (gethash words table) 1))
      ))
      
      ((numberp (nth-value 0 (gethash words table)))
       (format t "number is true~%")
       (setf (gethash words table) (+ (gethash words table) 1)))
      ((not (numberp (nth-value 0 (gethash words table))))
       (format t "number is not true~%")
       (setf (gethash words table) 1))))

(defun pprint-table (data)
;;  (describe table)
  (loop :for word :being :each :hash-key :of data
	:do (format t "~A: ~A~%" word (gethash word data))))
		(print word)))

(defun create-frequency-table (file)
  (let ((table (make-hash-table :test #'equal))
	(tmp))
    (with-open-file (s file :direction :input)
      (do ((line (read-line s nil nil)
		 (read-line s nil nil)))
	  ((null line))

	(setf tmp (nth-value 0 (gethash line table)))

 	(if tmp
	    (incf (gethash line table))
	    (setf (gethash line table) 1))))
;;;; 	   (setf (gethash line table)
;;;; 		 (+ (nth-value 0 (gethash line table))  1))
;;;; 	   (setf (gethash line table) 1))))

;;	(count-words (uiop/utility:split-string line))))
;;	(cond
;;	  ((null (nth-value 0 (gethash line table)))
;;	   (setf (gethash line table) 1))
;;	  ((numberp (nth-value 0 (gethash line table)))
;;	   (setf (gethash line table)
;;		 (+ (nth-value 0 (gethash line table))  1))))))
    (pprint-table table)))
