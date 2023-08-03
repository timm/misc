(unless (fboundp 'rand) (load "lib"))

(defun run (fun)
  (let ((saved (copy-tree *settings*)))
    (srand (? seed))
    (prog1
      (funcall fun)
      (setf *settings* (copy-tree saved)))))

(defun egs ()
  (do-all-symbols (sym)  
    (let ((str (symbol-name sym)))
      (if (fboundp sym) 
        (if (equalp "eg-" (subseq str 0 (min 3 (length str))))
          (push (cons str sym) out)))))
  (mapcar #'cdr (sort out #'string< :key #'car)))

(defun runs () (loop for sym in (egs) sum (run sym)))

(defun eg-aa  () (> .10 (/ 10 10 ))) ; error is true

(defun bad(oops); if crash, return 1. if oops returns non-nil, return 1
  (ignore-errors (return-from bad (if (funcall oops) 1 0)))
  1)

