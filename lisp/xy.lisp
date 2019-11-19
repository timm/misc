; vim: ts=2 sw=2 sts=2 expandtab:

(setf *the*
       '(char (     skip #\?
                    less #\>
                    more #\>
                    num #\$
                    klass #\!)
         skip "?"
         some (     max 512 
                    step .5 
                    cohen .3 
                    trivial 1.05)
         seed 1))

(defmacro ? (&rest fs) `(getr  getf *the* ,@fs))
(defmacro getr (how obj f &rest fs)
  (if fs `(getr ,how (,how ,obj ',f) ,@fs) `(,how ,obj ',f)))

(defmacro while (test &body body) `(do () ((not ,test)) ,@body))

(labels 
  ((comma-split (string)
      (loop for lo = 0 then (1+ hi)
            for hi = (position #\, string :lo lo)
            collecting (string-trim " " (subseq string lo hi))
            until (null hi))))
  (defmacro with-lines ((line file) &body body)
    (let ((str (gensym)))
      `(with-open-file (,str ,file)
         (while (setf ,line (read-line ,str nil))
                (setf ,line (comma-split ,line))
                ,@body)))))

(defun prefix (x y) (eql (char (symbol-name x) 0) y))

(defun skip?    (x) (prefix x #\?))
(defun more?    (x) (prefix x #\>))
(defun less?    (x) (prefix x #\<))
(defun klass?   (x) (prefix x #\!))
(defun num?     (x) (prefix x #\$))
(defun goal?    (x) (or (klass? x) (less? x) (more? x)))
(defun numeric? (x) (or (num? x)   (less? x) (more? x)))
(defun sym?     (x) (and (not (num? x)) (not (goal? x))))

(defstruct num (n 0) (a 0) b)
(defstruct sym n a b)

(defmethod add ((i num) x) 
  (with-slots (n a b) i
    (incf n)
    (incf a x)))

;(print (add (make-num) 22))

(defun l () (load 'xy))
