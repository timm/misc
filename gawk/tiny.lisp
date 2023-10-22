(defstruct opt
  (file "auto93.lisp"))

(defmacro freq (x lst &optional (init 0))
  `(cdr (or (assoc ,x ,lst :test #'equal)
            (car (setf ,lst (cons (cons ,x ,init) ,lst))))))

(defmacro o (s x &rest xs)
  (if xs ‘(o (slot−value ,s ’,x) ,@xs) ‘(slot−value ,s ’,x)))

(defstruct cols (x y all names))
(defstruct data (row cols))
(defstruct sym (at 0) (txt " ") (n 0) counts)
(defstruct num (at 0) (txt " ") (n 0) (heaven 1) (lo 1E30) (hi -1E30))

(defstruct col0 (&optional (at 0) (txt " "))
  (if (upper-case-p (prefix txt))
    (make-num :at at :txt txt :heaven (if (eq #\- (suffix txt)) 0 1))
    (make-syn :at at :txt txt)))

(defmethod add ((num1 num) x)
  (with-slots (mu lo hi n) num1
    (unless (eq x '?)
      (incf n)
      (setf  lo (min lo x)
             hi (max hi x)
             mu (+ mu (/ (- x  mu) n))))))

(defmethod add ((sym1 sym) x)
  (with-slots (n counts) sym1
    (unless (eq x '?)
      (incf n)
      (incf (freq x counts)))))

(defun cols0 (lst &aux (c -1) (cols1 (make-cols :names lst)))
  (with-slots (all x y) cols1
    (dolist (txt lst cols1)
      (let ((col1 (col0 (incf c) txt)))
        (push all col1)
        (unless (eq #\M (suffix (o col1 txt)))
          (case  (suffix (o col1 txt))
            (#\+ (push y col1))
            (#\- (push y col1))
            (t   (push x col1))))))))

(defun data0
(defun reads (f)
  (with-open-file (stream f) (read stream)))

(defun prefix (x y) 
  (elt (symbol-name x) 0))

(defun suffix (x y) 
  (let* ((s (symbol-name x))
      (n (length s)))
     (elt s (1- n))))
