(unless (fboundp 'aif) (load "defmacro"))

(defvar *settings*
  '((about "cutr"
           ("cutr: to understand 'it',  cut 'it' up, then seek patterns in"
            "the pieces. E.g. here we use cuts for multi- objective,"
            "semi- supervised, rule-based explanation."
            "(c) Tim Menzies <timm@ieee.org>, BSD-2 license"
            ""))
    (bins      "initial number of bins"     16)
    (bootstrap "bootstraps"                 256)
    (cliffs    "nonparametric small delta"  .147)
    (cohen     "parametric small delta"     .35)
    (eg        "start up action"            help)
    (file      "read data file"             "../data/auto93.csv")
    (help      "show help"                  nil)
    (seed      "random number seed"         1234567891)
    (min       "min size"                   .5)
    (rest      "exapansion best to rest"    3)
    (top       "top items to explore"       10)
    (want      "optimization goal"          plan)))
;----------------------------------------------------
(defmacro ? (x) `(caddr (assoc ',x *settings*)))

(defun args ()
  #+sbcl sb-ext:*posix-argv*
  #+clisp ext:*args*)

(defun cli (lst)
  (loop for (flag help b4) in lst collect 
    (list flag help (aif (member (format nil "--~(~a~)" flag) (args) :test #'equalp)
                      (typecase b4
                        (number (read-from-string (second it)))
                        (string (second it))
                        (t      (not b4)))
                      b4)))) ; no update

(defun show-help ()
  (format t "~%~{~a~%~}OPTIONS:~%" (? about))
  (dolist (x (cdr *settings*)) 
    (format t "  --~(~10a~) ~a~%"  (first x) (second x))))
;------------------------------------------------------------------------------------
(setf *settings* (cli *settings*))
(if (? help) (show-help))
