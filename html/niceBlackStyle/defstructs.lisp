#|
## Defstruct

### Less is More
One of my design heroes is Deiter Rams, the german 
|#

(unless (fboundp 'aif) (load "defmacro"))

(things
  (defstruct num (n 0) (at 0) (txt "") (mu 0) (m2 0) (lo 1E32) (hi -1E32) (heaven 1))
  (defstruct sym (n 0) (at 0) (txt "") (most 0) mode seen)

(defun make-num(&optional init &key (at 0) (txt " ")) 
  (%make-name :at at :txt txt  
              :heaven (if (eql #\- (char txt (1- (length txt)))) 0 1)))

