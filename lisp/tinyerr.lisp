# 🧠 Slip: A Clean Lisp Dialect

_Slide past boilerplate. Think clearly. Write less._

**Slip** is a small set of macros layered atop Common Lisp that smooths over the rough spots:  
shorter function definitions, cleaner scoping, and modern function composition — all in under 100 lines.

---

## ✨ Features

### `def` — like `defun`, but smarter
```lisp
(def greet (name (title "Dr.") (:loud nil))
  (format t "~a ~a~%" title name)
  (when loud (format t "LOUD MODE~%")))
