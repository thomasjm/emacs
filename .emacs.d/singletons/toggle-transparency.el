;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))

(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

(eval-when-compile (require 'cl))

(defun toggle-transparency ()
  (interactive)
  (if (/=
	   (cadr (frame-parameter nil 'alpha))
	   100)
	  (set-frame-parameter nil 'alpha '(100 100))
	(set-frame-parameter nil 'alpha '(90 90))))
