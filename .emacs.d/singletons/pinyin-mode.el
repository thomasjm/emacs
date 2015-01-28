
(setq accent-letters '(("a" . ("ā" "á" "ǎ" "à"))
					   ("e" . ("ē" "é" "ě" "è"))
					   ("i" . ("ī" "í" "ǐ" "ì"))
					   ("o" . ("ō" "ó" "ǒ" "ò"))
					   ("u" . ("ū" "ú" "ǔ" "ù"))))

(defun pinyin-process (n)
  (let ((c (string (char-before))))
	(delete-backward-char 1)
	(insert (nth (- n 1) (cdr (assoc c accent-letters))))))

(define-minor-mode pinyin-mode
  "Toggle Pinyin mode.
     Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state. TODO: add more documentation"
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Pinyin"
  ;; The minor mode bindings.
  '(
	((kbd "1") . (lambda () (interactive) (pinyin-process 1)))
	((kbd "2") . (lambda () (interactive) (pinyin-process 2)))
	((kbd "3") . (lambda () (interactive) (pinyin-process 3)))
	((kbd "4") . (lambda () (interactive) (pinyin-process 4)))
	)
  :group 'pinyin)
