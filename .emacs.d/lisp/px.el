
(defun trim-string (string)
  "Remove white spaces in beginning and ending of STRING.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10)."
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" string))
  )

(defun say-done (process event)
  (message (format "PXFE make: %s" (trim-string event))))

(defun my-runmake ()
  (interactive)
  (let
      ( (default-directory "/Users/tom/code/pxfe/dist") )
    (with-current-buffer (get-buffer-create "*make output*")
      (message "Starting PXFE build...")
      (erase-buffer)
      ;; (display-buffer (current-buffer) t))
      (buffer-disable-undo)
      (start-process "pxfe-make-process" "*make output*" "/usr/bin/make"
					 "hep"
					 "mode=notest"
					 )
      (set-process-sentinel (get-process "pxfe-make-process") 'say-done)
      (buffer-enable-undo))))


(defun px-run-current-test ()
  (interactive)
  (let
      ( (default-directory "/Users/tom/code/pxfe/dist")
		(output-buffer-name (format "*px test %s *" (buffer-name)))
		(path (substring buffer-file-name (string-match "px/" buffer-file-name)))
		)
    (with-current-buffer (get-buffer-create output-buffer-name)
      (message "Starting PXFE build...")
	  (message "File name: ")
	  (message path)
      (erase-buffer)
      (display-buffer (current-buffer) t))
      (buffer-disable-undo)
      (start-process "pxfe-test-process" output-buffer-name "/usr/bin/make"
					 "hep"
					 (format "tests=%s" path)
					 )
      (buffer-enable-undo)))



(defun say-done2 (process event)
  (message (format "SETUP_GUIDE make: %s" (trim-string event))))

(defun my-runmake2 ()
  (interactive)
  (let
      ( (default-directory "/Users/tom/code/pxfe/dist") )
    (with-current-buffer (get-buffer-create "*make output*")
      (message "Starting SETUP_GUIDE build...")
      (erase-buffer)
      (buffer-disable-undo)
      (start-process "pxfe-make-process" "*make output*" "/usr/bin/make"
					 "setup_guide"
					 "mode=notest"
					 )
      (set-process-sentinel (get-process "pxfe-make-process") 'say-done2)
      (buffer-enable-undo))))


(global-set-key (kbd "C-M-m") 'my-runmake2)

;; Useful keystroke to run make on pxfe
(if (eq system-type 'darwin)
	(progn
	  (global-set-key (kbd "C-m") 'my-runmake)
	  (global-set-key (kbd "C-c C-m") 'px-run-current-test)))


(defun process-first-sets ()
  (interactive)

  (beginning-of-buffer)
  (insert "{\"")

  (end-of-buffer)
  (insert "\"]}")

  (beginning-of-buffer)
  (while (search-forward "\t" nil t)
	(replace-match "\": [\"" nil t))

  (beginning-of-buffer)
  (while (search-forward ", " nil t)
	(replace-match "\", \"" nil t))

  (beginning-of-buffer)
  (while (search-forward "\n" nil t)
	(replace-match "\"], \"" nil t)))

(defun change (orig new start end)
  (progn
	(goto-char start)
	(while (search-forward orig end t)
	  (replace-match new nil t))))

(defun px-transform-comparators ()
  (interactive)
  (let ((start (region-beginning))
		(end (region-end))
		(f (lambda (orig new) (change orig new start end))))
	(save-excursion
	  (funcall f "\"lt\"" "\"<\"")
	  (funcall f "\"gt\"" "\">\"")
	  (funcall f "\"leq\"" "\"<=\"")
	  (funcall f "\"geq\"" "\">=\"")
	  )))
