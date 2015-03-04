(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
						   (buffer-substring (region-beginning) (region-end))
						 (read-string "Google: "))))))

(defun eval-next-sexp ()
  (interactive)
  (save-excursion
    (forward-sexp)
    (eval-last-sexp nil)))

;; Don't use this, just use eval-defun (C-M-x)
;; (defun eval-surrounding-sexp (levels)
;;   (interactive "p")
;;   (save-excursion
;;     (up-list (abs levels))
;;     (eval-last-sexp nil)))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; org-mode
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)


(defun despacify ()
  (while (re-search-forward "[[:space:]]*" (region-end) t)
    (replace-match "" nil nil)))

(defun despacify (start end)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (let ((regexp "[[:space:]\n\t]")
			(to-string ""))
		(narrow-to-region start end)
		(goto-char (point-min))
		(while (re-search-forward regexp nil t)
		  (replace-match to-string nil nil))))))


(defun replace-regexp-in-region (start end)
  (interactive "*r")
  (save-restriction
    (let ((regexp (read-string "Regexp: "))
		  (to-string (read-string "Replacement: ")))
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
		(replace-match to-string nil nil)))))

(defun replace-regexp-in-region-specific (start end regexp to-regexp)
  (interactive "*r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (replace-match to-regexp nil nil))))

;; Re-builder thing
(defun reb-query-replace (to-string)
  "Replace current RE from point with `query-replace-regexp'."
  (interactive
   (progn (barf-if-buffer-read-only)
		  (list (query-replace-read-to (reb-target-binding reb-regexp)
									   "Query replace"  t))))
  (with-current-buffer reb-target-buffer
    (query-replace-regexp (reb-target-binding reb-regexp) to-string)))

(defun replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

(defun new-scratch ()
  "open up a guaranteed new scratch buffer"
  (interactive)
  (switch-to-buffer (loop for num from 0
						  for name = (format "scratch-%03i" num)
						  while (get-buffer name)
						  finally return name)))

;; For building parser tests on pxfe frontend
(defun process-raw-tests (&optional start end)
  (interactive "r")
  (shell-command-on-region start end "python ~/tmp/generate_tests.py" (current-buffer) t)
  (beginning-of-buffer)
  (replace-regexp "\\$\"" "")
  (beginning-of-buffer)
  (replace-regexp "\"\\$" "")

  (beginning-of-buffer)
  (replace-regexp "\"undefined\"" "undefined")

  )

;; Scheme mode
(defun mechanics ()
  (interactive)
  (run-scheme
   "/usr/local/scmutils/mit-scheme/bin/scheme --library /usr/local/scmutils/mit-scheme/lib"
   ))

(defun zap-up-to-char (arg char)
  "Kill up to and including ARGth occurrence of CHAR.
 Case is ignored if `case-fold-search' is non-nil in the current buffer.
 Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
					 (read-char "Zap to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
		(setq char (or (aref translation-table-for-input char) char))))
  (kill-region (point) (progn
						 (search-forward (char-to-string char) nil nil arg)
						 (backward-char)
						 (point))))

;; Node js eval region or buffer
(defun node-js-eval-region-or-buffer ()
  "Evaluate the current buffer (or region if mark-active),
    and return the result into another buffer,
    which is to be shown in a window."
  (interactive)
  (let ((debug-on-error t) (start 1) (end 1))
    (cond
     (mark-active
      (setq start (point))
      (setq end (mark)))
     (t
      (setq start (point-min))
      (setq end (point-max))))
    (call-process-region
     start end     ; seems the order does not matter
     "node"        ; node.js
     nil           ; don't delete region
     "node.js"     ; output buffer
     nil)          ; no redisply during output
    (message "Region or buffer evaluated!")
    (setq deactivate-mark t))) ; deactive the region, regardless

;; Fancy regexp to add console.log to js functions
(defun log-js-functions (start end)
  (interactive "*r")
  "Make all js functions print out a console.log that they're being called"
  (query-replace-regexp
   "var \\([^[:space:]]*\\)\\( = [^[:space:]]*\\)?\\( = function\(.*\) {\\)"
   "var \\1\\2\\3\nconsole.log(\"FUNCTION: \\1\");"))

;; Date insertion
(defun insert-current-date () (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))
(defun insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%c")))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
 	(delq (current-buffer)
 	      (remove-if-not 'buffer-file-name (buffer-list)))))

;; Open Dropbox folder in dired
(defun open-dropbox ()
  (interactive)
  (dired (cond ((equal system-type 'gnu/linux) "~/Dropbox")
			   ((equal system-type 'darwin) "~/Dropbox-personal/Dropbox"))))

;; Open special emacs config files
(defun open-keybindings ()
  (interactive)
  (find-file "~/.emacs.d/lisp/keybindings.el"))
(defun open-emacs-packages ()
  (interactive)
  (find-file "~/.emacs.d/lisp/emacs_packages.el"))


;; Open the todo.org file
(defun open-todo ()
  (interactive)
  (find-file (cond ((equal system-type 'gnu/linux) "~/Dropbox/todo/todo.org")
				   ((equal system-type 'darwin) "~/Dropbox \(Personal\)/todo/todo.org"))))

;; Open the todo.org file
(defun open-work-todo ()
  (interactive)
  (find-file (cond ((equal system-type 'gnu/linux) "~/Dropbox/todo/work.org")
				   ((equal system-type 'darwin) "~/Dropbox \(Personal\)/todo/work.org"))))

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
				  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

;; It's probably better to just use run-python
(defun run-python-script
  (interactive)
  (compile (concat "python " (buffer-name))))
;; (defun send-python-script ()
;;   "Send a script"
;;   (interactive)
;;   (python-shell-send-file (buffer-name))) ;; Use python-shell-send-buffer

(defun eclim-run-test ()  (interactive)  (if (not (string= major-mode "java-mode"))    (message "Sorry cannot run current buffer."))  (compile (concat eclim-executable " -command java_junit -p " eclim--project-name " -t " (eclim-package-and-class))))

;; Kill the runniing python shell tied to a given python.el buffer.
;; Then start a new python shell and do python-shell-send-buffer
;; Useful because python doesn't automatically reload modules
(defun restart-python-process-and-send-buffer ()
  (interactive)
  (progn
	(let ((process (python-shell-get-process)))
	  (when process (kill-process process)))
	(sit-for 0.3 t) ; This method is also used in python.el
	(run-python (python-shell-parse-command) t t)
	(sit-for 0.3 t)
	(python-shell-send-buffer)))


(defun test ()
  (interactive)
  (message (org-current-level)))

(defun new-org-entry ()
  (interactive)
  (save-excursion
	(progn
	  ;; Jump to the first top-level org item and set the mark
	  (beginning-of-buffer)
	  (search-forward "*")
	  (beginning-of-line)
	  (push-mark)

	  ;; Go to the end of this org item and copy it
	  (save-excursion
		(progn
		  (next-line)

		  ;; Go down until we hit another level 1 entry
		  (while (> (org-current-level) 1)
			(next-line))


		  (previous-line)
		  (kill-ring-save (mark) (point))
		  ))

	  (previous-line)
	  (insert-string "\n")

	  (let ( (beginning-point (point)) )
		(yank)
		(goto-char beginning-point)
		(forward-char)
		(forward-char)
		(kill-line)
		(insert (shell-command-to-string "echo -n $(date +%-m/%-d/%Y)"))))))


(defun comment-dwim (arg)
  (interactive "*P")
  (comment-normalize-vars)
  (if (and mark-active transient-mark-mode)
      (comment-or-uncomment-region (region-beginning) (region-end) arg)
	(comment-or-uncomment-region (line-beginning-position) (line-end-position) arg)))

;; TODO: move the stuff below to a "clojure site development" .el file
(defun compile-site ()
  (interactive)
  ;; TODO: cider-jack-in if necessary
  (cider-interactive-eval "(site.core/-main)"))

;; Change a couple variables before using benchmark-init to show the data
;; Requires that profiling with benchmark-init already happened
(defun show-tree-benchmark ()
  (interactive)
  (progn
	(setq max-lisp-eval-depth 10000)
	(setq max-specpdl-size 10000)
	(benchmark-init/show-durations-tree)))

;; TODO: Make this not keep going backwards through history
(defun cider-repl-run-last ()
  (interactive)
  (save-excursion
	(progn
	  (cider-switch-to-repl-buffer nil) ;; TODO: use cider everywhere
	  ;; (set-buffer "*cider-repl clj*")
	  (end-of-buffer)
	  (cider-repl-previous-input)
	  (cider-repl-return)
	  (cider-repl-next-input)
	  (end-of-buffer))))

(defun open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path is starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
This command is similar to `find-file-at-point' but without prompting for confirmation.
"
  (interactive)
  (let ( (path (if (region-active-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'filename) ) ))
    (if (string-match-p "\\`https?://" path)
        (browse-url path)
      (progn ; not starting “http://”
        (if (file-exists-p path)
            (find-file path)
          (if (file-exists-p (concat path ".el"))
              (find-file (concat path ".el"))
            (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" path) )
              (find-file path )) ) ) ) ) ))

(defun close-and-kill-this-pane ()
  "If there are multiple windows, then close this pane and kill the buffer in it also."
  (interactive)
  (kill-this-buffer)
  (if (not (one-window-p))
	  (delete-window)))

(defun pycallgraph ()
  (interactive)
  (let ((outfile (make-temp-name "pycallgraph")))
	(shell-command (format "pycallgraph --max-depth 10 graphviz --output-file=%s -- %s" outfile (buffer-file-name)))

  (find-file-other-window outfile)))


(defun which-active-modes ()
  "Give a message of which minor modes are enabled in the current buffer."
  (interactive)
  (let ((active-modes))
    (mapc (lambda (mode) (condition-case nil
                             (if (and (symbolp mode) (symbol-value mode))
                                 (add-to-list 'active-modes mode))
                           (error nil) ))
          minor-mode-list)
    (message "Active modes are %s" active-modes)))


(defun elisp-find-definition (name)
  "Jump to the definition of the function (or variable) at point."
  (interactive (list (thing-at-point 'symbol)))
  (cond (name
         (let ((symbol (intern-soft name))
               (search (lambda (fun sym)
                         (let* ((r (save-excursion (funcall fun sym)))
                                (buffer (car r))
                                (point (cdr r)))
                           (cond ((not point)
                                  (error "Found no definition for %s in %s"
                                         name buffer))
                                 (t
                                  (switch-to-buffer buffer)
                                  (goto-char point)
                                  (recenter 1)))))))
           (cond ((fboundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-function-noselect symbol))
                 ((boundp symbol)
                  (elisp-push-point-marker)
                  (funcall search 'find-variable-noselect symbol))
                 (t
                  (message "Symbol not bound: %S" symbol)))))
  (t (message "No symbol at point"))))
