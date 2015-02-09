(defun enable-dired-omit-mode () (dired-omit-mode 1))
(add-hook 'dired-mode-hook 'enable-dired-omit-mode)

(defadvice recover-session (around disable-dired-omit-for-recover activate)
  (let ((dired-mode-hook dired-mode-hook))
	(remove-hook 'dired-mode-hook 'enable-dired-omit-mode)
	ad-do-it))

(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-mode)
	(if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
		(progn
		  (set (make-local-variable 'dired-dotfiles-show-p) nil)
		  (message "h")
		  (dired-mark-files-regexp "^\\\.")
		  (dired-do-kill-lines))
	  (progn (revert-buffer) ; otherwise just revert to re-show
			 (set (make-local-variable 'dired-dotfiles-show-p) t)))))

(defvar v-dired-omit t
  "If dired-omit-mode enabled by default. Don't setq me.")
(defun dired-omit-switch ()
  "This function is a small enhancement for `dired-omit-mode', which will
   \"remember\" omit state across Dired buffers."
  (interactive)
  (if (eq v-dired-omit t)
	  (setq v-dired-omit nil)
	(setq v-dired-omit t))
  (dired-omit-caller)
  (revert-buffer))

(defun dired-omit-caller ()
  (if v-dired-omit
	  (setq dired-omit-mode t)
	(setq dired-omit-mode nil)))

(define-key dired-mode-map (kbd "C-x M-o") 'dired-omit-switch)
(add-hook 'dired-mode-hook 'dired-omit-caller)

;; (global-dired-hide-details-mode -1)
