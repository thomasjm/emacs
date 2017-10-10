(defun iswitchb-local-keys ()
  (mapc (lambda (K)
	      (let* ((key (car K)) (fun (cdr K)))
			(define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	    '(("<right>" . iswitchb-next-match)
	      ("<left>"  . iswitchb-prev-match)
	      ("<up>"    . ignore             )
	      ("<down>"  . ignore             ))))
(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)


;; Shell config
(defun shell ()
  (interactive)
  (ansi-term "/bin/bash"))
 ;;; Make shell emacs
(setq shell-file-name "/bin/bash")
;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

;; Push mark when using ido-imenu
(defvar push-mark-before-goto-char nil)
(defadvice goto-char (before push-mark-first activate)
  (when push-mark-before-goto-char
    (push-mark)))
(defun ido-imenu-push-mark ()
  (interactive)
  (let ((push-mark-before-goto-char t))
    (ido-imenu)))

;; ?
(put 'dired-find-alternate-file 'disabled nil)
