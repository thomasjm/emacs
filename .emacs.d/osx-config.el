;; ;;; Path adjustment for OS X
;; (if (eq system-type 'darwin)
;;     ;; from http://stackoverflow.com/questions/2266905/emacs-is-ignoring-my-path-when-it-runs-a-compile-command
;;     (defun set-exec-path-from-shell-PATH ()
;;       (let ((path-from-shell
;; 	     (replace-regexp-in-string “[[:space:]\n]*$” “”
;; 					(shell-command-to-string “$SHELL -l -c ‘echo $PATH’”))))
;; 	(setenv “PATH” path-from-shell)
;; 	(setq exec-path (split-string path-from-shell path-separator)))))

;; Tell python-mode to use the right python
(when (equal system-type 'darwin)
  (setq python-shell-interpreter "/usr/bin/python"))
