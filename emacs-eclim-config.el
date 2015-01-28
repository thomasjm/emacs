;; TODO: Make the below a hook that happens when emacs-eclim activates
(when nil
  (progn
	;; Compilation error messages
	(setq help-at-pt-display-when-idle t)
	(setq help-at-pt-timer-delay 0.1)
	(help-at-pt-set-timer)

	;; add the emacs-eclim source
	(require 'ac-emacs-eclim-source)
	(ac-emacs-eclim-config)

	;; (require 'company)
	;; (require 'company-emacs-eclim)
	;; (company-emacs-eclim-setup)
	;; (global-company-mode t)
	))
