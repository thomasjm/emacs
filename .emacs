;; (let ((benchmark-init.el "/Users/tomm/tools/benchmark-init-el/benchmark-init.el")
;; 	  (benchmark-init-modes.el "/Users/tomm/tools/benchmark-init-el/benchmark-init-modes.el"))
;;   (when (file-exists-p benchmark-init.el)
;;     (load benchmark-init.el)
;; 	(load benchmark-init-modes.el)
;; 	))
;; (require 'benchmark-init)
;; (benchmark-init/activate)

;; Do this to prevent package-initialize from being called. Can speed up Emacs starting.
;; (setq package-enable-at-startup nil)


;;; General settings
(setq mac-option-modifier 'control)
(setq mac-command-modifier 'meta)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; Automatically refresh buffers when changed on disk, as long as they're not modified
(global-auto-revert-mode t)
;; Don't show scrollbars
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
;; When you have a selected region and start typing, overwrite
(delete-selection-mode 1)
;; ido-mode integrates well with Projectile
(ido-mode t)
;; ibuffer is awesome
(defalias 'list-buffers 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)

;;; Load path stuff
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/singletons") ;; Emacs packages that were downloaded separately and come as single files
;; Misc functions
(load "misc-functions.el")
;; Keybindings
(load "keybindings.el")
;; Platform-specific stuff
(when (equal system-type 'gnu/linux)
  (load "linux-config.el"))
(when (equal system-type 'darwin)
  (progn
    (load "osx-config.el")
    ; Dropbox-specific stuff
    (load "db.el")))

;; Never use tabs
(load "never-use-tabs.el")

;; Theming
'(custom-enabled-themes (quote (wombat)))

;; Random hooks
'(haskell-mode-hook (quote (turn-on-haskell-indentation)))
(setq auto-mode-alist
      (append
       ;; File name (within directory) starts with a dot.
       '(
		 ("\\.js\\'" . js2-mode)
		 )
       auto-mode-alist))

;; delete-trailing-whitespace before every save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Push mark when using ido-imenu
(defadvice ido-imenu (before push-mark activate)
  (push-mark))

;; Org mode setup
(setq org-todo-keywords '((sequence "TODO" "IN PROGRESS" "IN REVIEW" "BLOCKED" "|" "WONTDO" "DONE")))
;; MobileOrg setup
(setq org-directory "~/Dropbox/todo")
(setq org-mobile-inbox-for-pull "~/Dropbox/todo/flagged.org") ;; New notes
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; node
(setq inferior-js-program-command "node --interactive")

;; Load packages
(add-hook 'after-init-hook (lambda () (load ".emacs_packages.el")))

;; Custom custom file
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file)

;; (benchmark-init/deactivate)
(put 'downcase-region 'disabled nil)

;; Useful for debugging startup problems
;; (toggle-debug-on-error 1)
