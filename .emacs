;; Uncomment these lines to run benchmarks on Emacs initialization
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
;; ibuffer is awesome
(defalias 'list-buffers 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)

;;; Load path stuff
(add-to-list 'load-path "~/.emacs.d/lisp") ;; configs and elisp packages I've written
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

;; delete-trailing-whitespace before every save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Load packages
(add-hook 'after-init-hook (lambda ()
                             (load "emacs_packages.el")

                             ;; Apply theme
                             (load-theme 'afternoon t)))

;; Custom custom file
(setq custom-file "~/.emacs.d/lisp/emacs_custom.el")
(load custom-file)

(global-set-key (kbd "M-h") '(lambda () nil))
(global-set-key [(super h)] '(lambda () nil))

;; (benchmark-init/deactivate) ;; Uncomment to benchmark Emacs initialization

;; Useful for debugging startup problems
;; (toggle-debug-on-error 1)
(put 'upcase-region 'disabled nil)
