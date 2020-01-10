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

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(menu-bar-mode -1)

;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

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
(setq dired-listing-switches "-alh")
;; ibuffer is awesome
(defalias 'list-buffers 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)

;;; Load path stuff
(add-to-list 'load-path "~/.emacs.d/lisp") ;; configs and elisp packages I've written
(add-to-list 'load-path "~/.emacs.d/singletons") ;; Emacs packages that were downloaded separately and come as single files

(setenv "PATH"
  (concat
   "/home/tom/.yarn/bin" ":"
   (getenv "PATH")
  )
)

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
;; (remove-hook 'before-save-hook 'delete-trailing-whitespace)

(message "Before emacs_packages")

;; Load packages
;; (add-hook 'after-init-hook (lambda ()
                             (load "emacs_packages.el")

                             ;; Apply theme
                             (load-theme 'afternoon t)
                             ;; ))

(message "Before custom file")

;; Custom custom file
(setq custom-file "~/.emacs.d/lisp/emacs_custom.el")
(load custom-file)

(global-set-key (kbd "M-h") '(lambda () nil))
(global-set-key [(super h)] '(lambda () nil))

;; (benchmark-init/deactivate) ;; Uncomment to benchmark Emacs initialization

(setq projectile-enable-caching t)

;; For some reason this key in keybindings.el is being overridden or something on Linux, so putting it here
(global-set-key (kbd "M-h") 'backward-kill-word)

(message "Before loading db.el")


(load "db.el")

(message "Before tide setq")

(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

; try to improve slow performance on windows.
(setq w32-get-true-file-attributes nil)

(setq ring-bell-function 'ignore)

; Set up TLS security
; https://glyph.twistedmatrix.com/2015/11/editor-malware.html#fnref:4
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile))))

;; Useful for debugging startup problems
;; (toggle-debug-on-error 1)
(put 'upcase-region 'disabled nil)

;; Change default font size
(set-face-attribute 'default nil :height 150)
