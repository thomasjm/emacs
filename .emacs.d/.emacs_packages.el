(require 'projectile)
(projectile-global-mode)

(autoload 'dirtree "dirtree" "Dirtree thing" t)
  ;; (require 'dirtree)

;; JS linter
(add-to-list 'load-path "~/.emacs.d/jshint-mode")
(require 'flymake-jshint)
(add-hook 'javascript-mode-hook
		  (lambda () (flymake-mode t)))

;; Disable the M-j keybinding in js2-mode
(add-hook 'js2-mode-hook
          (lambda () (define-key js2-mode-map (kbd "M-j") nil)))

;; ;; Hipchat
;; (load "hipchat.el")

;; Emacs-eclim
;; (add-to-list 'load-path "~/.emacs.d/emacs-eclim")
;; (require 'eclim)
;; (require 'eclimd)
;; (global-eclim-mode)

;;; Multiple cursors
;; (require 'multiple-cursors)

;; smex
(smex-initialize)

;; ;; JDEE
;; (add-to-list 'load-path "~/.emacs.d/jdee")
;; (load "jde-int.el")
;; (add-to-list 'load-path "~/.emacs.d/jdee/jdee-2.4.1/lisp")
;; (load "jde")

;; (require 'semantic/ia)
;; (semantic-mode 1)
;; (setq global-semantic-tag-folding-mode 1)
;; (put 'narrow-to-region 'disabled nil)

;; Clojure
;; (require 'clojure-mode)
;; (eval-after-load 'clojure
;;   (define-key clojure-mode-map (kbd "C-;") 'compile-site))
;; (add-hook 'clojure-mode-hook
;;           (lambda () (local-set-key (kbd "C-;") 'compile-site)))

(require 'thingatpt)
(define-key isearch-mode-map (kbd "C-*")
  (lambda ()
    "Reset current isearch to a word-mode search of the word under point."
    (interactive)
    (setq isearch-word t
          isearch-string ""
          isearch-message "")
    (isearch-yank-string (word-at-point))))

(require 'sr-speedbar)
(require 'speedbar)
(defconst my-speedbar-buffer-name " SPEEDBAR")
;(defconst my-speedbar-buffer-name "SPEEDBAR")

;; WLR
(require 'whole-line-or-region)
(whole-line-or-region-mode)

;; Impatient mode
;; (require 'impatient-mode)
;; (load "impatient-mode-stuff")

;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)

;; Dockblockr style comments for Javascript
(require 'js-doc)
(global-set-key (kbd "C-5") 'js-doc-insert-function-doc)

;; Semantic stuff
(add-to-list 'semantic-default-submodes
			 'global-semanticdb-minor-mode
			 'global-cedet-m3-minor-mode)
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

;; parscope-mode
(require 'parscope)

;; Turn on elpy in python mode
(add-hook 'python-mode-hook
		  (lambda () (elpy-mode)))

			  ;; (local-set-key (kbd "M-.") 'rope-goto-definition)
			  ;; (define-key elpy-mode-map (kbd "M-.") 'rope-goto-definition)
			;; (define-key python-mode-map [M-.] 'rope-goto-definition)))
		  ;; (lambda () (elpy-mode)))
;; (eval-after-load 'elpy-mode
;;   '(progn
;; 	 (message "LOADING ELPY MODE")))
;; 	 ;; (define-key elpy-mode-map (kbd "M-.") 'rope-goto-definition)))

;; (defun special-python-keys ()
;;   (message "LOADING ELPY KEYS")
;;   (define-key elpy-mode-map (kbd "M-.") 'rope-goto-definition))

;; (add-hook 'python-mode-hook
;; 		  'special-python-keys)



(setq py-load-pymacs-p nil)

(defun load-ropemacs ()
  "Load pymacs and ropemacs"
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  ;; Automatically save project python buffers before refactorings
  (setq ropemacs-confirm-saving 'nil)

  (define-key ropemacs-local-keymap (kbd "M-/") nil)
)
(global-set-key "\C-xpl" 'load-ropemacs)


;; eimp
(autoload 'eimp-mode "eimp" "Emacs Image Manipulation Package." t)
(add-hook 'image-mode-hook 'eimp-mode)

;; flymake-hlint
;; (eval-after-load 'haskell
;;   (flymake-hlint-load))
;; (add-hook 'haskell-mode-hook 'flymake-hlint-load)

;; Clean modeline
(load "clean-modeline")

;; Dired X (allows you to omit uninteresting files from dired)
;; (add-hook 'dired-load-hook '(lambda () (require 'dired-x)))
(require 'dired-x)
(setq dired-omit-mode t)
(load "dired_config")

;; Persist clocks
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(eval-after-load 'flycheck
  '(require 'flycheck-hdevtools))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)


;; Nunjucks files
(add-to-list 'auto-mode-alist '("\\.nunjucks\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode))

;; TSX
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
