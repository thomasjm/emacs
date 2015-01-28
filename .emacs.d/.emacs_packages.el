;; (let ((benchmark-init.el "/Users/tomm/tools/benchmark-init-el/benchmark-init.el")
;; 	  (benchmark-init-modes.el "/Users/tomm/tools/benchmark-init-el/benchmark-init-modes.el"))
;;   (when (file-exists-p benchmark-init.el)
;;     (load benchmark-init.el)
;; 	(load benchmark-init-modes.el)
;; 	))
;; (require 'benchmark-init)
;; (benchmark-init/activate)

(require 'projectile)
(projectile-global-mode)

;; Perspective
(require 'perspective)
(persp-mode)
(require 'persp-projectile)
(define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project) ;; TODO: move to keybindings file

;; Dirtree
(autoload 'dirtree "dirtree" "Dirtree thing" t)

;; JS linter
(add-to-list 'load-path "~/.emacs.d/jshint-mode")
(require 'flymake-jshint)
(add-hook 'javascript-mode-hook
		  (lambda () (flymake-mode t)))

;; Dash
(add-to-list 'load-path "~/.emacs.d/dash-at-point")
(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)

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
;; (global-set-key [(meta x)] (lambda ()
;;                              (interactive)
;;                              (or (boundp 'smex-cache)
;;                                  (smex-initialize))
;;                              (global-set-key [(meta x)] 'smex)
;;                              (smex)))

;; (global-set-key [(shift meta x)] (lambda ()
;;                                    (interactive)
;;                                    (or (boundp 'smex-cache)
;;                                        (smex-initialize))
;;                                    (global-set-key [(shift meta x)] 'smex-major-mode-commands)
;;                                    (smex-major-mode-commands)))

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
(require 'clojure-mode)
(eval-after-load 'clojure
  (define-key clojure-mode-map (kbd "C-;") 'compile-site))
(add-hook 'clojure-mode-hook
          (lambda () (local-set-key (kbd "C-;") 'compile-site)))

(require 'thingatpt)
(define-key isearch-mode-map (kbd "C-*")
  (lambda ()
    "Reset current isearch to a word-mode search of the word under point."
    (interactive)
    (setq isearch-word t
          isearch-string ""
          isearch-message "")
    (isearch-yank-string (word-at-point))))

;; Speedbar
; Lazy loading trick
(defun speedbar-lazy-load ()
  (interactive)
  (require 'sr-speedbar)
  (require 'speedbar)
  (defconst my-speedbar-buffer-name " SPEEDBAR")

  (speedbar-add-supported-extension ".hs")

  (global-set-key [(kbd "<f9>")] 'sr-speedbar-toggle)
  (global-set-key (kbd "C-{") 'sr-expand-buffer-functions-in-speedbar))

(global-set-key [(kbd "<f9>")] 'speedbar-lazy-load)


;; (defconst my-speedbar-buffer-name " SPEEDBAR")
;; (autoload 'sr-speedbar-toggle "sr-speedbar")
;; (require 'sr-speedbar)
;; (require 'speedbar)

;; WLR
(require 'whole-line-or-region)
(whole-line-or-region-mode)

;; Impatient mode
;; (add-to-list 'load-path "~/.emacs.d/impatient-mode")
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

;; use ipython in python-mode
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; In python-mode, flip the send-file and send-buffer hotkeys (C-c C-l is usually send-buffer)
(add-hook 'python-mode-hook
          (lambda ()
            (progn
              (define-key python-mode-map "\C-c\C-c" 'python-shell-send-file)
              (define-key python-mode-map "\C-c\C-l" 'python-shell-send-buffer)
              )))

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
(require 'dired+)
(setq dired-omit-mode t)
(load "dired_config")

;; Options for Dired+
(toggle-diredp-find-file-reuse-dir 1)

;;; Haskell setup
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
;; (eval-after-load 'flycheck
;;   '(require 'flycheck-hdevtools))
;; ghc-mod
(setq haskell-program-name "cabal repl") ;; important
(add-to-list 'load-path "~/.emacs.d/ghc-mod")

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
;; tags on save
(setenv "PATH" (concat "~/.cabal/bin:" (getenv "PATH")))
(add-to-list 'exec-path "~/.cabal/bin")
(custom-set-variables '(haskell-tags-on-save t))
;; customization
(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-ode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
;; GHC
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;; hide-lines
(load "hide-lines.el")

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; CIDER
(setq cider-auto-select-error-buffer nil)
(setq nrepl-error-buffer-select nil)
(add-hook 'clojure-mode-hook (lambda ()
                               (load "my-smartparens-config")
                               ;; (define-key cider-mode-map (kbd "<M-up>") 'sp-splice-sexp-killing-backward)
                               (define-key cider-mode-map "\C-c\C-k" 'cider-load-file)
                               (define-key cider-mode-map "\C-c\C-l" 'cider-load-buffer)))


;; clj-refactor
(add-hook 'clojure-mode-hook (lambda ()
                               (require 'clj-refactor)
                               (clj-refactor-mode 1)
                               (rainbow-delimiters-mode)
                               ;; insert keybinding setup here
                               (cljr-add-keybindings-with-prefix "C-c C-j")
                               ))

;; Chinese stuff
(load "toggle-case.el")
(autoload 'pinyin-mode "pinyin-mode.el" "Pinyin mode" t)

;; Windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; purescript
(add-hook 'purescript-mode-hook (lambda ()
                                  (require 'repl-toggle)
                                  (require 'psci)
                                  (add-to-list 'rtog/mode-repl-alist '(purescript-mode . psci))
                                  (inferior-psci-mode)))

;; git-gutter
(global-git-gutter-mode)

;; toggle-transparency
(load "toggle-transparency")

;; Hipchat config
(load "hipchat_config.el")

;; (benchmark-init/deactivate)
