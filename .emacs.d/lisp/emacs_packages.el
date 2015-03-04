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
;; (require 'persp-projectile)
;; (define-key projectile-mode-map (kbd "s-s") 'projectile-persp-switch-project) ;; TODO: move to keybindings file

;; Dirtree
(autoload 'dirtree "dirtree" "Dirtree thing" t)

;; JS linter
(add-to-list 'load-path "~/.emacs.d/jshint-mode")
;; (require 'flymake-jshint)
;; (add-hook 'javascript-mode-hook
;; 		  (lambda () (flymake-mode t)))

;; JS
(setq auto-mode-alist
      (append
       ;; File name (within directory) starts with a dot.
       '(
		 ("\\.js\\'" . js2-mode)
		 )
       auto-mode-alist))

;; Dockblockr style comments for Javascript
(require 'js-doc)
(global-set-key (kbd "C-5") 'js-doc-insert-function-doc)

;; node
(setq inferior-js-program-command "node --interactive")


;; Dash
(add-to-list 'load-path "~/.emacs.d/dash-at-point")
(autoload 'dash-at-point "dash-at-point"
  "Search the word at point with Dash." t nil)

;; Emacs-eclim
;; (add-to-list 'load-path "~/.emacs.d/emacs-eclim")
;; (require 'eclim)
;; (require 'eclimd)
;; (global-eclim-mode)

;;; Multiple cursors
;; (require 'multiple-cursors)

;; smex
;; (smex-initialize) ;; disabled since we use helm now

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

;; Thing at point
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

;; auto-complete
;; (require 'auto-complete-config)
;; (ac-config-default)
;; company-mode
(global-company-mode)
;; (add-to-list 'company-backends 'company-ghc)
(add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

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

;; Org mode setup
(setq org-todo-keywords '((sequence "TODO" "IN PROGRESS" "IN REVIEW" "BLOCKED" "|" "WONTDO" "DONE")))
;; MobileOrg setup
(setq org-directory "~/Dropbox/todo")
(setq org-mobile-inbox-for-pull "~/Dropbox/todo/flagged.org") ;; New notes
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-default-notes-file (concat org-directory "/notes.org"))


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
(add-hook 'haskell-mode-hook 'global-flycheck-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
(eval-after-load 'flycheck
  '(require 'flycheck-ghcmod))
;; (eval-after-load 'flycheck
;;   '(require 'flycheck-hdevtools)) ;; hdevtools is poorly maintained/doesn't understand Cabal sandboxes
;; ghc-mod
(setq haskell-program-name "cabal repl") ;; important
(add-to-list 'load-path "~/.emacs.d/ghc-mod")

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
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c <f17>") 'haskell-interactive-bring) ;; Used for keyremappings on Cocoa emacs
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
  (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)

  (define-key haskell-mode-map (kbd "C-x C-s")
    (lambda ()
      (interactive)
      (save-excursion
        (beginning-of-buffer)
        ;; Sort the initial import block
        (when (search-forward "import" nil t)
          (beginning-of-line)
          (haskell-sort-imports)
          )

        ;; Sort any following import blocks
        (while (search-forward "

import" nil t)
          (beginning-of-line)
          (haskell-sort-imports)
          ))

      (save-buffer)))
  ))
(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-ode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
;; GHC
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook #'hindent-mode)
(eval-after-load 'haskell-mode
  `(define-key haskell-mode-map
     (kbd "C-c C-d")
     #'ghc-imported-from-haddock-for-symbol-at-point))

;; hide-lines
(load "hide-lines.el")

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; smartparens (way better than paredit)
(load "my-smartparens-config")

;; CIDER
(setq cider-auto-select-error-buffer nil)
(setq nrepl-error-buffer-select nil)
(add-hook 'clojure-mode-hook (lambda ()
                               ;; (load "my-smartparens-config")
                               ;; (define-key cider-mode-map (kbd "<M-up>") 'sp-splice-sexp-killing-backward)
                               (define-key cider-mode-map (kbd "C-c <f17>") 'cider-switch-to-repl-buffer) ;; Used for keyremappings on Cocoa emacs
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

;; downcase-region (TODO why did I do this)
(put 'downcase-region 'disabled nil)

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

;; livedown
(add-hook 'markdown-mode-hook (lambda ()
                                (message "Running markdown mode hook!")
                                (require 'livedown)))

;; git-gutter
(global-git-gutter-mode)

;; toggle-transparency
(load "toggle-transparency")

;; ido-mode
;; ido-mode integrates well with Projectile
(ido-mode t)
;; Push mark when using ido-imenu
(defadvice ido-imenu (before push-mark activate)
  (push-mark))

;; helm
(defun projectile-helm-ag-at-point (arg)
  (interactive "P")
  (if arg
      (setq helm-ag-insert-at-point 'symbol)
    (setq helm-ag-insert-at-point nil))
  (helm-ag (projectile-project-root)))
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(setq helm-split-window-in-side-p t)
;; Enable useful helm commands
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c h t") 'helm-top)
(global-set-key (kbd "C-c a") 'projectile-helm-ag-at-point)
;; helm-projectile
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(defun projectile-persp-switch-project (project-to-switch)
  (interactive (list (projectile-completing-read "Switch to project: "
                                                 (projectile-relevant-known-projects))))
  (let* ((name (file-name-nondirectory (directory-file-name project-to-switch)))
         (persp (gethash name perspectives-hash)))
    (when (not (equal persp persp-curr))
      (persp-switch name))
    (projectile-switch-project-by-name project-to-switch)))
(global-set-key (kbd "C-c p q") 'projectile-persp-switch-project)

(defadvice projectile-switch-project (before projectile-create-perspective-after-switching-projects activate)
  "Create a dedicated perspective for current project's window after switching projects."
  (message "ADVICE CALLED")
  (let ((project-name (projectile-project-name)))
    (persp-switch project-name)))
(define-key helm-map [remap helm-projectile-switch-project] 'projectile-persp-switch-project)

;; helm-ag
(defun projectile-helm-ag ()
  (interactive)
  (helm-do-ag (projectile-project-root)))

;; Turn on helm
(helm-mode 1)
(helm-autoresize-mode t)


;; rest client
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

;; Objective C
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . objc-mode))

;; (benchmark-init/deactivate)
