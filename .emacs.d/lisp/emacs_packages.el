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
(add-hook 'javascript-mode-hook
		  (lambda () (flycheck-mode t)))
(add-hook 'js2-mode-hook
		  (lambda () (flycheck-mode t)))

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


(defun replace-all-dired ()
  (interactive)

  (find-name-dired default-directory "*")
  (while (get-buffer-process (get-buffer "*Find*"))
    (sit-for 1))

  (dired-toggle-marks)

  (dired-do-query-replace-regexp)
  )


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

;; company-mode
(global-company-mode)
;; (add-to-list 'company-backends 'company-ghc)
(add-to-list 'company-backends '(company-ghc :with company-dabbrev-code))
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-preview-common ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))


   ;; From emacs_custom.el
   '(company-tooltip-annotation ((t (:foreground "deep sky blue"))))
   '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :foreground "deep sky blue" :weight bold))))
   '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))

   ))

;; Stuff from emacs_custom.el
(setq company-bg-color (face-attribute 'default :background))

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
;; (define-key org-mode-map "\M-q" 'toggle-truncate-lines)

;; eimp
(autoload 'eimp-mode "eimp" "Emacs Image Manipulation Package." t)
(add-hook 'image-mode-hook 'eimp-mode)

;; Org mode setup
(setq org-todo-keywords '((sequence "TODO" "IN PROGRESS" "IN REVIEW" "BLOCKED" "|" "WONTDO" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . "yellow")
        ("IN PROGRESS" . org-warning)))
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

;; Flycheck
(require 'flycheck)
(global-flycheck-mode t)


;; TSX
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))
            (subword-mode)
            )
          )

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;;; Haskell setup
(setq haskell-program-name "stack ghci") ;; important
;;(eval-after-load 'flycheck
;;  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
;; (eval-after-load 'flycheck
  ;; '(require 'flycheck-ghcmod))

;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)

;; Haddock displays in w3m
(require 'w3m-haddock)
(add-hook 'w3m-display-hook 'w3m-haddock-display)

(add-hook 'haskell-mode-hook (lambda ()
                               ;; (flycheck-select-checker 'haskell-stack)
                               (subword-mode)
                               (interactive-haskell-mode)
                               (intero-mode)
                               ;; (intero-mode)
                               ;; (ghc-init)
                               ))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(with-eval-after-load 'intero
  (flycheck-add-next-checker 'intero '(warning . haskell-hlint)))

;; (add-to-list 'load-path "~/.emacs.d/lisp/stack-mode")
;; (require 'stack-mode)
;; (add-hook 'haskell-mode-hook 'stack-mode)

;; tags on save
;; (setenv "PATH" (concat "~/.cabal/bin:" (getenv "PATH")))
(add-to-list 'exec-path "~/.local/bin")
(custom-set-variables '(haskell-tags-on-save t))
;; customization
(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))
(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c m") 'haskell-interactive-run-last)
  (define-key haskell-mode-map (kbd "C-c <return>") 'haskell-interactive-run-last)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c <f17>") 'haskell-interactive-bring) ;; Used for keyremappings on Cocoa emacs
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
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
;; (eval-after-load 'haskell-cabal '(progn
;;   (message "JUST LOADED HASKELL CABAL")
;;   (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-ode-clear)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
;; GHC
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;; (add-hook 'haskell-mode-hook #'hindent-mode)
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
(global-set-key (kbd "C-c o") 'helm-swoop)
;; (global-set-key (kbd "C-u C-c o") 'helm-multi-swoop)
(global-set-key (kbd "C-c h t") 'helm-top)
(global-set-key (kbd "C-c a") 'projectile-helm-ag-at-point)
;; helm-swoop
(setq helm-swoop-split-direction 'split-window-vertically)
(setq helm-swoop-pre-input-function
      (lambda () ""))
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
(defun projectile-helm-ag (arg)
  (interactive "P")
  (if arg
      (progn
        ;; Have to kill the prefix arg so it doesn't get forwarded
        ;; and screw up helm-do-ag
        (set-variable 'current-prefix-arg nil)
        (helm-do-ag (file-name-directory (buffer-file-name)))
        )
    (helm-do-ag (projectile-project-root))
    ))

;; (defun projectile-helm-ag (arg &optional options)
;;   (interactive "P")
;;   (if arg
;;       (progn
;;         ;; Have to kill the prefix arg so it doesn't get forwarded
;;         ;; and screw up helm-do-ag
;;         (set-variable 'current-prefix-arg nil)
;;         (let* ((grep-find-ignored-files (cl-union (projectile-ignored-files-rel) grep-find-ignored-files))
;;                (grep-find-ignored-directories (cl-union (projectile-ignored-directories-rel) grep-find-ignored-directories))
;;                (ignored (mapconcat (lambda (i)
;;                                      (concat "--ignore " i))
;;                                    (append grep-find-ignored-files grep-find-ignored-directories)
;;                                    " "))
;;                (helm-ag-command-option options)
;;                (helm-ag-base-command (concat helm-ag-base-command " " ignored))
;;                (current-prefix-arg nil))
;;           (helm-do-ag (file-name-directory (buffer-file-name)) (car (projectile-parse-dirconfig-file))))
;;         )
;;     (let* ((grep-find-ignored-files (cl-union (projectile-ignored-files-rel) grep-find-ignored-files))
;;            (grep-find-ignored-directories (cl-union (projectile-ignored-directories-rel) grep-find-ignored-directories))
;;            (ignored (mapconcat (lambda (i)
;;                                  (concat "--ignore " i))
;;                                (append grep-find-ignored-files grep-find-ignored-directories)
;;                                " "))
;;            (helm-ag-command-option options)
;;            (helm-ag-base-command (concat helm-ag-base-command " " ignored))
;;            (current-prefix-arg nil))
;;       (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
;;     ))

(defun projectile-helm-ag-in-folder ()
  (interactive)
  (helm-do-ag (projectile-project-root)))


;; Turn on helm
(helm-mode 1)
(helm-autoresize-mode t)
(global-set-key (kbd "M-y") 'yank-pop)

;; rest client
(add-to-list 'auto-mode-alist '("\\.rest\\'" . restclient-mode))

;; Objective C
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . objc-mode))

;; Highlight colors in CSS-like files
(add-hook 'css-mode-hook 'xah-syntax-color-hex)
(add-hook 'scss-mode-hook 'xah-syntax-color-hex)
(add-hook 'less-mode-hook 'xah-syntax-color-hex)
(add-hook 'html-mode-hook 'xah-syntax-color-hex)

;; Coffeescript
(custom-set-variables '(coffee-tab-width 2))


;; ob-ipython stuff
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (emacs-lisp . t)
   ))
(setq org-confirm-babel-evaluate nil)   ;don't prompt me to confirm everytime I want to evaluate a block
;;; display/update images in the buffer after I evaluate
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)


;; C/C++/Java settings
(setq-default c-basic-offset 4)

;; Typescript
;; sample config
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode t)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode t)
            (subword-mode)
            ;; (setq tab-width 4)
            ;; (setq typescript-indent-level 4)
            ))


;; multiple cursors improvements
(defun mark-next-sexp ()
  (interactive)
  (when (not (use-region-p))
    (push-mark)
    (setq mark-active t)
    (sp-forward-sexp)
    )
  (mc/mark-next-like-this 1)
  )
(global-set-key (kbd "C-c C->") 'mark-next-sexp)


;; Turn on hideshowvis-minor-mode globally
(define-globalized-minor-mode my-global-hideshowvis-mode hideshowvis-minor-mode
  (lambda () (hideshowvis-minor-mode 1)))
;; (my-global-hideshowvis-mode 1)


;; Jedi.el
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;; tramp-hdfs
(require 'tramp-hdfs)

;; which-key
(require 'which-key)
(which-key-mode)

;; wanderlust
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; IMAP
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "pyro777@gmail.com")
(setq elmo-imap4-default-authenticate-type 'clear)
(setq elmo-imap4-default-port '993)
(setq elmo-imap4-default-stream-type 'ssl)

(setq elmo-imap4-use-modified-utf7 t)

;; SMTP
(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "pyro777")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-local-domain "gmail.com")

(setq wl-default-folder "%inbox")
(setq wl-default-spec "%")
(setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
(setq wl-trash-folder "%[Gmail]/Trash")

(setq wl-folder-check-async t)

(setq elmo-imap4-use-modified-utf7 t)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))


;; Nunjucks files
(add-to-list 'auto-mode-alist '("\\.nunjucks\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode))

;; coq mode
(add-hook 'coq-mode-hook #'company-coq-mode)

;; Seems this needs to be defined manually in Emacs 25
(define-key global-map "\M-*" 'pop-tag-mark)

(setq cquery-executable "/home/tom/path/cquery")

;; (benchmark-init/deactivate)
