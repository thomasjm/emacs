(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(beacon-color "#c82829")
 '(coffee-tab-width 2)
 '(company-dabbrev-code-modes
   (quote
    (prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode haskell-interactive-mode)))
 '(company-global-modes (quote (not python-mode)))
 '(company-idle-delay 0.0)
 '(custom-enabled-themes (quote (afternoon)))
 '(custom-safe-themes
   (quote
    ("3d5307e5d6eb221ce17b0c952aa4cf65dbb3fa4a360e12a71e03aab78e0176c5" "885ef8634f55df1fa067838330e3aa24d97be9b48c30eadd533fde4972543b55" "96ae9ca589464d9ab7c9efaa3d8ba6ddce5d06af587817786806ea5a510c1364" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(flycheck-hlint-language-extensions (quote ("QuasiQuotes")))
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-args-stack-ghci (quote ("--ghci-options=-ferror-spans")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-tags-on-save t)
 '(helm-split-window-inside-p t)
 '(iflipb-ignore-buffers nil)
 '(js-indent-level 2)
 '(lsp-clojure-server-command (quote ("/bin/bash" "-c" "/home/tom/path/clojure-lsp")))
 '(package-selected-packages
   (quote
    (indent-tools ccls persp-projectile bats-mode lsp-rust lsp-sh lsp-typescript lsp-clangd company-coq gited yasnippet-bundle wget unbound typescript tidy sql-indent reveal-in-finder pymacs parscope org notify ipython hideshowvis helm-package flymake-cursor eimp dirtree dired-sort-menu+ dired-details+ dired+ company-cabal company-c-headers cmake-project cmake-mode cinspect calfw-gcal calfw autodisass-java-bytecode all ag afternoon-theme ack-and-a-half ace-jump-mode ac-python)))
 '(safe-local-variable-values
   (quote
    ((eval c-set-offset
           (quote access-label)
           (quote -))
     (eval c-set-offset
           (quote substatement-open)
           0)
     (eval c-set-offset
           (quote arglist-cont-nonempty)
           (quote +))
     (eval c-set-offset
           (quote arglist-cont)
           0)
     (eval c-set-offset
           (quote arglist-intro)
           (quote +))
     (eval c-set-offset
           (quote inline-open)
           0)
     (eval c-set-offset
           (quote defun-open)
           0)
     (eval c-set-offset
           (quote innamespace)
           0)
     (indicate-empty-lines . t)
     (eval c-set-offset
           (quote innamespace)
           4)
     (bug-reference-bug-regexp . "\\(\\(?:[Ii]ssue \\|[Ff]ixe[ds] \\|[Rr]esolve[ds]? \\|[Cc]lose[ds]? \\|[Pp]\\(?:ull [Rr]equest\\|[Rr]\\) \\|(\\)#\\([0-9]+\\))?\\)")
     (intero-targets "codedown-server:lib" "codedown-server:exe:codedown-server" "codedown-server:test:selenium-tests")
     (intero-targets "aeson-typescript:lib" "aeson-typescript:test:aeson-typescript-test")
     (intero-targets "hite:lib" "hite:exe:hite" "hite:test:hite-test" "hite-core:lib" "hite-test:lib")
     (intero-targets "hite-server:lib" "hite-server:exe:hite-server" "hite-server:test:selenium-tests"))))
 '(typescript-indent-level 2)
 '(web-mode-code-indent-offset 2)
 '(web-mode-enable-auto-quoting nil))
;; (require 'color)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-preview-common ((t (:background "#21e824bc35b0"))))
 '(company-scrollbar-bg ((t (:background "#2bd12f784561"))))
 '(company-scrollbar-fg ((t (:background "#21e824bc35b0"))))
 '(company-tooltip ((t (:inherit default :background "#1bf61e4b2c46"))))
 '(company-tooltip-annotation ((t (:foreground "deep sky blue"))))
 '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :foreground "deep sky blue" :weight bold))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(helm-selection ((t (:background "gray25" :distant-foreground "black" :foreground "white smoke")))))
