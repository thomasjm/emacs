(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(coffee-tab-width 2)
 '(custom-enabled-themes (quote (afternoon)))
 '(custom-safe-themes
   (quote
    ("885ef8634f55df1fa067838330e3aa24d97be9b48c30eadd533fde4972543b55" "96ae9ca589464d9ab7c9efaa3d8ba6ddce5d06af587817786806ea5a510c1364" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(exec-path
   (quote
    ("~/.cabal/bin" "/Users/tomm/path" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin" "/usr/local/Cellar/ghc/7.6.3/bin/" "/usr/local/Cellar/imagemagick/6.8.9-1/bin" "/Users/tomm/Library/Haskell/bin/" "/Users/tomm/ihaskell/.cabal-sandbox/bin" "/Users/tomm/.cabal/bin/" "/Applications/ghc-7.8.3.app/Contents/bin/" "/home/tom/path")))
 '(fci-rule-color "#073642")
 '(haskell-ask-also-kill-buffers nil)
 '(haskell-indentation-cycle-warn nil)
 '(haskell-interactive-mode-eval-pretty t)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(markdown-command
   "pandoc -f markdown_github -c /Users/tomm/Dropbox/useful/markdown/kultiad.css")
 '(org-startup-truncated nil)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("milkbox" . "http://melpa.milkbox.net/packages/"))))
 '(safe-local-variable-values (quote ((buffer-file-coding-system . utf-8-unix))))
 '(vc-annotate-background "#93a1a1")
 '(vc-annotate-color-map
   (quote
    ((20 . "#990A1B")
     (40 . "#FF6E64")
     (60 . "#cb4b16")
     (80 . "#7B6000")
     (100 . "#b58900")
     (120 . "#DEB542")
     (140 . "#546E00")
     (160 . "#859900")
     (180 . "#B4C342")
     (200 . "#3F4D91")
     (220 . "#6c71c4")
     (240 . "#9EA0E5")
     (260 . "#2aa198")
     (280 . "#69CABF")
     (300 . "#00629D")
     (320 . "#268bd2")
     (340 . "#69B7F0")
     (360 . "#d33682"))))
 '(vc-annotate-very-old-color "#93115C")
 '(winner-mode t))
(setq company-bg-color (face-attribute 'default :background))
;; (require 'color)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(persp-selected-face ((t (:foreground "green4" :weight bold)))))
