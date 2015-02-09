(deftheme My theme with afternoon
  "Created 2015-01-28.")

(custom-theme-set-variables
 'My theme with afternoon
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes (quote ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "31a01668c84d03862a970c471edbd377b2430868eccf5e8a9aec6831f1a0908d" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "1297a022df4228b81bc0436230f211bad168a117282c20ddcba2db8c6a200743" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(exec-path (quote ("/Users/tomm/path" "/usr/bin" "/bin" "/usr/sbin" "/sbin" "/Applications/Emacs.app/Contents/MacOS/libexec" "/Applications/Emacs.app/Contents/MacOS/bin" "/usr/local/bin" "/usr/local/Cellar/ghc/7.6.3/bin/" "/usr/local/Cellar/imagemagick/6.8.9-1/bin" "/Users/tomm/Library/Haskell/bin/")))
 '(haskell-mode-hook (quote (turn-on-haskell-indent (lambda nil (ghc-init)))))
 '(inhibit-startup-screen t)
 '(org-startup-truncated nil)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("milkbox" . "http://melpa.milkbox.net/packages/"))))
 '(winner-mode t))

(custom-theme-set-faces
 'My theme with afternoon
 '(persp-selected-face ((t (:foreground "green4" :weight bold)))))

(provide-theme 'My theme with afternoon)
