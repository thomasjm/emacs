;;; nsis-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "nsis-mode" "nsis-mode.el" (0 0 0 0))
;;; Generated autoloads from nsis-mode.el

(setq auto-mode-alist (append '(("\\.[Nn][Ss][HhIi]\\'" . nsis-mode)) auto-mode-alist))

(autoload 'nsis-mode "nsis-mode" "\
Major mode for editing Nsi files.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nsis-mode" '("nsis-" "yas-munge-callback-")))

;;;***

;;;### (autoloads nil nil ("nsis-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; nsis-mode-autoloads.el ends here
