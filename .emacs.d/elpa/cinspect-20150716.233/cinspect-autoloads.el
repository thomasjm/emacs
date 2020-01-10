;;; cinspect-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "cinspect" "cinspect.el" (0 0 0 0))
;;; Generated autoloads from cinspect.el

(autoload 'cinspect-mode "cinspect" "\
CInspect Mode.
Uses `cinspect' (https://github.com/punchagan/cinspect) to show CPython source for Python builtins
Can be used as a fallback option for `jedi-mode' (https://github.com/tkf/emacs-jedi).

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "cinspect" '("cinspect-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; cinspect-autoloads.el ends here
