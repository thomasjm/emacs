;;; repl-toggle-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (rtog/toggle-repl rtog/add-repl rtog/switch-to-shell-buffer
;;;;;;  repl-toggle-mode) "repl-toggle" "repl-toggle.el" (21601 9523
;;;;;;  0 0))
;;; Generated autoloads from repl-toggle.el

(defvar repl-toggle-mode nil "\
Non-nil if Repl-Mode mode is enabled.
See the command `repl-toggle-mode' for a description of this minor mode.")

(custom-autoload 'repl-toggle-mode "repl-toggle" nil)

(autoload 'repl-toggle-mode "repl-toggle" "\
A minor mode to allow uniform repl buffer switching.

\(fn &optional ARG)" t nil)

(autoload 'rtog/switch-to-shell-buffer "repl-toggle" "\
Make sure that `BUFFER-NAME' exists and is displayed.

Executes `SHELL-COMMAND', passing `SHELL-ARGS', if buffer
`BUFFER-NAME' doesn't exist.

\(fn BUFFER-NAME SHELL-COMMAND &optional SHELL-ARGS)" nil t)

(autoload 'rtog/add-repl "repl-toggle" "\
Associate MODE with REPL-CMD at runtime..

If in a buffer with `major-mode' MODE, execute REPL-CMD when
`rtog/toggle-repl' is called.

\(fn MODE REPL-CMD)" t nil)

(autoload 'rtog/toggle-repl "repl-toggle" "\
Switch to the repl asscociated with the current major mode.

If in a repl already switch back to the buffer we
came from.

If you provide PASSALONG? as a universal prefix with
\\[universal-argument], the current line or region is passed to
the repl buffer, using \\[universal-argument]
\\[universal-argument] the current function or definition is
passed, and finaly using
\\[universal-argument]\\[universal-argument]\\[universal-argument]
you can pass the whole current buffer.

Additional paramters passed will be IGNORED.

\(fn &optional PASSALONG\\? &rest IGNORED)" t nil)

;;;***

;;;### (autoloads nil nil ("repl-toggle-pkg.el") (21601 9523 294874
;;;;;;  0))

;;;***

(provide 'repl-toggle-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; repl-toggle-autoloads.el ends here
