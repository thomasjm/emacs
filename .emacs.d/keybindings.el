;;; Backspace keys ;;;
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;;; Other keybindings
(global-set-key (kbd "C-M-r") 'revert-buffer)

;; Fastnav
(require 'fastnav)
;; (global-set-key "\M-z" 'fastnav-zap-up-to-char-forward)
(global-set-key "\M-Z" 'fastnav-zap-up-to-char-backward)
(global-set-key "\M-s" 'fastnav-jump-to-char-forward)
(global-set-key "\M-S" 'fastnav-jump-to-char-backward)
(global-set-key "\M-r" 'fastnav-replace-char-forward)
(global-set-key "\M-R" 'fastnav-replace-char-backward)
(global-set-key "\M-i" 'fastnav-insert-at-char-forward)
(global-set-key "\M-I" 'fastnav-insert-at-char-backward)
(global-set-key "\M-j" 'fastnav-execute-at-char-forward)
(global-set-key "\M-J" 'fastnav-execute-at-char-backward)
(global-set-key "\M-k" 'fastnav-delete-char-forward)
(global-set-key "\M-K" 'fastnav-delete-char-backward)
(global-set-key "\M-M" 'fastnav-mark-to-char-backward)
(global-set-key "\M-p" 'fastnav-sprint-forward)
(global-set-key "\M-P" 'fastnav-sprint-backward)

(global-set-key (kbd "M-z") 'zap-up-to-char)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; ace-jump
(global-set-key (kbd "C-0") 'ace-jump-mode)
(global-set-key (kbd "C-9") 'ace-jump-line-mode)

;; replace-last-sexp
(global-set-key (kbd "C-x e") 'replace-last-sexp)

;; lively
(global-set-key (kbd "C-c l") 'lively)
(global-set-key (kbd "C-c s") 'lively-stop)

;; New scratch buffer
(global-set-key (kbd "<f7>") 'new-scratch)

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Node js eval region or buffer
(define-key global-map (kbd "C-c v") 'node-js-eval-region-or-buffer)

;; Copy an entire line
(global-set-key "\C-c\C-k" 'copy-line)

;; Python mode improvements
(global-set-key "\C-c\C-d" 'restart-python-process-and-send-buffer)

;; Like vim "super star"
(global-set-key (kbd "C-*") 'highlight-symbol-next)
(global-set-key (kbd "C-&") 'highlight-symbol-prev)

;; senator
(global-set-key (kbd "C--") 'senator-fold-tag)
(global-set-key (kbd "C-+") 'senator-unfold-tag)

;; Speedbar
(global-set-key (kbd "<f9>") 'sr-speedbar-toggle)
(global-set-key (kbd "C-{") 'sr-expand-buffer-functions-in-speedbar)

;; Clojure editing stuff
(global-set-key (kbd "C-;") 'compile-site)
(global-set-key (kbd "C-M-;") 'cider-repl-run-last)

;; magit
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "<f8>") 'magit-status)

;; Org mode capture
(define-key global-map "\C-cc" 'org-capture)

;; Dash (OSX) or Zeal (Ubuntu)
(cond ((equal system-type 'gnu/linux) (global-set-key "\C-cd" 'zeal-at-point))
	  ((equal system-type 'darwin) (global-set-key "\C-cd" 'dash-at-point)))

;; kill-ring-search
(global-set-key (kbd "C-M-y") 'kill-ring-search)

;; C-x ` is really hard to reach
(global-set-key (kbd "C-x C-n") 'next-error)
(global-set-key (kbd "C-x C-p") 'previous-error)

;; Some useful variants of built-in things
(global-set-key (kbd "C-c C-f") 'open-file-at-cursor)
(global-set-key (kbd "C-c 0") 'close-and-kill-this-pane)


;; winner
(global-set-key (kbd "<C-left>") 'winner-undo)
(global-set-key (kbd "<C-right>") 'winner-redo)

;; (global-set-key (kbd "C-x C-r") 'projectile-helm-ag)


;; TODO: figure this out
;; (global-set-key [C-mouse-wheel-up-event]  'text-scale-increase)
;; (global-set-key  [C-mouse-wheel-down-event] 'text-scale-decrease)
;; (global-set-key [C-mouse-4] 'text-scale-increase)
;; (global-set-key [C-mouse-5] 'text-scale-decrease)
;; (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
;; (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)
