;;;;;;;;;
;; global
(require 'smartparens-config)
(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

(define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
(define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
(define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
(define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
(define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
(define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
(define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
(define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
(define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)

;; Tom stuff

(defun my-parens ()
  (interactive)
  (sp-select-next-thing)
  (insert-parentheses)
  (sp-backward-sexp))

(define-key sp-keymap (kbd "M-(") 'my-parens)

(defun kill-sexp-if-paren ()
  (interactive)
  (if (or
       (looking-at "\(")
       (looking-at "\\[")
       (looking-at "\{"))
      (sp-kill-sexp)
    (kill-word 1)))

;; (define-key sp-keymap (kbd "M-d") 'kill-sexp-if-paren)

(defun reformat-sexp ()
  (interactive)
  (save-excursion
    (sp-up-sexp)
    (sp-up-sexp)
    (sp-up-sexp)
    (sp-up-sexp)
    (sp-up-sexp)
    (sp-up-sexp)
    (sp-up-sexp)

    (sp-backward-sexp)
    (sp-select-next-thing)

    (indent-for-tab-command)))


(define-key sp-keymap (kbd "M-<down>") 'reformat-sexp)
(define-key sp-keymap (kbd "M-<up>") 'sp-splice-sexp-killing-backward)


(defun my-create-newline-and-allman-format (&rest _ignored)
"Allman-style formatting for C."
  (interactive)
  (progn
    (newline-and-indent)
    (previous-line) (previous-line) (search-forward "{") (backward-char) (newline-and-indent)
    (next-line) (indent-according-to-mode)))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (interactive)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;; (sp-local-pair '(java-mode) "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'java-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'javascript-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'js2-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
