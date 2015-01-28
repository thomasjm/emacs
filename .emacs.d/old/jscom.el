
(require 'js-comint)
;; Use node as our repl
(setq inferior-js-program-command "node")

(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
		   (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))))
;; (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)))))

(setq js2-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
		   (replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))))
;;(replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)))))

;; (setq inferior-js-mode-hook
;;       (lambda ()
;;         ;; We like nice colors
;;         (ansi-color-for-comint-mode-on)
;;         ;; Deal with some prompt nonsense
;;         (add-to-list 'comint-preoutput-filter-functions
;;                      (lambda (output)
;;                        (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
;;                      (replace-regexp-in-string ".*1G.*3G" "&gt;" output))))))

(add-hook 'js2-mode-hook '(lambda ()
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))

(defun node-repl () (interactive)
      (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive"))
    (node-repl))
