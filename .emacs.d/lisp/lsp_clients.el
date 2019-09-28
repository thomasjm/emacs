
(require 'lsp-mode)

;; (lsp-define-stdio-client lsp-R "R"
;;                          (lambda () default-directory)
;;                          '("R" "--slave" "-e" "languageserver::run()"))

;; (add-hook 'R-mode-hook #'lsp-R-enable)

;; (lsp-register-client
;;     (make-lsp-client :new-connection
;;         (lsp-stdio-connection '("bash-language-server" "start"))
;;         :major-modes '(sh-mode)
;;         :server-id 'lsp-bash))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :config
  (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure-mode"))
  :init
  (setq lsp-enable-indentation nil)
  ;; (add-hook 'clojure-mode-hook #'lsp)
  ;; (add-hook 'clojurec-mode-hook #'lsp)
  ;; (add-hook 'clojurescript-mode-hook #'lsp)
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp)


(require 'ccls)
