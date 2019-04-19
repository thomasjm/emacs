
(require 'lsp-mode)

(lsp-define-stdio-client lsp-R "R"
                         (lambda () default-directory)
                         '("R" "--slave" "-e" "languageserver::run()"))

(add-hook 'R-mode-hook #'lsp-R-enable)



(lsp-register-client
    (make-lsp-client :new-connection
        (lsp-stdio-connection '("bash-language-server" "start"))
        :major-modes '(sh-mode)
        :server-id 'lsp-bash))
