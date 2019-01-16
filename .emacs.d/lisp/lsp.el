
(require 'lsp-mode)

(lsp-define-stdio-client lsp-R "R"
                         (lambda () default-directory)
                         '("R" "--slave" "-e" "languageserver::run()"))

(add-hook 'R-mode-hook #'lsp-R-enable)
