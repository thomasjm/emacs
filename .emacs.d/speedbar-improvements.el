

(add-hook 'speedbar-visiting-file-hook 'expand-buffer-functions-in-speedbar)

(add-hook 'speedbar-load-hook (lambda ()
								(message "Speedbar load hook!!!")))
