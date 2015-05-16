;; (when (load "flymake" t)
;;  (defun flymake-pyflakes-init ()
;;    (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                       'flymake-create-temp-inplace))
;;           (local-file (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;      (list "/Users/tomm/path/pyxl_pyflakes" (list local-file))))

;;  (add-to-list 'flymake-allowed-file-name-masks
;;               '("\\.py\\'" flymake-pyflakes-init)))

;; (add-hook 'python-mode-hook 'flymake-find-file-hook)

;; TODO: this should make elpy use flycheck instead of flymake
;; (when (require 'flycheck nil t)
;;   (setq elpy-default-minor-modes (delete 'flymake-mode elpy-default-minor-modes))
;;   (add-to-list 'elpy-default-minor-modes 'flycheck-mode))

;; Autosave to temp folder
(setq backup-directory-alist
      `((".*" . , temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" , temporary-file-directory t)))

(add-to-list 'exec-path "/Users/tomm/path")


(defun apxfe-run-current-test ()
  (interactive)
  (let
      ( (output-buffer-name (format "*apxfe test %s *" (buffer-name)))
		(path buffer-file-name)
		)
    (with-current-buffer (get-buffer-create output-buffer-name)
      (message "Running APXFE test: %s" path)
      (erase-buffer)
      (term-mode)
      (display-buffer (current-buffer) t))
    (buffer-disable-undo)
    (start-process "pxfe-test-process" output-buffer-name
                   "gulp"
                   "test"
                   "--tests"
                   path
                   )
    (buffer-enable-undo)))

(defun apxfe-run-current-test-in-terminal ()
  (interactive)
  (let
      ( (buf (if (get-buffer "*apxfe test*")
                 (get-buffer "*apxfe test*")
               (ansi-term "/bin/bash" "apxfe test")))

		(path (buffer-file-name))
        )
    (message "Got path: %s" (buffer-file-name))
    (with-current-buffer buf
      (message "Running APXFE test: %s" path)
      (display-buffer (current-buffer) t)
      (term-line-mode)
      (end-of-buffer)
      (insert "cd /Users/tomm/src/apxfe/nighthawk")
      (newline)
      (term-send-input)

      (message (concat "gulp test --tests " path))
      (insert (concat "gulp test --tests " path))
      (newline)
      (term-send-input)

      )))
