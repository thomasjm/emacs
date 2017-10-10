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


(defun sudo-shell-command (buffer password command)
  (message "command")
  (message command)
  (with-current-buffer (get-buffer-create buffer)
    (erase-buffer)
    (term-mode)
    (let ((fullcmd (concat "sudo /bin/bash -c \"" command "\"")))
      (insert fullcmd)
      (let ((proc (start-process-shell-command "*sudo*" buffer fullcmd)))
        (process-send-string proc password)
        (process-send-string proc "\r")
        (process-send-eof proc)))))

;; (defun run-current-file (password)
;;   (interactive (list (read-passwd "Sudo password: ")))
;;   (sudo-shell-command "*python-test*" password
;;                       (format "/home/tomm/run_test.sh %s" (buffer-file-name)))
;;   (clear-string password))

(defun run-current-file ()
  (interactive)
  (let* (
         (runcmd (format "/home/tomm/run_test.sh %s" (buffer-file-name)))
         (cmd (shell-command-to-string runcmd))
         (gnomecmd (format "gnome-terminal -e '%s'" cmd))
        )

    (message cmd)
    (message gnomecmd)
    (shell-command gnomecmd)
    )
  )

; Mysterious why this won't work
; sudo lxc-attach --name server_default_1446843349962_99299 -- py.test /srv/server/metaserver/metaserver/tests/lib_tests/stormcrow/data_field_tests.py -k usage

(defun make-relative-path (abspath relto)
  (let* ((command (format "python -c \"import os.path; print os.path.relpath('%s', '%s')\"" abspath relto)))
    (replace-regexp-in-string "\n$" "" (shell-command-to-string command))))

(defun run-current-test ()
  (interactive)
  ;; Note: make sure lxc-ls and lxc-attach are set to NOPASSWD in your sudoers file
  ;; Use a line like this:
  ;;
  ;; /etc/sudoers
  ;; $YOUR_USERNAME ALL=(root) NOPASSWD: /usr/bin/lxc-ls, /usr/bin/lxc-info, /usr/bin/lxc-attach
  ;;
  ;; How to use: put the cursor inside a test somewhere in the server repo and run this function.
  ;; It will open a new Gnome terminal and run the test.
  ;; With a prefix argument, it will run the entire file.
  (require 'which-func)
  (let* (
         (container (replace-regexp-in-string "\n$" ""
                                              (shell-command-to-string "sudo lxc-ls -f | grep RUNNING | awk '{print $1;}'")))
         (filename (make-relative-path (buffer-file-name) "$HOME/src"))
         (test (car (cdr (split-string (which-function) "\\."))))
         (cmd (if current-prefix-arg
                  (format "/bin/bash -c \"sudo lxc-attach --name %s -- su -c \\\"py.test /srv/%s; read -n 1 key\\\" vagrant\""
                          container filename)
                (format "/bin/bash -c \"sudo lxc-attach --name %s -- su -c \\\"py.test /srv/%s -k %s; read -n 1 key\\\" vagrant\""
                        container filename test)))
         (gnomecmd (format "gnome-terminal -e '%s'" cmd))
        )

    (if (string= "" container)
        (message "Error: no server LXC container found")
      (shell-command gnomecmd)
      )))


(defun ipython-here ()
  (interactive)
  ;; Note: make sure lxc-ls and lxc-attach are set to NOPASSWD in your sudoers file
  ;; Use a line like this:
  ;;
  ;; /etc/sudoers
  ;; $YOUR_USERNAME ALL=(root) NOPASSWD: /usr/bin/lxc-ls, /usr/bin/lxc-info, /usr/bin/lxc-attach
  ;;
  ;; How to use: put the cursor inside a test somewhere in the server repo and run this function.
  ;; It will open a new Gnome terminal and run the test.
  ;; With a prefix argument, it will run the entire file.
  (require 'which-func)
  (let* (
         (container (replace-regexp-in-string "\n$" ""
                                              (shell-command-to-string "sudo lxc-ls -f | grep RUNNING | awk '{print $1;}'")))
         (filename (make-relative-path (buffer-file-name) "$HOME/src"))
         (cmd (format "/bin/bash -c \"sudo lxc-attach --name %s -- su -c \\\"ipython -i /srv/%s\\\" vagrant\"" container filename))
         (gnomecmd (format "gnome-terminal -e '%s'" cmd))
        )

    (message cmd)

    ;; TODO: raise error if container not found
    (if (string= "" container)
        (message "Error: no server LXC container found")
      (shell-command gnomecmd)
      )
    ))
