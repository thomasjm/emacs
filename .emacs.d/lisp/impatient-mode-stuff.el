;; Clojure buffer visiting code
(require 's)

(defun test-cider-print ()
  (interactive)
  (cider-interactive-eval "(site.core/-main)"))

(global-set-key (kbd "C-M-m") 'test-cider-print)

(defun imp-clj-html (buffer)
  ;; (interactive)
  (with-current-buffer (get-buffer buffer)
    (cider-eval-defun-at-point))
  (let* ((content (with-temp-buffer
		    (cider-interactive-eval-print "(site.layout/page)")
		    (sleep-for 0.1)
		    (buffer-string)))
	 (sanitized-content (s-replace-all '(("\\n" . "\n") ("\\\"" . "'")) content)))
    (princ sanitized-content (current-buffer))
    )
  (delete-region (point-min) (+ (point-min) 1))
  (delete-region (- (point-max) 1) (point-max)))

(defun try-cider-ping ()
  (condition-case nil
	  (progn
		(string= (cider-ping) "PONG"))
	((debug error) nil)))

(defun impatient-clojure ()
  (interactive)
  (httpd-start)

  ; Start a new cider server if necessary
  (when (or (not (fboundp 'cider-ping))
			(not (try-cider-ping)))
	(cider-jack-in))

  ; Wait for the server to come up
  (message "Waiting for nREPL server...")
  (block myloop
	(while t
	  (condition-case nil
		  (progn
			(sit-for 0.5)
			(when (string= (cider-ping) "PONG") ; TODO: just use try-cider-ping here
			  (return-from myloop)))
		((debug error) nil))))


  (cider-eval-buffer)

  (impatient-mode)

  (imp-visit-buffer)
  (imp-set-user-filter 'imp-clj-html))


;;;;;;;;;;;;;;;;;; Markdown editing ;;;;;;;;;;;;;;;;;;

;; Usage:
;; (imp-markdown-current-buffer) ;; start
;; (imp-normal) ;; back to normal impatient-mode

;; (defun imp-markdown (buffer)
;;   (interactive)
;;   (httpd-start)
;;   (unless (and (boundp 'impatient-mode) impatient-mode)
;;     (impatient-mode))
;;   (unless (fboundp 'imp--send-state-old)
;;     (defalias 'imp--send-state-old (symbol-function 'imp--send-state)))
;;   (defun imp--send-state (proc)
;;     (let ((id (number-to-string imp-last-state))
;;           (buffer (current-buffer)))
;;       (with-temp-buffer
;;         (insert id " ")
;;         (insert (markdown-to-html buffer))
;;         (httpd-send-header proc "text/plain" 200 :Cache-Control "no-cache"))))
;;   (imp-visit-buffer)
;;   'imp--send-state-old)

;;;###autoload
(defun imp-normal ()
  (interactive)
  (defalias 'imp--send-state 'imp--send-state-old))

;;;###autoload

(defun imp-markdown-html (buffer)
  (let ((md-file "/tmp/this-is-a-nonsense-file.md"))
    (unwind-protect
        (progn
          (with-temp-file md-file
            (kill-region (point-min) (point-max))
            (insert (with-current-buffer buffer (buffer-string))))
          (let ((content (shell-command-to-string
						  (format "markdown %s" md-file))))
			(princ content (current-buffer))))
      (delete-file md-file))))

(defun imp-markdown-current-buffer ()
  (interactive)
  (httpd-start)

  (impatient-mode)
  (imp-visit-buffer)
  (imp-set-user-filter 'imp-markdown-html))
