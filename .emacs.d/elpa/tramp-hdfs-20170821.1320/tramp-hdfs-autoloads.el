;;; tramp-hdfs-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "tramp-hdfs" "tramp-hdfs.el" (0 0 0 0))
;;; Generated autoloads from tramp-hdfs.el

(eval-after-load 'tramp '(tramp-set-completion-function "hdfs" tramp-completion-function-alist-ssh))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "tramp-hdfs" '("tramp-hdfs-" "file-modes-number-to-string" "hdfs-" "http-header-regexp" "webhdfs-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tramp-hdfs-autoloads.el ends here
