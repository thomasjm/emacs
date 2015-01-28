(defun email () 
  (interactive)
 
  (when (not (featurep 'mu4e))
    (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
 
    (require 'mu4e)
    (require 'org-mu4e)
 
    ;; defaults
 
    (setq mu4e-maildir "~/Maildir/")
    (setq mu4e-drafts-folder "/gmail/[Gmail].Drafts")
    (setq mu4e-sent-folder   "/gmail/[Gmail].Sent Mail")
    (setq mu4e-trash-folder  "/gmail/[Gmail].Trash")
 
    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
    (setq mu4e-sent-messages-behavior 'delete)
 
    ;; setup some handy shortcuts
    ;; you can quickly switch to your Inbox -- press ``ji''
    ;; then, when you want archive some messages, move them to
    ;; the 'All Mail' folder by pressing ``ma''.
 
    (setq mu4e-maildir-shortcuts
          '( ("/gmail/INBOX"               . ?i)
             ("/gmail/[Gmail].IMPORTANT"   . ?!)
             ;; ("/gmail/[Gmail].Sent Mail"   . ?s)
             ;; ("/gmail/[Gmail].Trash"       . ?t)
             ("/gmail/[Gmail].All Mail"    . ?a)))
 
    ;; allow for updating mail using 'U' in the main view:
    ;; I have this running in the background anyway
    ;; (setq mu4e-get-mail-command "offlineimap")
 
    ;; something about ourselves
    (setq
     user-mail-address "tom@parastructure.com"
     user-full-name  "Tom McLaughlin"
     message-signature nil)
 
    ;; sending mail -- replace USERNAME with your gmail username
    ;; also, make sure the gnutls command line utils are installed
    ;; package 'gnutls-bin' in Debian/Ubuntu
 
    (require 'smtpmail)
 
    (if (string< emacs-version "24")
        (setq message-send-mail-function 'smtpmail-send-it
              starttls-use-gnutls t
              smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
              smtpmail-auth-credentials
              '(("smtp.gmail.com" 587 "user@domain" nil))
              smtpmail-default-smtp-server "smtp.gmail.com"
              smtpmail-smtp-server "smtp.gmail.com"
              smtpmail-smtp-service 587)
 
      ;; alternatively, for emacs-24 you can use:
      (setq message-send-mail-function 'smtpmail-send-it
            smtpmail-stream-type 'starttls
            smtpmail-default-smtp-server "smtp.gmail.com"
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587)
      )
 
 
    ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)
 
    ;; show images
    (setq mu4e-show-images t)
 
    ;; use imagemagick, if available
    (when (fboundp 'imagemagick-register-types)
      (imagemagick-register-types))
 
    ;;; message view action
    (defun mu4e-msgv-action-view-in-browser (msg)
      "View the body of the message in a web browser."
      (interactive)
      (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
            (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
        (unless html (error "No html part for this message"))
        (with-temp-file tmpfile
          (insert
           "<html>"
           "<head><meta http-equiv=\"content-type\""
           "content=\"text/html;charset=UTF-8\">"
           html))
        (browse-url (concat "file://" tmpfile))))
 
    (add-to-list 'mu4e-view-actions
                 '("View in browser" . mu4e-msgv-action-view-in-browser) t)
 
    ;; convert org mode to HTML automatically
    (setq org-mu4e-convert-to-html t)
 
    ;; need this to convert some e-mails properly
    (setq mu4e-html2text-command "html2text -utf8 -width 72")
  )
  (mu4e)
)
 
(defalias 'org-mail 'org-mu4e-compose-org-mode)
