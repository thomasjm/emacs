;; HipChat
(setq ssl-program-name "gnutls-cli"
      ssl-program-arguments '("--insecure" "-p" service host)
      ssl-certificate-verification-policy 1)

;; Connect using jabber.el
;; M-x jabber-connect <RET>

;; Config
(setq jabber-account-list '(("26619_858606@chat.hipchat.com")))
(defvar hipchat-number "26619")
(defvar hipchat-nickname "Tom McLaughlin")

;; Join a room
(defun hipchat-join (room)
  (interactive "sRoom name: ")
  (jabber-groupchat-join
   (jabber-read-account)
   (concat hipchat-number "_" room "@conf.hipchat.com")
   hipchat-nickname
   t))

;; Mention nicknames in a way that HipChat clients will pickup
(defun hipchat-mention (nickname)
  (interactive
    (list (jabber-muc-read-nickname jabber-group "Nickname: ")))
      (insert (concat "@\"" nickname "\" ")))

(setq jabber-alert-presence-message-function (lambda (who oldstatus newstatus statustext) nil))


;; (require 'notify)

;; (defun notify-jabber-notify (from buf text proposed-alert)
;;   "(jabber.el hook) Notify of new Jabber chat messages via notify.el"
;;   (when (or jabber-message-alert-same-buffer
;;             (not (memq (selected-window) (get-buffer-window-list buf))))
;;     (if (jabber-muc-sender-p from)
;;         (notify (format "(PM) %s"
;;                        (jabber-jid-displayname (jabber-jid-user from)))
;;                (format "%s: %s" (jabber-jid-resource from) text)))
;;       (notify (format "%s" (jabber-jid-displayname from))
;;              text)))

;; (add-hook 'jabber-alert-message-hooks 'notify-jabber-notify)

;; (require 'autosmiley)
;; (add-hook 'jabber-chat-mode-hook 'autosmiley-mode)
