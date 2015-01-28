;; HipChat 

(require 'jabber)

(setq ssl-program-name "gnutls-cli"
      ssl-program-arguments '("--insecure" "-p" 5223)
      ;; ssl-program-arguments '("--insecure" "-p" 5222 "--starttls")      
      ssl-certificate-verification-policy 1)
 
;; Connect using jabber.el
;; M-x jabber-connect <RET>
 
;; Config
;;(setq jabber-account-list '(("56118_432933@chat.hipchat.com")))
(setq jabber-account-list '(
                            ("56118_432933@chat.hipchat.com"
                              (:password . nil) or (:password . "your-pass")
                              (:network-server . "chat.hipchat.com")
                              (:port . 5223)
                              (:connection-type . ssl))
                            ))


(setq hipchat-number "56118")
(setq hipchat-nickname "Tom McLaughlin")

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

(defun hipchat-connect ()
  (interactive)
  (jabber-connect-all)
  (hipchat-join "parastructure"))
