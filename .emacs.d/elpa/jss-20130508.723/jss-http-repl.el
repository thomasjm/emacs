;;; jss-http-repl.el -- major mode for sending http requests
;;
;; Copyright (C) 2013 Edward Marco Baringer
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE. See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

(require 'cl)
(require 'url)
(require 'jss-utils)
(require 'jss-super-mode)

(make-variable-buffer-local
 (defvar jss-http-repl-status nil
   "The current status of the server connection (:opening, :sending, :receiving-headers, :receiving-data, :idle or :closed)"))

;;;###autoload
(define-derived-mode jss-http-repl-mode jss-super-mode "JSS HTTP REPL"
  "Major mode for manually creating, editing and submitting HTTP requests.

A jss-http-repl buffer contains a set of HTTP
transactions (request / response pairs). The request, consisting
of headers and data, is edited as normal emacs text, then
converted into an HTTP request, sent as binary data (the way text
is converted into binary still needs work) and the response,
headers and data, is inserted into the buffer.

After each transaction a new response header/data set is created
and inserted into the buffer (this is usually just a copy of the
previous sent headers and data, but if
jss-http-repl-track-cookies is T, the default, Cookie headers may
be added.

Each http request consists of an endpoint, specifying the server,
port and method (http or https) to connect to, a set of headers
and some request data (if a post). Type the request method (GET,
POST, HEAD, whatever) after the endpoint marker followed by the
desired url (note: include the http or https schema, otherwise we
don't know which protocol to use). The Host header and
Request-Line is automatically updated from the value of the
Endpoint: line.

The request headers can be edited manually, though the function
jss-http-repl-ensure-header (bound to C-<return> by defoult)
provides a slightly more convenient interface for creating common
headers.

Any post data should be added after the '--request data follows
this line--' marker, when submitting the Content-Length and
Content-Type headers will have their default values (which can be
changed) set based on this data.

Once the requets data has been prepared hit C-c
C-c (jss-http-repl-send-request), to open up a network connection
and send the request. The buffer will become read only, the
response (headers and data), will be inserted in the buffer and
then a new request line, based on the previous request and the
response headers, will be inserted.

Note: The network traffic generated by this mode does _not_ go
through an external web browser, it is sent by the running emacs
instance itself. (this generally has no consequences, but it does
mean that, compared to other requests made via jss, the ip
address and http handling may be different).

Some convenience functions are provided for setting certain
headers (Authorization is the only one that's actually useful,
the other are just shortucts).

A jss-http-repl buffer can be quickly created from a JSS IO
buffer; this allows for easily editing/debugging/replaying recent
json requests from the browser.

This mode is designed for testing/debugging json/ajax requests
but it can be used with any kind of HTTP request."
  (setf buffer-file-coding-system 'utf-8-unix
        jss-http-repl-status :closed)
  ;; we do same string->byte conversions and assume that the buffer is
  ;; a multibyte buffer, this is already emacs' default, but let's
  ;; just be explicit about it.
  (set-buffer-multibyte t)
  (add-hook 'after-change-functions 'jss-http-repl-after-change-function nil t)
  t)

(easy-menu-define jss-http-repl-menu jss-http-repl-mode-map
  "Menu for JSS HTTP REPL buffers."
  '("JSS HTTP REPL"
    [ "Submit" jss-http-repl-send-request t]
    [ "Ensure Header" jss-http-repl-ensure-header t ]))

(define-key jss-http-repl-mode-map (kbd "C-c <return>") 'jss-http-repl-ensure-header)
(define-key jss-http-repl-mode-map (kbd "C-c C-c") 'jss-http-repl-send-request)

(defcustom jss-http-repl-track-cookies t
  "When T, and can be buffer local, automatically keep track of
  cookies by adding/removing headers from the request objects."
  :type 'boolean
  :group 'jss)

(make-variable-buffer-local 'jss-http-repl-track-cookies)

(defvar jss-http-repl-cookie-jar '())

(make-variable-buffer-local
 (defvar jss-http-repl-previous-request-data nil))

(make-variable-buffer-local
 (defvar jss-http-repl-keep-alive nil
   "When T the current server connection should be reused."))

(make-variable-buffer-local
 (defvar jss-http-repl-content-length nil
   "The number of bytes of body data we're expecting."))

(make-variable-buffer-local
 (defvar jss-http-repl-response-data-start nil
   "The number of bytes of body data we're expecting."))

(make-variable-buffer-local
 (defvar jss-http-repl-set-cookies '()
   "Any cookies that should be added to the following request's headers."))

(defface jss-http-repl-meta-data-face
  '((t :inherit font-lock-special-keyword-face))
  "Face used to mark 'meta' headers such as Endpoint:, --post
  data starts here-- etc."
  :group 'jss)

(defface jss-http-repl-submitted-face
  '((t :slant italic))
  "Face used to mark data that has been sent and is no longer editable."
  :group 'jss)

(defun* jss-http-repl-new (&rest insert-args)
  (with-current-buffer (generate-new-buffer "*JSS HTTP REPL*")
    (jss-http-repl-mode)
    (apply 'jss-http-repl-insert-request insert-args)
    (when insert-args
      (jss-http-repl-goto-data-start))
    (current-buffer)))

;;;###autoload
(defun jss-http-repl (&optional initial-endpoint)
  (interactive)
  (switch-to-buffer (jss-http-repl-new :method "GET" :url initial-endpoint)))

(defun* jss-http-repl-insert-request (&key header-string data-string url method http-version)
  ;; (declare (ignore ssl))
  (unless (memq jss-http-repl-status (list :idle :closed))
    (error "Request in progress, can't insert new request."))
  
  (setf jss-http-repl-status :closed)

  (let (endpoint-point)

    (combine-after-change-calls
      
      (jss-wrap-with-text-properties (list 'jss-http-repl-endpoint t)
        (insert "Endpoint:"))

      (setf endpoint-point (point))
      
      (jss-wrap-with-text-properties (list 'read-only t 'face 'jss-http-repl-meta-data-face 'jss-header-end-marker t)
        (insert "--request data follows this line--"))
      
      (jss-wrap-with-text-properties (list 'jss-header-end-marker t 'rear-nonsticky t)
        (let ((inhibit-read-only t))
          (insert "\n")))

      (jss-http-repl-set-endpoint :url url :method method :http-version http-version)
      (when header-string
        (if jss-http-repl-set-cookies
            (progn
              (jss-http-repl-set-headers header-string :extra-headers (mapcar (lambda (cookie)
                                                                                (cons "Cookie" cookie))
                                                                              jss-http-repl-set-cookies))
              (setf jss-http-repl-set-cookies '()))
          (jss-http-repl-set-headers header-string)))
      (when data-string
        (jss-http-repl-set-request-data data-string)))
    
    (goto-char endpoint-point)
    (end-of-line)))

(defvar jss-http-repl-request-method-regexp
  "\\(GET\\|POST\\|HEAD\\|PUT\\|DELETE\\|TRACE\\|CONNECT\\)")

(defvar jss-http-repl-request-line-regexp
  (concat "^" jss-http-repl-request-method-regexp "\\s-+\\(.*\\)\\s-+\\(HTTP/1\\.[10]\\)\\s-*$"))

(defun jss-http-repl-goto-header-start ()
  (let ((endpoint (jss-find-property-block 'jss-http-repl-endpoint t)))
    (goto-char (car endpoint))
    (forward-line 1)
    (beginning-of-line)
    (point)))

(defun jss-http-repl-goto-header-end ()
  (let ((endpoint (jss-find-property-block 'jss-http-repl-endpoint t)))
    (goto-char (car endpoint))
    (while (not (get-text-property (point) 'jss-header-end-marker))
      (when (eobp)
        (error "Looking for header end but got to end of buffer."))
      (forward-char 1))
    (point)))

(defun jss-http-repl-goto-data-start ()
  (jss-http-repl-goto-header-end)
  (block nil
    (while (get-text-property (point) 'jss-header-end-marker)
      (when (eobp)
        (return))
      (forward-char 1)))
  (point))

(defvar jss-http-repl-request-header-editors
  '("Accept" "Accept-Charset" "Accept-Encoding" "Accept-Language" "Accept-Datetime"
    ("Authorization" . jss-http-repl-read-authorization)
    ("Cache-Control" . jss-http-repl-read-cache-control)
    "Connection"
    "Cookie"
    ("Content-Length" . jss-http-repl-read-content-length) "Content-MD5"
    ("Content-Type" . jss-http-repl-read-content-type)
    "Date"
    "Expect"
    "From"
    "Host"
    "If-Match" "If-Modified-Since" "If-None-Match" "If-Range" "If-Unmodified-Since"
    "Max-Forwards"
    "Pragma"
    "Proxy-Authorization"
    "Range"
    "Referer"
    "TE"
    "Upgrade"
    ("User-Agent" . jss-http-repl-choose-user-agent)
    "Via"
    "Warning"
    "X-Requested-With"))

(defun jss-http-repl-ensure-header ()
  "If there already is a header named `header-name` then simple
moves to it, otherwise inserts it. Will prompt, with completion,
for the name of the header to add."
  (interactive)
  (let* ((header-name (jss-completing-read "Header: "
                                           (sort (mapcar (lambda (header)
                                                           (if (consp header) (car header) header))
                                                         jss-http-repl-request-header-editors)
                                                 'string<)
                                           :require-match nil))
         (header-value (jss-http-repl-request-read-header-value header-name)))
    (jss-http-repl-set-header header-name header-value)))

(defun jss-http-repl-set-header (header-name header-value)
  "Sets the value of `header-name` to `header-value'. If value is
NIL leaves point after the newly inserted ?: char, if value is a
string inserts it and moves to the next line. If a header named
header-name already exists overwrites it."
  (let ((inhibit-read-only t))
    (when (jss-http-repl-goto-header header-name)
      (delete-region (line-beginning-position) (line-end-position))
      (when (and (eolp)
                 (bolp)
                 (not (eobp)))
        ;; on an empty line
        (delete-char 1)))
    (insert header-name ": ")
    (when header-value
      (insert header-value))
    (insert "\n")
    (jss-http-repl-goto-header header-name)))

(defun jss-http-repl-header-location (header-name)
  "If there is a header named `header-name` in the current
request then returns a cons of (start . end), the points in the
buffer where the header line starts and ends, otherwise returns
nil."
  (save-match-data
    (save-excursion
      (jss-http-repl-goto-header-start)
      (block nil
        (while (not (jss-http-repl-in-header-line header-name))
          (forward-line 1)
          (when (looking-at "--request data follows this line--")
            (return nil))
          (when (eobp)
            (error "Buffer ended before be could find the header. Is the --request data-- line missing?")
            (return)))
        (cons (line-beginning-position) (line-end-position))))))

(defun jss-http-repl-goto-header (header-name)
  "If there is a header named `header-name` in the current
request then moves point to after its ?: char and returns t,
otherwise leaves point at the last line of the headers (which a
simple insert is enough to insert a new header) and returns nil"
  (let ((location (jss-http-repl-header-location header-name)))
    (if location
        (progn
          (goto-char (car location))
          (jss-http-repl-gopast-header header-name)
          t)
      (jss-http-repl-goto-header-end)
      nil)))

(defun jss-http-repl-header-value (header-name)
  (save-excursion
    (if (jss-http-repl-goto-header header-name)
        (buffer-substring-no-properties (point) (line-end-position))
      nil)))

(defun jss-http-repl-in-header-line (name)
  (save-excursion
    (beginning-of-line)
    (looking-at (concat "^" (regexp-quote name) ":"))))

(defun jss-http-repl-looking-at-header-p (header-name)
  (looking-at (concat "^" (regexp-quote header-name) "\\(:\\)")))

(defun jss-http-repl-gopast-header (name)
  (beginning-of-line)
  (save-match-data
    (if (jss-http-repl-looking-at-header-p name)
        (progn
          (goto-char (match-end 1))
          (while (and (not (eolp)) (looking-at "\\s-"))
            (forward-char 1)))
      (error "Not looking at the header %s." name))))

(defun jss-http-repl-request-header-editor (header-name)
  (dolist (h jss-http-repl-request-header-editors)
    (cond
     ((and (consp h)
           (string= header-name (car h)))
      (return (cdr h)))
     ((and (stringp h)
           (string= h header-name))
      (return nil)))))

(defun jss-http-repl-request-read-header-value (header-name)
  (let ((reader (jss-http-repl-request-header-editor header-name)))
    (if reader
        (funcall reader)
      nil)))

(defvar jss-http-repl-user-agents
  '(("IE 6" . "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1)")
    ("IE 7" . "Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 6.0)")
    ("IE 8" . "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 6.1)")
    ("Googlebot" . "Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")
    ("Google Chrome 24/Mac OS X" . "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_2) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1309.0 Safari/537.17")
    ("Firefox" "Mozilla/5.0 (Windows NT 6.2; Win64; x64; rv:16.0.1) Gecko/20121011 Firefox/16.0.1")
    ("Safari" "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_6_8) AppleWebKit/537.13+ (KHTML, like Gecko) Version/5.1.7 Safari/534.57.2")))

(defun jss-http-repl-choose-user-agent ()
  (interactive)
  (let* ((user-agent-name (jss-completing-read "Browser: " (mapcar 'car jss-http-repl-user-agents)
                                               :require-match nil))
         (known-value (cl-assoc user-agent-name jss-http-repl-user-agents :test 'string=)))
    (if known-value
        (cdr known-value)
      ;; if the user typed in their own user agent string then we
      ;; won't find it in jss-http-repl-user-agents and we just
      ;; use whatever they sent.
      user-agent-name)))

(defun jss-http-repl-read-cache-control ()
  (interactive)
  "no-cache")

(defun jss-http-repl-read-content-length ()
  (interactive)
  (save-excursion
    (jss-completing-read "Content-Length: " (list (format "%d" (string-bytes
                                                                (buffer-substring-no-properties (jss-http-repl-goto-data-start)
                                                                                                (point-max)))))
                         :require-match nil)))

(defcustom jss-http-repl-content-types '(("x-www-form-urlencoded" . "application/x-www-form-urlencoded; charset=us-ascii")
                                         ("json" . "application/json; charset=utf-8")
                                         ("text/plain" . "text/plain"))
  "Predefined list of content types, an alist of label (used only to simplify input) and full value strings."
  :type 'list
  :group 'jss)

(defun jss-http-repl-read-content-type ()
  (interactive)
  (let* ((type (jss-completing-read "Content-Type: " (mapcar 'car jss-http-repl-content-types)
                                    :require-match nil))
         (type-string (jss-if-bind (value-cons (cl-assoc type jss-http-repl-content-types :test 'string=))
                        (cdr value-cons)
                        type)))
    
    (save-match-data
      (unless (string-match ".*;\\s-*charset=\\([a-zA-Z0-9-]+\\)" type-string)
        (when (jss-yes-or-no-p "No charset detected, add utf-8? ")
          (setf type-string (concat type-string "; charset=utf-8")))))

    type-string))

(defun jss-http-repl-request-encoding ()
  (save-excursion
    (save-match-data
      (jss-if-bind (Content-Type (jss-http-repl-header-value "Content-Type"))
          (if (string-match ".*;\\s-*charset=\\([-a-zA-Z0-9]+\\)" Content-Type)
              (let ((charset (downcase (match-string-no-properties 1 Content-Type))))
                (cond
                 ((string= "utf-8" charset) 'utf-8-unix)
                 ((string= "us-ascii" charset) 'us-ascii-unix)
                 (t (error "Unrecognized charset in %s" Content-Type))))
            nil)
        nil))))

(defun jss-http-repl-read-authorization (username password)
  (interactive (list (read-from-minibuffer "Username: ")
                     (read-from-minibuffer "Password: ")))
  (concat "Basic " (base64-encode-string (format "%s:%s" username password))))

(defun jss-http-repl-delete-headers ()
  (jss-http-repl-goto-header-start)
  (delete-region (point) (jss-http-repl-goto-header-end)))

(defun jss-http-repl-looking-at-newline ()
  (and (char-after (point))
       (char-equal ?\r (char-after (point)))
       (char-after (1+ (point)))
       (char-equal ?\n (char-after (1+ (point))))))

(defun jss-chars-to-string (&rest chars)
  (apply 'concat (mapcar 'char-to-string chars)))

(defun* jss-http-repl-set-headers (header-string &key extra-headers)
  (delete-region (jss-http-repl-goto-header-start)
                 (jss-http-repl-goto-header-end))
  ;; the above delete-region leaves point at the beginning of the header block
  (setf header-string (replace-regexp-in-string (jss-chars-to-string #x0d #x0a #x0d #x0a ?\\ ?')
                                                (jss-chars-to-string #x0a)
                                                header-string))
  (setf header-string (replace-regexp-in-string (jss-chars-to-string #x0d #x0a)
                                                (jss-chars-to-string #x0a)
                                                header-string))
  (insert header-string)
  (loop
   for (name . value) in extra-headers
   do (insert name ": " value "\n"))
  (jss-http-repl-goto-header-start)
  (when (looking-at jss-http-repl-request-method-regexp)
    (add-text-properties (point) (line-end-position) (list 'jss-http-repl-auto-Request-Line t)))
  (point))

(defun jss-http-repl-request-data ()
  (save-excursion
    (jss-http-repl-goto-data-start)
    (buffer-substring-no-properties (point) (point-max))))

(defun jss-http-repl-set-request-data (data-string)
  (jss-http-repl-goto-data-start)
  (delete-region (point) (point-max))
  (when data-string
    (insert data-string)))

(defun* jss-http-repl-set-endpoint (&key url method http-version)
  (let ((endpoint-location (jss-find-property-block 'jss-http-repl-endpoint t)))
    (let ((inhibit-read-only t))
      (goto-char (car endpoint-location))
      (delete-region (car endpoint-location) (cdr endpoint-location))
      (jss-wrap-with-text-properties (list 'rear-nonsticky t
                                           'jss-http-repl-endpoint t
                                           'read-only t
                                           'face 'jss-http-repl-meta-data-face)
        (insert "Endpoint: "))
      (insert (or method "GET") " ")
      (when url
        (insert url))
      (insert "\n"))))

(defvar jss-http-repl-endpoint-regexp
  (concat "Endpoint:\\s-*" jss-http-repl-request-method-regexp "\\s-+\\([^ \t]+?\\)\\(\\s-+HTTP/1\\.[01]\\)?\\s-*$")
  "Regexp which matches at the beginning of an endpoint line and binds, in two groups, the method and url.")

(defun* jss-http-repl-get-endpoint ()
  (let ((endpoint-location (jss-find-property-block 'jss-http-repl-endpoint t)))
    (goto-char (car endpoint-location))
    (save-match-data
      (if (looking-at jss-http-repl-endpoint-regexp)
          (let* ((method (match-string-no-properties 1))
                 (url-string (match-string-no-properties 2))
                 (http-version (match-string-no-properties 3))
                 (url (url-generic-parse-url url-string))
                 (url-host (url-host url))
                 (url-schema (url-type url))
                 (url-port (url-port url))) 
            (list :method method
                  :http-version http-version
                  :url url-string
                  :host url-host
                  :schema (or url-schema "http")
                  :port (cond
                         (url-port url-port)
                         ((or (null url-schema)
                              (string= "http" url-schema))
                          80)
                         ((string= "https" url-schema)
                          443)
                         (t
                          (error "Can't infere remote port from %s." url)))))
        nil))))

(defun jss-http-repl-update-inferred-headers ()
  (when (looking-at jss-http-repl-endpoint-regexp)
    (let* ((inhibit-read-only t)

           (method (match-string-no-properties 1))
           ;; nb: url-generic-parse-url will change match data :(
           (url    (match-string-no-properties 2))
           (http-version (match-string-no-properties 3))
           (url    (url-generic-parse-url url)))
      (jss-http-repl-goto-header-start)

      (when (or (not (looking-at jss-http-repl-request-method-regexp))
                (get-text-property (point) 'jss-http-repl-auto-Request-Line))
        (when (looking-at jss-http-repl-request-method-regexp)  
          (delete-region (line-beginning-position) (1+ (line-end-position))))
        (jss-wrap-with-text-properties (list 'jss-http-repl-auto-Request-Line t)
          (insert method)
          (if (url-filename url)
              (insert " "
                      (if (string= (url-filename url) "")
                          "/"
                        (url-filename url))
                      " "))
          (insert (or http-version "HTTP/1.1"))
          (insert "\n")))
      
      ;; as the Request-Line stuff above may have moved the
      ;; auto-host-line it's important to call
      ;; find-property-block not earlier than now.
      (let ((auto-host-line (jss-find-property-block 'jss-http-repl-auto-host-line t :error nil)))
        (if auto-host-line
            (progn
              (goto-char (car auto-host-line))
              (delete-region (car auto-host-line) (cdr auto-host-line))
              (when (url-host url)
                (jss-wrap-with-text-properties (list 'jss-http-repl-auto-host-line t)
                  (insert "Host: " (url-host url) "\n"))))
          (unless (jss-http-repl-header-location "Host")
            (when (url-host url)
              (jss-wrap-with-text-properties (list 'jss-http-repl-auto-host-line t)
                (insert "Host: " (url-host url) "\n")))))))))

(defun jss-http-repl-after-change-function (change-start change-end previous-length)
  (interactive)
  (save-excursion
    (save-match-data
      (block nil
        (goto-char change-start)
        
        (when (get-text-property (line-beginning-position) 'jss-http-repl-endpoint)
          (beginning-of-line)
          (return (jss-http-repl-update-inferred-headers)))

        (when (jss-http-repl-in-header-line "Host")
          (let ((loc (jss-find-property-block 'jss-http-repl-auto-host-line t :error nil)))
            (when loc
              (let ((inhibit-read-only t))
                (remove-text-properties (car loc) (cdr loc) (list 'jss-http-repl-auto-host-line t))))))
        
        (when (<= change-end (point))
          (return))
        
        (forward-line 1)))))

(defun jss-http-repl-preflight ()
  (unless (memq jss-http-repl-status '(:idle :closed))
    (error "Can't prepared a new request unless we're in editing mode. current mode is %s." jss-http-repl-status))
  (let* ((data-string (buffer-substring-no-properties (jss-http-repl-goto-data-start)
                                                      (point-max)))
         (data-bytes nil)
         (endpoint (jss-http-repl-get-endpoint))
         (request-start (car (jss-find-property-block 'jss-http-repl-endpoint t)))
         (submitted-overlay (make-overlay request-start (point-max))))

    (unless endpoint
      (error "Missing endpoint data, don't where to send request."))

    (when (string= "POST" (cl-getf endpoint :method))

      (unless (jss-http-repl-header-value "Content-Type")
        (when (jss-yes-or-no-p "No Content-Type header found. Do you want to add one? ")
          (jss-http-repl-goto-header-end)
          (insert "Content-Type: " (jss-http-repl-read-content-type) "\n")))

      (let ((effective-encoding nil))
        
        (jss-if-bind (encoding (jss-http-repl-request-encoding))
            (setf effective-encoding encoding)
          (warn "No encoding found, will assume utf-8-unix.")
          (setf effective-encoding 'utf-8-unix))

        (setf data-bytes (encode-coding-string data-string effective-encoding))
        
        (let* ((overwrite-content-length nil)
               (data-binary-length (length data-bytes))
               (content-length-string (jss-http-repl-header-value "Content-Length"))
               (content-length (and content-length-string
                                    (jss-parse-integer content-length-string))))

          (cond
           ((null content-length-string)
            (when (jss-yes-or-no-p "No Content-Length header found, add one? ")
              (setf overwrite-content-length t)))
           ((not content-length)
            (when (jss-yes-or-no-p "No Content-Length header's value is not a number. Reset? ")
              (setf overwrite-content-length t)))
           ((not (= content-length data-binary-length))
            (when (jss-yes-or-no-p "No Content-Length header's value does not match current data. Reset? ")
              (setf overwrite-content-length t))))

          (when overwrite-content-length
            (jss-http-repl-set-header "Content-Length" (format "%d" data-binary-length) )))))
    
    (overlay-put submitted-overlay 'face 'jss-http-repl-submitted-face)
    
    (let ((inhibit-read-only t)
          ;; get the headers at the very end since we may have changed them.
          (header-string (buffer-substring-no-properties (jss-http-repl-goto-header-start)
                                                         (jss-http-repl-goto-header-end))))
      (remove-text-properties request-start (point-max)
                              (list 'jss-http-repl-endpoint t
                                    'jss-header-end-marker t
                                    'rear-nonsticky t
                                    'jss-http-repl-Request-Line t))
      (add-text-properties request-start (point-max) (list 'read-only t))
      
      (list* :ssl (string= "https" (cl-getf endpoint :schema))
             :header-string header-string
             :data-string data-string
             :data-bytes data-bytes
             endpoint))))

(defun jss-http-repl-send-request ()
  (interactive)
  (apply 'jss-request-submit (jss-http-repl-preflight)))

(defun* jss-request-submit (&rest request-data)
  (setf jss-http-repl-previous-request-data request-data
        jss-http-repl-status :opening)

  (destructuring-bind (&key host port ssl &allow-other-keys)
      request-data
    (make-network-process :name "jss-http-repl-request"
                          :server nil
                          :host host
                          :service port
                          :nowait t
                          :coding '(binary . binary)
                          :filter 'jss-http-repl-process-filter
                          :sentinel 'jss-http-repl-process-sentinel
                          :buffer (current-buffer))))

(defun jss-http-repl-process-send-data (proc)
  (goto-char (point-max))
  (let ((inhibit-read-only t))
    (jss-wrap-with-text-properties (list 'face 'jss-http-repl-meta-data-face
                                         'read-only t)
      (unless (= (point) (line-beginning-position))
        (insert "\n"))
      (insert "--response headers follow this line--\n")))
  (setf jss-http-repl-status :sending)
  (destructuring-bind (&key header-string data-bytes &allow-other-keys)
      jss-http-repl-previous-request-data
    (process-send-string proc (encode-coding-string header-string 'us-ascii-dos))
    (process-send-string proc (jss-chars-to-string #x0d #x0a))
    (when data-bytes
      (process-send-string proc data-bytes))
    (setf jss-http-repl-status :receiving-headers)))

(defun jss-http-repl-process-sentinel (proc status)
  (with-current-buffer (process-buffer proc)
    (cond
     ((string= "open\n" status)
      (jss-http-repl-process-send-data proc))
     ((cl-member status '("connection broken by remote peer\n" "deleted\n") :test 'string=)
      (message "Connection closed.")
      (setf jss-http-repl-status :closed
            jss-http-repl-keep-alive nil)
      (when (memq jss-http-repl-status '(:receiving-headers :receiving-data))
        (jss-http-repl-insert-next-request)))
     (t
      (message "%s got unknown sentinel status %s." proc status)))))

(defun jss-http-repl-insert-next-request ()
  (let ((inhibit-read-only t))
    (insert "\n")
    (destructuring-bind (&key header-string data-string url method http-version &allow-other-keys)
        jss-http-repl-previous-request-data
      ;; just do this d-bind to ignore those element of
      ;; jss-http-repl-previous-request-data we don't need at this
      ;; point
      (jss-http-repl-insert-request :header-string header-string
                                    :data-string data-string
                                    :url url
                                    :method method
                                    :http-version http-version)))
  (setf jss-http-repl-previous-request-data '()))

(defun jss-http-repl-process-filter (proc output)
  (unless (memq jss-http-repl-status '(:receiving-headers :receiving-data))
    (error "Process filter got unexpected data: %s (%s)" output jss-http-repl-status))
  (with-current-buffer (process-buffer proc)
    (save-match-data
      (goto-char (point-max))
      (let ((inhibit-read-only t)
            (start (point)))
        (jss-wrap-with-text-properties (list 'read-only t)
          (insert output))
        (when (eql :receiving-headers jss-http-repl-status)
          (goto-char start)
          (beginning-of-line)
          (block nil
            (while (not (eobp))
              (when (looking-at "Connection:\\s-*\\(.*\\)\\s-*$")
                (setf jss-http-repl-keep-alive (string= "keep-alive" (downcase (match-string-no-properties 1)))))
              (when (looking-at "Content-Length:\\s-*\\([0-9]+\\)\\s-*$")
                (setf jss-http-repl-content-length (jss-parse-integer (match-string-no-properties 1))))
              (when (and jss-http-repl-track-cookies
                         (looking-at (concat "Set-Cookie:\\s-*\\(.*\\)" (jss-chars-to-string #x0d #x0a))))
                (push (match-string-no-properties 1) jss-http-repl-set-cookies))
              (when (looking-at (jss-chars-to-string #x0d #x0a))
                (delete-region (1- (point)) (+ 2 (point)))
                (jss-wrap-with-text-properties (list 'read-only t
                                                     'face 'jss-http-repl-meta-data-face)
                  (insert "\n--response data follows this line--\n"))
                (setf jss-http-repl-status :receiving-data
                      jss-http-repl-response-data-start (point))
                (return))
              (forward-line 1))))
        (when (eql :receiving-data jss-http-repl-status)
          (goto-char (point-max))
          (when (and jss-http-repl-response-data-start
                     jss-http-repl-content-length
                     (= jss-http-repl-content-length (- (point) jss-http-repl-response-data-start)))
            (setf jss-http-repl-status :idle)))
        (when (eql :idle jss-http-repl-status)
          (jss-http-repl-insert-next-request))))))

(provide 'jss-http-repl)
