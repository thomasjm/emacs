;;; stack-mode.el --- A minor mode enabling various features based on
;;; stack-ide.

;; Copyright (c) 2015 Chris Done.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Imports

(require 'haskell-mode)
(require 'cl-lib)
(require 'fifo)
(require 'checklist)
(require 'flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes

(define-minor-mode stack-mode
  "A minor mode enabling various features based on stack-ide.

Automatically starts and stops flycheck-mode when you
enable/disable it. It makes this assumption in the interest of
easier user experience. Disable with `stack-mode-manage-flycheck'."
  :lighter " Stack"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-.") 'stack-mode-goto)
            (define-key map (kbd "C-c C-k") 'stack-mode-clear)
            (define-key map (kbd "C-c C-t") 'stack-mode-type)
            (define-key map (kbd "C-c C-i") 'stack-mode-info)
            (define-key map (kbd "C-c C-l") 'stack-mode-load)
            map)
  (when (buffer-file-name)
    (if stack-mode
        (when stack-mode-manage-flycheck
          (flycheck-mode 1)
          (flycheck-select-checker 'stack-ide))
      (when stack-mode-manage-flycheck
        (flycheck-mode -1)))))

(define-derived-mode inferior-stack-mode fundamental-mode "Inferior-Stack-IDE"
  "Major mode for interacting with an inferior stack-ide process.")

(define-key inferior-stack-mode-map (kbd "C-c C-c") 'stack-mode-stop)
(define-key inferior-stack-mode-map (kbd "C-c C-k") 'stack-mode-clear)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization

(defgroup stack-mode nil
  "IDE backend support for Haskell."
  :group 'haskell)

(defcustom stack-mode-proc-path
  "stack"
  "Path to the stack executable."
  :type 'string
  :group 'stack-mode)

(defcustom stack-mode-manage-flycheck
  t
  "Automatically start and stop flycheck when the minor mode is
enabled/disabled."
  :type 'boolean
  :group 'stack-mode)

(defcustom stack-mode-print-error-messages
  nil
  "Print error messages after loading the project?"
  :type 'boolean
  :group 'stack-mode)

(defvar stack-mode-queue nil)
(make-variable-buffer-local 'stack-mode-queue)

(defvar stack-mode-buffer nil)
(make-variable-buffer-local 'stack-mode-buffer)

(defvar stack-mode-name nil)
(make-variable-buffer-local 'stack-mode-name)

(defvar stack-mode-tried-to-start nil)
(make-variable-buffer-local 'stack-mode-tried-to-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions

(defun stack-mode-status ()
  "Print the status of the current stack process."
  (interactive)
  (if (stack-mode-buffer)
      (if (stack-mode-process)
          (if (process-live-p (stack-mode-process))
              (message "The process is live.")
            (message "There is a Stack process, but it's dead."))
        (message "There is a stack buffer, but no Stack process."))
    (message "There is no Stack buffer.")))

(defun stack-mode-start ()
  "Start an inferior process and buffer."
  (interactive)
  (if (stack-mode-live-p)
      (switch-to-buffer (stack-mode-buffer))
    (with-current-buffer (stack-mode-buffer)
      (setq buffer-read-only t)
      (inferior-stack-mode)
      (stack-mode-set-initial-command)
      (setq stack-mode-buffer "")
      (let* ((project-name (stack-mode-name))
             (name (stack-mode-process-name project-name))
             (args (append (list name
                                 nil
                                 stack-mode-proc-path
                                 "ide"
                                 "start")
                           (list project-name)))
             (process (or (get-process name)
                          (progn (stack-mode-log "Starting: %S" args)
                                 (apply #'start-process
                                        args)))))
        (set-process-sentinel process 'stack-mode-sentinel)
        (set-process-filter process 'stack-mode-filter)))))

(defun stack-mode-set-initial-command ()
  "Set the initial command callback. The `stack ide` command will
reload targets on start-up, so that's the default command we'll
start with."
  (setq stack-mode-current-command
        (list :json nil
              :data nil
              :cont 'stack-mode-loading-callback))
  (setq stack-mode-queue (fifo-make))
  (message "Set initial command."))

(defun stack-mode-stop ()
  "Stop the process."
  (interactive)
  (with-current-buffer (stack-mode-buffer)
    (when (stack-mode-process)
      (setq stack-mode-current-command nil)
      (setq stack-mode-buffer "")
      (kill-process (stack-mode-process))
      (delete-process (stack-mode-process)))))

(defun stack-mode-reset ()
  "Reset the process."
  (interactive)
  (with-current-buffer (stack-mode-buffer)
    (when (stack-mode-process)
      (setq stack-mode-current-command nil)
      (setq stack-mode-buffer "")
      (setq stack-mode-queue (fifo-make)))))

(defun stack-mode-restart ()
  "Restart the process with a fresh command queue."
  (interactive)
  (stack-mode-stop)
  (stack-mode-start))

(defun stack-mode-live-p ()
  "Is the process alive?"
  (and (stack-mode-process)
       (process-live-p (stack-mode-process))))

(defun stack-mode-clear ()
  "Clear the interaction buffer."
  (interactive)
  (with-current-buffer (stack-mode-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun stack-mode-load ()
  "Load the current buffer's file."
  (interactive)
  (save-buffer)
  (with-current-buffer (stack-mode-buffer)
    (stack-mode-reload)))

(defun stack-mode-goto ()
  "Go to definition of thing at point."
  (interactive)
  (let ((filename (buffer-file-name))
        (module-name (haskell-guess-module-name))
        (span (stack-mode-span)))
    (let* ((span-info
            (stack-mode-get-span-info
             module-name
             (with-current-buffer (stack-mode-buffer)
               (file-relative-name filename default-directory))
             span))
           (infos
            (stack-contents
             span-info))
           (_ (when (and (vectorp infos) (= 0 (length infos)))
                (error "Couldn't find location for this. Is the module loaded in the backend?
Run `M-x stack-mode-list-loaded-modules' to see what's loaded.")))
           (parts (mapcar #'identity (elt infos 0)))
           (info (stack-contents (elt parts 0)))
           (span (elt parts 1))
           (scope (stack-lookup 'tag (stack-lookup 'idScope info)))
           (def-span (stack-lookup-contents
                      'idDefSpan
                      (stack-lookup 'idProp info))))
      (cond
       ((listp def-span)
        (stack-mode-goto-span def-span))
       (t
        (let* ((imported-from
                (stack-lookup
                 'idImportedFrom
                 (stack-lookup 'idScope info)))
               (imported-module (stack-lookup 'moduleName imported-from))
               (defined-in (stack-lookup
                            'idDefinedIn
                            (stack-lookup 'idProp info)))
               (package (stack-lookup 'modulePackage defined-in))
               (package-name (stack-lookup 'packageName package))
               (package-ver (stack-lookup 'packageVersion package))
               (module (stack-lookup 'moduleName defined-in)))
          (message "Imported via %s, defined in %s (%s-%s)"
                   (haskell-fontify-as-mode imported-module 'haskell-mode)
                   (haskell-fontify-as-mode module 'haskell-mode)
                   package-name
                   package-ver)))))))

(defun stack-mode-list-loaded-modules ()
  "List the loaded modules in the backend."
  (interactive)
  (let ((modules
         (stack-contents
          (with-current-buffer (stack-mode-buffer)
            (stack-mode-call
             `((tag . "RequestGetLoadedModules")
               (contents
                . [])))))))
    (pop-to-buffer (stack-mode-buffer))
    (stack-mode-log "Loaded modules: %s"
                    (mapconcat #'identity
                               (sort (mapcar #'identity modules) #'string<)
                               "\n"))))

(defun stack-mode-info ()
  "Display the info of the thing at point."
  (interactive)
  (let* ((filename (buffer-file-name))
         (module-name (haskell-guess-module-name))
         (points (stack-mode-points))
         (orig (point))
         (span (stack-mode-span-from-points (car points)
                                            (cdr points)))
         (info (stack-mode-get-span-info
                module-name
                (with-current-buffer (stack-mode-buffer)
                  (file-relative-name filename default-directory))
                span))
         (info-contents (stack-contents (elt (elt (stack-contents info) 0) 0)))
         (scope (stack-lookup 'idScope info-contents))
         (prop (stack-lookup 'idProp info-contents))
         (qual (stack-lookup 'idImportQual scope))
         (from (stack-lookup 'idImportedFrom scope))
         (span (stack-lookup 'idImportSpan scope))

         (space (stack-lookup 'idSpace prop))
         (idDefSpan (stack-lookup 'idDefSpan prop))
         (idDefinedIn (stack-lookup 'idDefinedIn prop))
         (modulePackage (stack-lookup 'modulePackage idDefinedIn))
         (moduleName (stack-lookup 'moduleName idDefinedIn))
         (packageVersion (stack-lookup 'packageVersion modulePackage))
         (packageKey (stack-lookup 'packageKey modulePackage))
         (packageName (stack-lookup 'packageKey modulePackage))
         (idType (stack-lookup 'idType prop))
         (idName (stack-lookup 'idName prop)))
    (message
     (concat
      "Identifier: " (haskell-fontify-as-mode idName 'haskell-mode) "\n"
      "Type: " (haskell-fontify-as-mode idType 'haskell-mode) "\n"
      "Module: " (haskell-fontify-as-mode moduleName 'haskell-mode) "\n"
      "Package: "  (if (string= "main" packageName)
                       "(this one)"
                     packageName)))))

(defun stack-mode-type (&optional insert-value)
  "Display type info of thing at point."
  (interactive "P")
  (let* ((filename (buffer-file-name))
         (module-name (haskell-guess-module-name))
         (points (stack-mode-points))
         (orig (point))
         (span (stack-mode-span-from-points (car points)
                                            (cdr points))))
    (let* ((types (stack-contents
                   (stack-mode-get-exp-types
                    module-name
                    (with-current-buffer (stack-mode-buffer)
                      (file-relative-name filename default-directory))
                    span)))
           (types (mapcar #'identity types))
           (code (buffer-substring-no-properties
                  (car points)
                  (cdr points)))
           (type (stack-contents (car types)))
           (ty (stack-lookup 'text type)))
      (if insert-value
          (let ((ident-pos (haskell-ident-pos-at-point)))
            (cond
             ((region-active-p)
              (delete-region (region-beginning)
                             (region-end))
              (insert "(" code " :: " ty ")")
              (goto-char (1+ orig)))
             ((= (line-beginning-position) (car ident-pos))
              (goto-char (line-beginning-position))
              (insert code " :: " (haskell-fontify-as-mode ty 'haskell-mode)
                      "\n"))
             (t
              (save-excursion
                (goto-char (car ident-pos))
                (let ((col (current-column)))
                  (save-excursion (insert "\n")
                                  (indent-to col))
                  (insert code " :: " (haskell-fontify-as-mode ty 'haskell-mode)))))))
        (unless (null types)
          (message
           "%s"
           (mapconcat (lambda (type)
                        (haskell-fontify-as-mode
                         (concat
                          code
                          " :: "
                          (elt type 0))
                         'haskell-mode))
                      (cl-subseq types 0 1)
                      "\n")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Process filters and sentinel

(defun stack-mode-filter (process response)
  (with-current-buffer (stack-mode-buffer (stack-mode-name-from-process process))
    (if stack-mode-current-command
        (let* ((lines (split-string (concat stack-mode-buffer response) "\n")))
          (setq stack-mode-buffer (car (last lines)))
          (setq lines (butlast lines))
          (let ((data (plist-get stack-mode-current-command :data))
                (cont (plist-get stack-mode-current-command :cont)))
            (while lines
              (let ((line (pop lines)))
                (stack-mode-log
                 "<- %s"
                 (haskell-fontify-as-mode line 'javascript-mode))
                (when (let* ((error-msg nil)
                             (json (condition-case e
                                       (json-read-from-string line)
                                     (error "Problem reading JSON from server, probably an error message:\n%s" line)))
                             (ret (condition-case e
                                      (funcall cont data json)
                                    (error (setq error-msg e)
                                           :error))))
                        (cl-ecase ret
                          (:done t)
                          (:continue nil)
                          (:error
                           (setq stack-mode-buffer "")
                           (setq stack-mode-current-command nil)
                           (setq stack-mode-queue nil)
                           (error "Command handler error: %S\n\nThe command queue has been cleared."
                                  error-msg))
                          (t
                           (error "A command handler must return either :done or :continue,
but it returned: %S
command was: %S" ret stack-mode-current-command))))
                  (cl-loop for line in lines
                           do (stack-mode-log
                               "Extraneous lines after command completed: %s"
                               (haskell-fontify-as-mode line 'javascript-mode)))
                  (setq stack-mode-current-command nil)
                  (setq lines nil)
                  (stack-mode-queue-trigger))))))
      (stack-mode-log "Ignoring: %s"
                      (haskell-fontify-as-mode response 'javascript-mode)))))

(defun stack-mode-sentinel (process event)
  (with-current-buffer (stack-mode-buffer (stack-mode-name-from-process process))
    (stack-mode-log "Process event: %s" event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command queue

(defvar stack-mode-current-command nil
  "Current command handler.")
(make-variable-buffer-local 'stack-mode-current-command)

(defvar stack-mode-buffer ""
  "A buffer for the process.")
(make-variable-buffer-local 'stack-mode-buffer)

(defvar stack-mode-queue nil
  "Command queue.")
(make-variable-buffer-local 'stack-mode-queue)

(defun stack-mode-queue ()
  "Get the FIFO queue of this process."
  (or stack-mode-queue
      (setq stack-mode-queue (fifo-make))))

(defun stack-mode-enqueue (json data cont)
  "Enqueue a JSON command to the command queue, calling (CONT
DATA line) for each response line until CONT returns nil."
  (cond
   ((stack-mode-live-p)
    (stack-mode-log "-> %s" (haskell-fontify-as-mode (json-encode json) 'javascript-mode))
    (fifo-push (stack-mode-queue)
               (list :json json :data data :cont cont))
    (stack-mode-queue-trigger))
   (t (stack-mode-try-start))))

(defun stack-mode-try-start ()
  "Try to start, but only try once."
  (cond
   ((not stack-mode-tried-to-start)
    (setq stack-mode-tried-to-start t)
    (message "Starting a Stack IDE backend process for this project: %s, stack directory: %s"
             (stack-mode-cabal-name)
             (stack-mode-dir))
    (stack-mode-start))
   (t (message "Attempted to run a Stack IDE command, but the server isn't started. We already tried once this session. Run `M-x stack-mode-restart` to confirm that you want to start it."))))

(defun stack-mode-call (json)
  "Call a JSON command. Wait for any existing queued commands to
complete, then sends the request, blocking on the
response. Returns the response."
  (let ((data (list nil)))
    (stack-mode-enqueue
     json data
     (lambda (data reply)
       (setcar data reply)
       :done))
    (stack-mode-queue-flush)
    (car-safe data)))

(defun stack-mode-queue-processed-p ()
  "Return t if command queue has been completely processed."
  (and (fifo-null-p stack-mode-queue)
       (null stack-mode-current-command)))

(defun stack-mode-queue-flush ()
  "Block till PROCESS's command queue has been completely processed.
This uses `accept-process-output' internally."
  (let ((proc (stack-mode-process)))
    (while (not (stack-mode-queue-processed-p))
      (stack-mode-queue-trigger)
      (accept-process-output proc 1))))

(defun stack-mode-queue-trigger ()
  "Trigger the next command in the queue if there is no current
command."
  (unless stack-mode-current-command
    (unless (fifo-null-p (stack-mode-queue))
      (setq stack-mode-current-command
            (fifo-pop (stack-mode-queue)))
      (process-send-string
       (stack-mode-process)
       (concat (json-encode (plist-get stack-mode-current-command :json))
               "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project functions

(defun stack-mode-packages ()
  "Get packages for the Stack configuration."
  (split-string (shell-command-to-string "stack ide packages") "\n" t))

(defun stack-mode-process ()
  "Get the current process."
  (get-process (stack-mode-process-name (stack-mode-name))))

(defun stack-mode-buffer (&optional name)
  "The inferior buffer."
  (let ((default-directory (stack-mode-dir)))
    (get-buffer-create
     (stack-mode-buffer-name
      (or name
          (stack-mode-name))))))

(defun stack-mode-name-from-process (proc)
  "Get the name of the project from the process."
  (substring (process-name proc) (length "stack:")))

(defun stack-mode-process-name (name)
  "Name for the inferior process."
  (format "stack:%s"
          name))

(defun stack-mode-buffer-name (name)
  "Name for the inferior buffer."
  (format "*stack:%s*"
          name))

(defun stack-mode-dir ()
  "The directory for the project."
  (file-name-directory (haskell-cabal-find-file)))

(defun stack-mode-name ()
  "The name for the current project based on the current
directory."
  (or stack-mode-name
      (setq stack-mode-name
            (or (stack-mode-cabal-name)
                (downcase
                 (file-name-nondirectory
                  (directory-file-name
                   (file-name-directory
                    (stack-yaml-file)))))))))

(defun stack-mode-cabal-name ()
  "Get the name of the session to use, based on the cabal file."
  (let ((cabal-file (haskell-cabal-find-file)))
    (if (string-match "\\([^\\/]+\\)\\.cabal$" cabal-file)
        (let ((name (match-string 1 cabal-file)))
          (when (not (member name (stack-mode-packages)))
            (message "This cabal project “%s” isn't in your stack.yaml configuration." name))
          name)
      (progn (message "Couldn't figure out cabal file, assuming no project.")
             nil))))

(defun stack-mode-log (&rest args)
  "Log a string to the inferior buffer."
  (with-current-buffer (stack-mode-buffer)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert (apply #'format args)
              "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

(defun stack-mode-reload ()
  "Compile the code and fetch compile errors."
  (with-current-buffer (stack-mode-buffer)
    (stack-mode-enqueue
     `((tag . "RequestUpdateSession")
       (contents . []))
     nil
     'stack-mode-loading-callback)))

(defun foo () "asdf")

(defun stack-mode-load-buffer ()
  "Compile the code and fetch compile errors."
  (interactive)
  (let ((filename (buffer-file-name)))
    (with-current-buffer (stack-mode-buffer)
      (stack-mode-enqueue
       `((tag . "RequestUpdateSession")
         (contents . [((tag . "RequestUpdateTargets")
                       (contents . ((tag . "TargetsInclude")
                                    (contents . [,filename]))))]))
       nil
       'stack-mode-loading-callback))))

(defun stack-mode-get-span-info (module file span)
  "Get the span info of the given location."
  (with-current-buffer (stack-mode-buffer)
    (stack-mode-call
     `((tag . "RequestGetSpanInfo")
       (contents
        . ((spanFilePath   . ,file)
           (spanFromLine   . ,(plist-get span :sl))
           (spanFromColumn . ,(plist-get span :sc))
           (spanToLine     . ,(plist-get span :el))
           (spanToColumn   . ,(plist-get span :ec))))))))

(defun stack-mode-get-exp-types (module file span)
  "Get the type info of the given location."
  (with-current-buffer (stack-mode-buffer)
    (stack-mode-call
     `((tag . "RequestGetExpTypes")
       (contents
        . ((spanFilePath   . ,file)
           (spanFromLine   . ,(plist-get span :sl))
           (spanFromColumn . ,(plist-get span :sc))
           (spanToLine     . ,(plist-get span :el))
           (spanToColumn   . ,(plist-get span :ec))))))))

(defun stack-mode-get-use-sites (module file span)
  "Get all uses of an identifier."
  )

(defun stack-mode-get-completions (module string)
  "Get all uses of an identifier."
  )

(defun stack-mode-loading-callback (_ reply)
  "Callback for when loading modules."
  (let ((tag (stack-tag reply)))
    (cond
     ((string= tag "ResponseUpdateSession")
      (let* ((contents (stack-contents reply))
             (tag (stack-tag contents)))
        (cond
         ((string= tag "UpdateStatusProgress")
          (stack-mode-progress-callback _ reply)
          :continue)
         ((string= tag "UpdateStatusDone")
          (stack-mode-enqueue
           `((tag . "RequestGetSourceErrors")
             (contents . []))
           nil
           'stack-mode-get-source-errors-callback)
          :done)
         (t :continue))))
     (t
      :continue))))

(defun stack-mode-progress-callback (_ reply)
  "Callback for status reports. Utilized in multiple places."
  (let* ((contents (stack-contents reply))
         (update (stack-contents contents))
         (step (stack-lookup 'progressStep update))
         (total (stack-lookup 'progressNumSteps update))
         (msg (stack-lookup 'progressParsedMsg update)))
    (message "[%s/%s] %s"
             (propertize (number-to-string step) 'face 'compilation-line-number)
             (propertize (number-to-string total) 'face 'compilation-line-number)
             msg)))

(defun stack-mode-get-source-errors-callback (_ reply)
  "Handle the reply from getting source errors."
  (let ((tag (stack-tag reply)))
    (cond
     ((string= tag "ResponseGetSourceErrors")
      (let ((any-errors nil)
            (warnings 0))
        (cl-loop
         for item in (mapcar #'identity (stack-contents reply))
         do (let* ((kind (stack-lookup 'errorKind item))
                   (span (stack-contents (stack-lookup 'errorSpan item)))
                   (msg (stack-lookup 'errorMsg item))
                   (fp (stack-lookup 'spanFilePath span))
                   (sl (stack-lookup 'spanFromLine span))
                   (sc (stack-lookup 'spanFromColumn span))
                   (el (stack-lookup 'spanToLine span))
                   (ec (stack-lookup 'spanToColumn span)))
              (cond ((string= kind "KindError")
                     (setq any-errors t))
                    ((string= kind "KindWarning")
                     (setq warnings (1+ warnings))))
              (when
                  stack-mode-print-error-messages
                (message "%s"
                         (propertize
                          (format "%s:(%d,%d)-(%d,%d): \n%s"
                                  fp sl sc el ec msg)
                          'face
                          (cond
                           ((string= kind "KindEarning")
                            'compilation-warning)
                           ((string= kind "KindError")
                            'compilation-error)))))))
        (unless any-errors
          (if (= 0 warnings)
              (message "OK.")
            (message (propertize "OK (%d warning%s)." 'face 'compilation-warning)
                     warnings
                     (if (= 1 warnings) "" "s")))))
      :done)
     (t :done))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Span functions

(defun stack-mode-points ()
  "Get the current points; either a selected region or an
identifier's points."
  (if (region-active-p)
      (cons (region-beginning) (region-end))
    (let ((ident (haskell-ident-pos-at-point)))
      (cons (car ident)
            (cdr ident)))))

(defun stack-mode-span-from-points (beg end)
  "Get the span representation for the span from BEG to END."
  (save-excursion
    (list :sl (progn (goto-char beg)
                     (line-number-at-pos))
          :sc (1+ (current-column))
          :el (progn (goto-char end)
                     (line-number-at-pos))
          :ec (1+ (current-column)))))

(defun stack-mode-span ()
  "Get the span from the haskell points."
  (let ((points (or (haskell-spanable-pos-at-point)
                    (haskell-ident-pos-at-point)
                    (stack-mode-loose-ident-at-point))))
    (if points
        (stack-mode-span-from-points (car points) (cdr points))
      (error "No identifier at point."))))

(defun stack-mode-goto-span (span)
  "Get buffer points from a span."
  (with-current-buffer (stack-mode-buffer)
    (find-file (stack-lookup 'spanFilePath span))
    (goto-char (point-min))
    (let ((beg (point)))
      (goto-char (point-min))
      (forward-line (1- (stack-lookup 'spanFromLine span)))
      (goto-char (line-beginning-position))
      (forward-char (1- (stack-lookup 'spanFromColumn span))))))

(defun stack-mode-loose-ident-at-point ()
  "Get the loose ident at point."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JSON helpers

(defun stack-mode-list->hashtable (xs)
  "Convert a list to a hashtable."
  (let ((h (make-hash-table)))
    (cl-loop for (key . val)
             in xs
             do (puthash key val h))
    h))

(defun stack-lookup (key object)
  "Get from a JSON object."
  (cdr (assoc key (mapcar #'identity object))))

(defun stack-contents (object)
  "Get from a JSON object."
  (stack-lookup 'contents object))

(defun stack-tag (object)
  "Get the tag of an object."
  (stack-lookup 'tag object))

(defun stack-lookup-contents (key object)
  "Get from a JSON object."
  (stack-contents (stack-lookup key object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck integration

(defun stack-mode-flycheck-start (checker flycheck-callback &optional buffer attempt-count)
  "Run a compile on demand, triggered by Flycheck."
  (when buffer (set-buffer buffer))
  (let ((max-attempts 2))
    (if (not (stack-mode-live-p))
        (if (> (or attempt-count 0) max-attempts)
            (message "Stack backend isn't ready for Flycheck use. Giving up (waited %d seconds)."
                     max-attempts)
          (message "Stack backend isn't ready. Waiting (%d attempts) ..."
                   (or attempt-count 0))
          (progn (stack-mode-try-start)
                 (run-at-time 1 nil 'stack-mode-flycheck-start checker flycheck-callback
                              (current-buffer)
                              (1+ (or attempt-count 0)))))
      (write-region (point-min) (point-max) (buffer-file-name))
      (clear-visited-file-modtime)
      (with-current-buffer (stack-mode-buffer)
        (stack-mode-enqueue
         `((tag . "RequestUpdateSession")
           (contents . []))
         (list :flycheck-callback flycheck-callback
               :stack-buffer (current-buffer))
         'stack-mode-flycheck-callback)))))

(defun stack-mode-flycheck-callback (state reply)
  "Callback for the flycheck loading. Once done, it will report
  errors/warnings to CALLBACK."
  (let ((tag (stack-tag reply)))
    (cond
     ((string= tag "ResponseUpdateSession")
      (let* ((contents (stack-contents reply))
             (tag (stack-tag contents)))
        (cond
         ((string= tag "UpdateStatusProgress")
          (stack-mode-progress-callback nil reply)
          :continue)
         ((string= tag "UpdateStatusDone")
          (stack-mode-enqueue
           `((tag . "RequestGetSourceErrors")
             (contents . []))
           state
           'stack-mode-flycheck-errors-callback)
          :done)
         (t :continue))))
     (t
      :continue))))

(defun stack-mode-flycheck-errors-callback (state reply)
  "Collect error messages and pass them to FLYCHECK-CALLBACK."
  (let ((tag (stack-tag reply)))
    (cond
     ((string= tag "ResponseGetSourceErrors")
      (let ((messages (list)))
        (cl-loop
         for item in (mapcar #'identity (stack-contents reply))
         do (let* ((kind (stack-lookup 'errorKind item))
                   (span (stack-contents (stack-lookup 'errorSpan item)))
                   (msg (stack-lookup 'errorMsg item))
                   (filename (stack-lookup 'spanFilePath span))
                   (sl (stack-lookup 'spanFromLine span))
                   (sc (stack-lookup 'spanFromColumn span))
                   (el (stack-lookup 'spanToLine span))
                   (ec (stack-lookup 'spanToColumn span)))
              (add-to-list
               'messages
               (flycheck-error-new-at
                sl sc
                (cond
                 ((string= kind "KindWarning") 'warning)
                 ((string= kind "KindError") 'error)
                 (t (message "kind: %s" kind)'error))
                msg
                :checker 'stack-ide
                :buffer
                (let ((orig (current-buffer))
                      (buffer
                       (with-current-buffer (plist-get state :stack-buffer)
                         (let ((value (find-file-noselect filename t nil nil)))
                           (if (listp value)
                               (car value)
                             value)))))
                  (set-buffer orig)
                  buffer))
               t)))
        ;; Calling it asynchronously is necessary for flycheck to
        ;; work properly. See
        ;; <https://github.com/flycheck/flycheck/pull/524#issuecomment-64947118>
        (run-with-idle-timer 0 nil (plist-get state :flycheck-callback) 'finished messages)
        (message "Flycheck done."))
      :done)
     (t :done))))

(flycheck-define-generic-checker 'stack-ide
  "A syntax and type checker for Haskell using Stack's IDE support."
  :start 'stack-mode-flycheck-start
  :modes '(haskell-mode))

(provide 'stack-mode)