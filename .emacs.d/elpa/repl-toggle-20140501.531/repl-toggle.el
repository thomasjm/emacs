;;; repl-toggle.el --- Switch to/from repl buffer for current major-mode

;; Copyright (C) 2013 Tom Regner

;; Author: Tom Regner <tom@goochesa.de>
;; Maintainer: Tom Regner <tom@goochesa.de>
;; Version: 20140501.531
;; X-Original-Version: 0.2.0
;; Keywords: repl, buffers, toggle
;; Package-Requires: ((fullframe  "0.0.5"))

;;  This file is NOT part of GNU Emacs

;;  This program is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This program is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is a generalization of an idea by Mickey Petersen of
;; masteringemacs fame: Use one keystroke to jump from a code buffer
;; to the corresponding repl buffer and back again.  This works even if
;; you do other stuff in between, as the last buffer used to jump to a
;; repl is stored in a buffer local variable in the repl buffer.
;;
;; Currently this assumes that the stored command to start the repl
;; will switch to an already open repl buffer if it exists.
;;
;; There are no repl/mode combinations preconfigured, put something
;; like the following in your Emacs setup for php and elisp repl:
;;
;;     (require 'repl-toggle)
;;     (setq rtog/mode-repl-alist '((php-mode . php-boris) (emacs-lisp-mode . ielm)))
;; 
;; This defines a global minor mode, indicated at with 'rt' in the modeline, that
;; grabs "C-c C-z" as repl toggling keybinding.
;;
;; I don't know with wich repl modes this actualy works.  If you use
;; this mode, please tell me your rtog/mode-repl-alist, so that I can
;; update the documentation.
;;
;; Known to work:
;;
;; - ~(php-mode . php-boris)~
;; - ~(emacs-lisp-mode . ielm)~
;; - ~(elixir-mode . elixir-mode-iex)~
;; - ~(ruby-mode . inf-ruby)~
;;
;; If you supply the universal prefix argument you can
;;
;; - C-u pass the current line
;; - C-u C-u pass the current defun
;; - C-u C-u C-u pass the the whole current buffer
;;
;; to the repl buffer you switch to.
;;
;; If you set rtog/fullscreen to true, prior to loading this module,
;; the repl-commands will be executed fullscreen, i.e. as single
;; frame, restoring the window-layout on stwitching back to the
;; buffer.
;;
;; Emacs -- of course -- has more than one function to switch
;; between buffers. You can customize ~rtog/goto-buffer-fun~ to
;; accommodate your needs. The default is ~switch-to-buffer~; to
;; move focus to another frame that already shows the other buffer,
;; instead of switching the current frame to it, use
;; ~pop-to-buffer~.
;; 
;; ~(setq rtog/goto-buffer-fun 'pop-to-buffer)~
;;
;; If the mode you want to use doesn't jump to an existing
;; repl-buffer, but always starts a new one, you can use
;; `rtog/switch-to-shell-buffer' in your configuration to get that
;; behaviour, e.g. for `octave-mode':
;;
;; (rtog/add-repl 'octave-mode (rtog/switch-to-shell-buffer 'inferior-octave-buffer 'inferior-octave))
;;
;;; Code:

(require 'fullframe)

;; customization

(defcustom rtog/fullscreen nil
  "Show REPL-buffers as single frame.
This setting must be true before this mode is loaded!"
  :type '(boolean)
  :group 'repl-toggle)

(defcustom rtog/mode-repl-alist ()
  "List of cons `(major-mode . repl-command)`.
It associates major modes with a repl command."
  :type '(alist :key-type symbol :value-type function)
  :group 'repl-toggle)

(defcustom rtog/goto-buffer-fun 'switch-to-buffer
  "Function to call to switch from repl to buffer."
  :type 'function
  :group 'repl-toggle)

;; variables
(defvar rtog/--last-buffer nil
  "Store the jump source in repl buffer.")
(make-variable-buffer-local 'rtog/--last-buffer)

(defvar rtog/--framed nil
  "Only advise with fullframe once.")

;; minor mode
(defvar repl-toggle-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-z") 'rtog/toggle-repl)
    m)
  "Keymap for `repl-toggle-mode'.")

;;;###autoload
(define-minor-mode repl-toggle-mode
  "A minor mode to allow uniform repl buffer switching."
  nil
  :lighter " rt"
  :keymap repl-toggle-mode-map
  :global t)

;; internal functions

(defun rtog/pass-code (passAlong?)
  "Return context depending on PASSALONG?.
Return the current line or region, function or definition or the
whole current buffer.

Passing of the buffer respects narrowing."
  (case passAlong?
    (4 (if (use-region-p)
           (buffer-substring-no-properties
            (region-beginning)
            (region-end))
         (thing-at-point 'line)))
    (16 (thing-at-point 'defun))
    (64 (buffer-substring-no-properties (point-min) (point-max)))))

(defun rtog/--switch-to-buffer ()
  "If `rtog/--last-buffer` is non nil, switch to this buffer."
  (if (and rtog/--last-buffer
           (buffer-live-p rtog/--last-buffer))
      (funcall rtog/goto-buffer-fun rtog/--last-buffer)
    (setq rtog/--last-buffer nil)))


(defun rtog/--switch-to-repl (&optional code &rest ignored)
  "Switch to a repl if defined for the current mode.

If `rtog/mode-repl-map` contains an entry for the `major-mode`
of the current buffer, call the value as function.

This assumes that the command executed will start a new repl, or
switch to an already running process.
 
Any text passed as CODE will be pasted in the repl buffer.

Additional paramters passed will be IGNORED."
  (let ((--buffer (current-buffer))
        (--mode-cmd  (cdr (assoc major-mode rtog/mode-repl-alist ))))
    (if (and --mode-cmd (functionp --mode-cmd))
        (progn 
          (funcall --mode-cmd)
          (setq rtog/--last-buffer --buffer)
          (if code
              (progn 
                (goto-char (point-max))
                (insert code)))
          )
      (message "--mode-cmd silly? %s" --mode-cmd))))

(defmacro rtog/with-gensym (names &rest body)
  "Make macros relying on multiple `cl-gensym' calls more readable.
Takes a list of symbols NAMES and defines `cl-gensym' variables in a `let'
  that has BODY as body.

Example:

\(rtog/with-gensym (one two three)
  (progn
    `(let ((,one \"one\")
          (,two \"two\")
          (,three \"three\"))
    (message \"%s:%s:%s\\n\" ,one ,two ,three))\)

Instead of

\(let ((one (cl-gensym \"sym-one\"))
       (two (cl-gensym \"sym-two\"))
       (three (cl-gensym \"sym-three\")))
  `(let ((,one \"one\")
        (,two \"two\")
        (,three \"three\"))
    (message \"%s:%s:%s\\n\" ,one ,two ,three)))

Idea attributed to Peter Seibel where I found it, but since I
found it in Paul Grahams On lisp, I guess it's either attributable
to him or common lispers knowledge."
  (declare (indent defun))
  `(let
       ,(cl-loop for n in names collect
                 `(,n (cl-gensym (concat "rtog/--"
                                         (symbol-name (quote ,n))))))
     ,@body))
;; API

;;;###autoload
(defmacro rtog/switch-to-shell-buffer (buffer-name shell-command &optional shell-args)
      "Make sure that `BUFFER-NAME' exists and is displayed.

Executes `SHELL-COMMAND', passing `SHELL-ARGS', if buffer
`BUFFER-NAME' doesn't exist."
     
      (rtog/with-gensym (fun bname shcomm args)
        `(let ((,bname ,buffer-name)
               (,shcomm ,shell-command)
               (,args ,shell-args))
           `(lambda ()
              (if (get-buffer ,,bname)
                  (funcall rtog/goto-buffer-fun (get-buffer ,,bname))
                (apply ',,shcomm ,,args))))))


;; interactive functions

;;;###autoload
(defun rtog/add-repl (mode repl-cmd)
  "Associate MODE with REPL-CMD at runtime..

If in a buffer with `major-mode' MODE, execute REPL-CMD when
`rtog/toggle-repl' is called."
  (interactive "Mmajor mode? \narepl function? ")
  (add-to-list 'rtog/mode-repl-alist (cons mode repl-cmd)))

;;;###autoload
(defun rtog/toggle-repl (&optional passAlong? &rest ignored)
  "Switch to the repl asscociated with the current major mode.

If in a repl already switch back to the buffer we
came from.

If you provide PASSALONG? as a universal prefix with
\\[universal-argument], the current line or region is passed to
the repl buffer, using \\[universal-argument]
\\[universal-argument] the current function or definition is
passed, and finaly using
\\[universal-argument]\\[universal-argument]\\[universal-argument]
you can pass the whole current buffer.

Additional paramters passed will be IGNORED."
  (interactive "p")
  (progn
    (when (and rtog/fullscreen (not rtog/--framed))
      (progn ; set fullscreen advice if wanted
        (setq rtog/--framed t)
        (fullframe rtog/--switch-to-repl rtog/--switch-to-buffer nil)))
    (if rtog/--last-buffer
      (rtog/--switch-to-buffer)
    (rtog/--switch-to-repl (rtog/pass-code passAlong?)))))

;; hook into comint modes no matter what
(defun rtog/activate ()
  "Activate the repl-toggle minor mode."
  (repl-toggle-mode 1))

(add-hook 'comint-mode-hook 'rtog/activate)
(provide 'repl-toggle)

;;; repl-toggle.el ends here