;; Instructions on auctex-latexmk:
;; To use this package, add the following line to your .emacs file:
;;     (require 'auctex-latexmk)
;;     (auctex-latexmk-setup)
;; And add the following line to your .latexmkrc file:
;;     # .latexmkrc starts
;;     $pdf_mode = 1;
;;     # .latexmkrc ends
;; After that, by using M-x TeX-command-master (or C-c C-c), you can use
;; LatexMk command to compile TeX source.


;; AucTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
  (push
    '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
      :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
     '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(add-hook 'LaTeX-mode-hook
          (lambda ()
            (push
             '("latexmk" "latexmk -synctex=1 -pdf %s" TeX-run-TeX nil t
               :help "Run latexmk on file")
             TeX-command-list)))

;; Shouldn't be necessary:
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)
