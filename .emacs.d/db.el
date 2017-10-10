(when (load "flymake" t)
 (defun flymake-pyflakes-init ()
   (let* ((temp-file (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
          (local-file (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
     (list "/Users/tomm/path/pyxl_pyflakes" (list local-file))))

 (add-to-list 'flymake-allowed-file-name-masks
              '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'python-mode-hook 'flymake-find-file-hook)
