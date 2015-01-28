(setq default-tab-width 4)
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(add-hook 'html-mode-hook
	  (lambda()
	    (setq sgml-basic-offset 4)
	    (setq indent-tabs-mode nil)))
(add-hook 'less-mode-hook
	  (lambda()
	    (setq sgml-basic-offset 4)
	    (setq indent-tabs-mode nil)))
(add-hook 'js2-mode-hook
	  (lambda()
	    (setq sgml-basic-offset 4)
	    (setq indent-tabs-mode nil)))
