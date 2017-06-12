(use-package ess-site
  :ensure ess)

(use-package polymode
  :ensure t
  :mode
  ("\\.Snw" . poly-noweb+r-mode)
  ("\\.Rnw" . poly-noweb+r-mode)
  ("\\.Rmd" . poly-markdown+r-mode))

(provide 'init-r)
