(use-package ivy
  :ensure t
  :config
  (ivy-mode 1))

(defvar my/bib-file-location "~/personal/bibliography/references.bib"
  "Where I keep my bib file.")

(use-package ivy-bibtex
  :ensure t
  :config
  (setq bibtex-completion-bibliography my/bib-file-location)
  (bind-key* "C-c C-r" #'ivy-bibtex)
  ;; The default action is to view pdf, here we change that to insert citation
  ;; code from https://github.com/tmalsburg/helm-bibtex
  (defun ivy-bibtex (&optional arg)
    "Search BibTeX entries using ivy.

With a prefix ARG the cache is invalidated and the bibliography
reread."
    (interactive "P")
    (when arg
      (setq bibtex-completion-bibliography-hash ""))
    (bibtex-completion-init)
    (ivy-read "BibTeX Items: "
              (bibtex-completion-candidates 'ivy-bibtex-candidates-formatter)
              :caller 'ivy-bibtex
              :action 'bibtex-completion-insert-citation)))

(provide 'init-ivy)
