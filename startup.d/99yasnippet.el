(require 'yasnippet)
(yas/initialize)
(yas/load-directory (expand-file-name "snippets" starter-kit-dir))

;; added from
;; http://blog.deadpansincerity.com/2011/05/setting-up-emacs-as-a-javascript-editing-environment-for-fun-and-profit/

(add-to-list 'ac-sources 'ac-source-yasnippet)
;; make yasnippets work with org-mode
(defun yas/org-very-safe-expand ()
      (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

    (defun yas/org-setup ()
      ;; yasnippet (using the new org-cycle hooks)
      (make-variable-buffer-local 'yas/trigger-key)
      (setq yas/trigger-key [tab])
      (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
      (define-key yas/keymap [tab] 'yas/next-field))

    (add-hook 'org-mode-hook #'yas/org-setup)

;;(setq yas/root-directory '("~/.emacs.d/vendor/yasnippet/snippets" "~/.emacs.d/snippets"))
;; (yas/global-mode 1)
;; (setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/x-prompt))
;; (setq yas/wrap-around-region 'cua)

;; (defun my/autoinsert-yas-expand()
;;   "Replace text in yasnippet template."
;;   (yas/expand-snippet (buffer-string) (point-min) (point-max)))


;; (custom-set-variables
;;      '(auto-insert 'other)
;;      '(auto-insert-directory "~/.emacs.d/autoinsert/")
;;      '(auto-insert-alist '((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header") . ["template.h" c++-mode my/autoinsert-yas-expand])
;;                            (("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ source") . ["template.cc" my/autoinsert-yas-expand])
;;                            (("\\.sh\\'" . "Shell script") . ["template.sh" my/autoinsert-yas-expand])
;;                            (("\\.el\\'" . "Emacs Lisp") . ["template.el" my/autoinsert-yas-expand])
;;                            (("\\.pl\\'" . "Perl script") . ["template.pl" my/autoinsert-yas-expand])
;;                            (("\\.pm\\'" . "Perl module") . ["template.pm" my/autoinsert-yas-expand])
;;                            (("\\.py\\'" . "Python script") . ["template.py" my/autoinsert-yas-expand])
;;                            (("[mM]akefile\\'" . "Makefile") . ["Makefile" my/autoinsert-yas-expand])
;;                            (("\\.tex\\'" . "TeX/LaTeX") . ["template.tex" my/autoinsert-yas-expand]))))
