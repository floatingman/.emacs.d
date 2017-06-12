;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   (progn
;;     (ac-config-default)
;;     (global-auto-complete-mode t)
;;     ))

(use-package company
  :ensure t
  :diminish "CM"
  :bind ("C-." . company-complete)
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  (use-package company-quickhelp
    :ensure t
    :init
    (add-hook 'company-mode-hook #'company-quickhelp-mode)
    :config
    (setq company-quickhelp-delay 1.0))
  :config
  (setq company-idle-delay 0.2
        ;; min prefix of 3 chars
        company-minimum-prefix-length 3
        ;; wrap completions around
        company-selection-wrap-around t
        ;; don't show numbers in completion
        company-show-numbers nil
        ;; don't downcase dabbrev suggestions
        company-dabbrev-downcase nil
        ;; sort completions by occurrence
        company-transformers '(company-sort-by-occurrence))
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-d" . company-show-doc-buffer)
             ("C-l" . company-show-location)
             ("<tab>" . company-complete)))

(require 'skeleton)

(define-skeleton dn-org-html-publish-header
  "Insert a standard header for org publish"
  "Title: "
  "#+TITLE: " str \n
  "#+AUTHOR: " (user-full-name) \n
  "#+EMAIL: " user-mail-address \n
  
  "#+OPTIONS: ':t *:t -:t ::t <:t H:3 \n:nil ^:t arch:headline author:t c:nil
   #+OPTIONS: creator:comment d:(not LOGBOOK) date:t e:t email:nil f:t inline:t
   #+OPTIONS: num:t p:nil pri:nil stat:t tags:t tasks:t tex:t timestamp:t toc:t
   #+OPTIONS: todo:t |:t
   #+CREATOR: Emacs 24.3.50.3 (Org mode 8.0.3)
   #+DESCRIPTION:
   #+EXCLUDE_TAGS: noexport
   #+KEYWORDS:
   #+LANGUAGE: en
   #+SELECT_TAGS: export"
  )

(define-skeleton mlh-org-header
  "Insert a standard header for org-mode files"
  "Title: "
  "#+TITLE: " str \n
  "#+AUTHOR: " (user-full-name) \n
  "#+EMAIL: " user-mail-address \n
  "#+LANGUAGE: en
#+PROPERTY: header-args :results code replace :exports both :noweb yes :tangle no
#+HTML_HEAD: <link rel=\"stylesheet\" href=\"http://dakrone.github.io/org.css\" type=\"text/css\" />
#+EXPORT_EXCLUDE_TAGS: noexport
#+OPTIONS: H:4 num:nil toc:t \\n:nil @:t ::t |:t ^:{} -:t f:t *:t
#+OPTIONS: skip:nil d:(HIDE) tags:not-in-toc
#+TODO: TODO(t) | DONE(d)
#+TODO: TODO(t) SOMEDAY(s) INPROGRESS(i) HOLD(h) WAITING(w@/!) NEEDSREVIEW(n@/!) | DONE(d)
#+TODO: TODO(t) INPROGRESS(i) | CANCELLED(c@/!)
#+STARTUP: fold nodlcheck lognotestate content

* ")

(define-skeleton mlh-org-wrap-elisp
  "Wrap text with #+BEGIN_SRC / #+END_SRC for the emacs-lisp code"
  nil
  > "#+BEGIN_SRC emacs-lisp" \n
  > _ \n
  > "#+END_SRC" \n)

(define-skeleton mlh-org-wrap-source
  "Wrap text with #+BEGIN_SRC / #+END_SRC for a code type"
  "Language: "
  > "#+BEGIN_SRC " str \n
  > _ \n
  > "#+END_SRC" \n)

(define-skeleton mlh-es-make-index
  "Insert boilerplate to create an index with `es-mode' syntax"
  "Index name: "
  "POST /" str \n
  "{" \n
  > "\"settings\": {" \n
  > "\"index\": {" \n
  > "\"number_of_shards\": 1," \n
  > "\"number_of_replicas\": 0" \n
  > "}" \n ;; index
  > "}," \n ;; settings
  > "\"mappings\": {" \n
  > "\"" (skeleton-read "Type name: ") "\": {" \n
  > "\"properties\": {" \n
  > "\"body\": {" \n
  > "\"type\": \"string\"" \n
  > "}" \n ;; body
  > "}" \n ;; properties
  > "}" \n ;; type
  > "}" \n ;; mappings
  > "}" \n)

(define-skeleton mlh-java-try-catch
  "Wrap code in a Java try/catch"
  nil
  > "try {" \n
  > _
  > "} catch (Exception e) {" \n
  > "throw e;" \n
  > "}" \n)

;;ggtags
(defun my/setup-helm-gtags ()
  (interactive)
  ;; these variables must be set before load helm-gtags
  ;; you can change to any prefix key of your choice
  (setq helm-gtags-prefix-key "C-c g")
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-suggested-key-mapping t)
  (use-package helm-gtags
    :ensure t
    :init (helm-gtags-mode t)
    :diminish "")
  ;; key bindings
  (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))

(defun my/setup-ggtags ()
  (interactive)
  (ggtags-mode 1)
  ;; turn on eldoc with ggtags
  (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
  ;; add ggtags to the hippie completion
  (setq-local hippie-expand-try-functions-list
              (cons 'ggtags-try-complete-tag
                    hippie-expand-try-functions-list))
  ;; use helm for completion
  (setq ggtags-completing-read-function nil))

(use-package ggtags
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                  (my/setup-semantic-mode)
                  (my/setup-helm-gtags))))))


(provide 'init-completion)
