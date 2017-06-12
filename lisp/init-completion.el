(use-package company
  :load-path "site-lisp/company-mode"
  :diminish company-mode
  :commands company-mode
  :config
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it)))

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

(provide 'init-completion)
