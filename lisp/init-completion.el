(use-package company
  :diminish company-mode
  :bind ("C-." . company-complete)
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  (use-package company-quickhelp
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

;; I also need some code so yasnippet and company don’t step on each other’s toes when it comes to the TAB key:

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  "If in the minibuffer, complete there. If a tab needs to be
inserted, do that, otherwise check if either a yasnippet should
be expanded or try company completion if not."
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas-minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;; ಠ_ಠ yasnippet
(add-hook 'yas-minor-mode-hook
          (lambda ()
            (local-set-key [tab] 'tab-indent-or-complete)))



(use-package yasnippet
  :disabled t
  :defer t
  :init
  (setq yas-snippet-dirs "~/.emacs.d/site-lisp/yasnippet-snippets")
  (add-hook 'org-mode-hook #'yas-minor-mode)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :diminish yas-minor-mode
  :commands (yas-expand yas-minor-mode)
  :functions (yas-guess-snippet-directories yas-table-name)
  :defines (yas-guessed-modes)
  :mode ("/\\.emacs\\.d/snippets/" . snippet-mode))

;;use abbrev to correct misspellings
(use-package abbrev
  :disabled t
  :diminish abbrev-mode
  :init (add-hook 'prog-mode-hook #'abbrev-mode)
  :defer 30
  :config
  (progn
    (define-key ctl-x-map "\M-a" 'my/ispell-word-then-abbrev)

    (defun my/ispell-word-then-abbrev (p)
      "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
      (interactive "P")
      (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
        (call-interactively 'ispell-word)
        (setq aft (downcase (or (thing-at-point 'word) "")))
        (unless (string= aft bef)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob"))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft))))

    (setq save-abbrevs t)
    (setq-default abbrev-mode t)))

;; excludes very large buffers from dabbrev
(defun sanityinc/dabbrev-friend-buffer (other-buffer)
  (< (buffer-size other-buffer) (* 1 1024 1024)))
(setq dabbrev-friend-buffer-function 'sanityinc/dabbrev-friend-buffer)

;; use smart tab for completion everywhere except erc, mu4e, and shell
(use-package smart-tab
  :defer t
  :diminish ""
  :init (global-smart-tab-mode 1)
  :config
  (progn
    (add-to-list 'smart-tab-disabled-major-modes 'mu4e-compose-mode)
    (add-to-list 'smart-tab-disabled-major-modes 'erc-mode)
    (add-to-list 'smart-tab-disabled-major-modes 'shell-mode)))

(use-package popwin
  :commands popwin-mode
  :init (popwin-mode 1)
  :config
  (progn
    (defvar popwin:special-display-config-backup popwin:special-display-config)
    (setq display-buffer-function 'popwin:display-buffer)

    ;; basic
    (push '("*Help*" :stick t :noselect t) popwin:special-display-config)
    (push '("*Pp Eval Output*" :stick t) popwin:special-display-config)

    ;; compilation
    (push '(compilation-mode :stick t :width 0.4) popwin:special-display-config)

    ;; magit
    (push '("*magit-process*" :stick t) popwin:special-display-config)

    ;; quickrun
    (push '("*quickrun*" :stick t) popwin:special-display-config)

    ;; dictionaly
    (push '("*dict*" :stick t) popwin:special-display-config)
    (push '("*sdic*" :stick t) popwin:special-display-config)

    ;; popwin for slime
    (push '(slime-repl-mode :stick t) popwin:special-display-config)

    ;; man
    (push '(Man-mode :stick t :height 20) popwin:special-display-config)

    ;; Elisp
    (push '("*ielm*" :stick t) popwin:special-display-config)
    (push '("*eshell pop*" :stick t) popwin:special-display-config)

    ;; pry
    (push '(inf-ruby-mode :stick t :height 20) popwin:special-display-config)

    ;; python
    (push '("*Python*"   :stick t) popwin:special-display-config)
    (push '("*Python Help*" :stick t :height 20) popwin:special-display-config)
    (push '("*jedi:doc*" :stick t :noselect t) popwin:special-display-config)

    ;; Haskell
    (push '("*haskell*" :stick t) popwin:special-display-config)
    (push '("*GHC Info*") popwin:special-display-config)

    ;; sgit
    (push '("*sgit*" :position right :width 0.5 :stick t)
          popwin:special-display-config)

    ;; git-gutter
    (push '("*git-gutter:diff*" :width 0.5 :stick t)
          popwin:special-display-config)

    ;; es-results-mode
    (push '(es-result-mode :stick t :width 0.5)
          popwin:special-display-config)

    ;; direx
    (push '(direx:direx-mode :position left :width 40 :dedicated t)
          popwin:special-display-config)

    (push '("*Occur*" :stick t) popwin:special-display-config)

    ;; prodigy
    (push '("*prodigy*" :stick t) popwin:special-display-config)

    ;; malabar-mode
    (push '("*Malabar Compilation*" :stick t :height 30)
          popwin:special-display-config)

    ;; org-mode
    (push '("*Org tags*" :stick t :height 30)
          popwin:special-display-config)

    ;; Completions
    (push '("*Completions*" :stick t :noselect t) popwin:special-display-config)

    ;; ggtags
    (push '("*ggtags-global*" :stick t :noselect t :height 30) popwin:special-display-config)

    ;; async shell commands
    (push '("*Async Shell Command*" :stick t) popwin:special-display-config)

    (defun my/popup-downloads ()
      "Pop up the downloads buffer (4th eshell buffer for me"
      (interactive)
      (when (not (get-buffer "*eshell downloads*"))
        (let ((eshell-buffer-name "*eshell downloads*"))
          (eshell)))
      (popwin:popup-buffer "*eshell downloads*"))

    ;; eshell 4 is always my "download stuff" buffer
    (global-set-key (kbd "C-x M-d") #'my/popup-downloads)
    (global-set-key (kbd "C-h e") 'popwin:messages)))

(require 'skeleton)

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
