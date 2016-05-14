(defun my/setup-semantic-mode ()
  (interactive)
  (use-package semantic
    :init
    (require 'semantic/ia)
    (require 'semantic/wisent)
    (semantic-mode t)))

(add-hook 'c-mode-hook #'my/setup-semantic-mode)
(add-hook 'java-mode-hook #'my/setup-semantic-mode)

(global-set-key (kbd "RET") 'newline-and-indent)

(use-package paredit
  :commands paredit-mode
  :diminish "()"
  :config
  (bind-key "M-)" #'paredit-forward-slurp-sexp paredit-mode-map)
  (bind-key "C-(" #'paredit-forward-barf-sexp paredit-mode-map)
  (bind-key "C-)" #'paredit-forward-slurp-sexp paredit-mode-map)
  (bind-key ")" #'paredit-close-parenthesis paredit-mode-map)
  (bind-key "M-\"" #'my/other-window-backwards paredit-mode-map))

(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
(bind-key "C-c f" 'find-function)

;;Projects
(use-package projectile
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (setq projectile-indexing-method 'alien)
    (add-to-list 'projectile-globally-ignored-files "node-modules"))
  :config
  (projectile-global-mode))

(defun my/recursive-find-file (file &optional directory)
  "Find the first FILE in DIRECTORY or its parents."
  (setq directory (or directory (file-name-directory (buffer-file-name)) (pwd)))
  (if (file-exists-p (expand-file-name file directory))
      (expand-file-name file directory)
    (unless (string= directory "/")
      (my/recursive-find-file file (expand-file-name ".." directory)))))

(defun my/find-tags ()
  "Set the TAGS file."
  (set (make-variable-buffer-local 'tags-table-list) nil)
  (set (make-variable-buffer-local 'tags-file-name)
       (my/recursive-find-file "TAGS")))

(eval-after-load 'drupal-mode
  '(progn
     (add-hook 'drupal-mode-hook 'my/find-tags)))

(electric-pair-mode 1)
(setq electric-pair-preserve-balance t
      electric-pair-delete-adjacent-pairs t
      electric-pair-open-newline-between-pairs nil)

(show-paren-mode 1)

(electric-indent-mode 1)

;; Ignore electric indentation for python and yaml
(defun electric-indent-ignore-mode (char)
  "Ignore electric indentation for python-mode"
  (if (or (equal major-mode 'python-mode)
          (equal major-mode 'yaml-mode))
      'no-indent
    nil))
(add-hook 'electric-indent-functions 'electric-indent-ignore-mode)

(electric-layout-mode 1)

(use-package smartparens
  :defer 5
  :diminish smartparens-mode
  :bind (("M-9" . sp-backward-sexp)
         ("M-0" . sp-forward-sexp))
  :init
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook #'turn-on-smartparens-mode)
	:config
  (add-to-list 'sp-sexp-suffix '(json-mode regex ""))
  (add-to-list 'sp-sexp-suffix '(es-mode regex ""))
  (add-hook 'js-mode-hook #'smartparens-mode)
  (add-hook 'web-mode-hook #'smartparens-mode)

  (use-package smartparens-config)

  ;; Remove the M-<backspace> binding that smartparens adds
  (let ((disabled '("M-<backspace>")))
    (setq sp-smartparens-bindings
          (cl-remove-if (lambda (key-command)
                          (member (car key-command) disabled))
                        sp-smartparens-bindings)))

  (define-key sp-keymap (kbd "C-(") 'sp-forward-barf-sexp)
  (define-key sp-keymap (kbd "C-)") 'sp-forward-slurp-sexp)
  (define-key sp-keymap (kbd "M-(") 'sp-forward-barf-sexp)
  (define-key sp-keymap (kbd "M-)") 'sp-forward-slurp-sexp)
  (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
  (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
  (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
  (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
  (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)
  (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
  (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
  (define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
  (define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)
  ;; (define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
  ;; (define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)
  (define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
  (define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)
  (define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
  (define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
  (define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
  (define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)
  (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
  (define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)
  (define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
  (define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
  (define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
  (define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
  (define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
  (define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
  (define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
  (define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
  (define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  ;; Remove '' pairing in elisp because quoting is used a ton
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)

  (sp-with-modes '(html-mode sgml-mode)
                 (sp-local-pair "<" ">"))

  (sp-with-modes sp--lisp-modes
                 (sp-local-pair "(" nil :bind "C-("))

  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context)))


;; flycheck for all your fly inspection needs
(use-package flycheck
  :defer 5
  :bind (("M-g M-n" . flycheck-next-error)
         ("M-g M-p" . flycheck-previous-error)
         ("M-g M-=" . flycheck-list-errors))
  :init (global-flycheck-mode)
  :diminish flycheck-mode 
  :config
	(progn
		(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc json-jsonlint json-python-json))
    (use-package flycheck-pos-tip
      :init (flycheck-pos-tip-mode))
    (use-package helm-flycheck
      :init (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))
    (use-package flycheck-haskell
      :init (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))))

(setq vc-handled-backends '(SVN Git))

(defun my/add-watchwords ()
  "Highlight FIXME, TODO, and NOCOMMIT in code"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|NOCOMMIT\\)\\>"
          1 '((:foreground "#d7a3ad") (:weight bold)) t))))

(add-hook 'prog-mode-hook #'hl-line-mode)


(add-hook 'prog-mode-hook #'my/add-watchwords)

(use-package subword
  :diminish subword-mode)

(use-package log4j-mode
  :init (add-hook #'log4j-mode-hook #'my/turn-on-viewing-mode))

(use-package bookmark+
  :defer 10
  :config
  (progn
    (setq bookmark-version-control t
          ;; auto-save bookmarks
          bookmark-save-flag 1)))

(provide 'init-coding-helpers)
