(use-package paredit
  :commands paredit-mode
  :diminish paredit-mode
  :config
  (use-package paredit-ext)

  (bind-key "C-M-l" #'paredit-recentre-on-sexp paredit-mode-map)

  (bind-key ")" #'paredit-close-round-and-newline paredit-mode-map)
  (bind-key "M-)" #'paredit-close-round paredit-mode-map)

  (bind-key "M-k" #'paredit-raise-sexp paredit-mode-map)
  (bind-key "M-I" #'paredit-splice-sexp paredit-mode-map)

  (unbind-key "M-r" paredit-mode-map)
  (unbind-key "M-s" paredit-mode-map)

  (bind-key "C-. D" #'paredit-forward-down paredit-mode-map)
  (bind-key "C-. B" #'paredit-splice-sexp-killing-backward paredit-mode-map)
  (bind-key "C-. C" #'paredit-convolute-sexp paredit-mode-map)
  (bind-key "C-. F" #'paredit-splice-sexp-killing-forward paredit-mode-map)
  (bind-key "C-. a" #'paredit-add-to-next-list paredit-mode-map)
  (bind-key "C-. A" #'paredit-add-to-previous-list paredit-mode-map)
  (bind-key "C-. j" #'paredit-join-with-next-list paredit-mode-map)
  (bind-key "C-. J" #'paredit-join-with-previous-list paredit-mode-map))

(or (use-package mic-paren
      :defer 5
      :config
      (paren-activate))
    (use-package paren
      :defer 5
      :config
      (show-paren-mode 1)))

;;Projects
(use-package projectile
  :load-path "site-lisp/projectile"
  :diminish projectile-mode
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on))
  (projectile-global-mode)
  (bind-key "s s"
            #'(lambda ()
                (interactive)
                (helm-do-grep-1 (list (projectile-project-root)) t))
            'projectile-command-map))


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

(use-package rainbow-delimiters
  :ensure t
  :init (rainbow-delimiters-mode))

;; flycheck for all your fly inspection needs
(use-package flycheck
  :load-path "site-lisp/flycheck"
  :defer 5
  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point))


(defun my/add-watchwords ()
  "Highlight FIXME, TODO, and NOCOMMIT in code"
  (font-lock-add-keywords
   nil '(("\\<\\(FIXME\\|TODO\\|NOCOMMIT\\)\\>"
          1 '((:foreground "#d7a3ad") (:weight bold)) t))))

(add-hook 'prog-mode-hook #'my/add-watchwords)

(use-package subword
  :ensure t
  :diminish subword-mode)


(use-package bookmark+
  :ensure t
  :defer 10
  :config
  (progn
    (setq bookmark-version-control t
          ;; auto-save bookmarks
          bookmark-save-flag 1)))

(use-package google-c-style
  :ensure t
  )

(use-package yaml-mode
  :ensure t
  )

(use-package editorconfig
  :ensure t
  :diminish "ec"
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1))
  )

;; setup some good log viewing defaults
;; automagically tail log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

(defun etc-log-tail-handler ()
  (end-of-buffer)
  (make-variable-buffer-local 'auto-revert-interval)
  (setq auto-revert-interval 1)
  (auto-revert-set-timer)
  (make-variable-buffer-local 'auto-revert-verbose)
  (setq auto-revert-verbose nil)
  (read-only-mode t)
  (font-lock-mode 0)
  (when (fboundp 'show-smartparens-mode)
    (show-smartparens-mode 0)))

(add-hook 'auto-revert-tail-mode-hook 'etc-log-tail-handler)

(bind-key "C-. m" #'kmacro-keymap)

(bind-key "C-. C-i" #'indent-rigidly)

(defvar insert-and-counting--index 1)
(defvar insert-and-counting--expr nil)

(defun insert-and-counting (&optional index expr)
  (interactive
   (if (or current-prefix-arg
           (not insert-and-counting--expr))
       (list (setq insert-and-counting--index
                   (prefix-numeric-value current-prefix-arg))
             (setq insert-and-counting--expr
                   (eval-expr-read-lisp-object-minibuffer "Pattern: ")))
     (list (setq insert-and-counting--index
                 (1+ insert-and-counting--index))
           insert-and-counting--expr)))
  (let ((n insert-and-counting--index))
    (eval expr)))

(bind-key "C-. C-y" #'insert-and-counting)

(use-package align
  :bind (("M-["   . align-code)
         ("C-c [" . align-regexp))
  :commands align
  :preface
  (defun align-code (beg end &optional arg)
    (interactive "rP")
    (if (null arg)
        (align beg end)
      (let ((end-mark (copy-marker end)))
        (indent-region beg end-mark nil)
        (align beg end-mark)))))

(use-package compile
  :bind (("C-c c" . compile)
         ("M-O" . show-compilation))
  :preface
  (defun show-compilation ()
    (interactive)
    (let ((compile-buf
           (catch 'found
             (dolist (buf (buffer-list))
               (if (string-match "\\*compilation\\*" (buffer-name buf))
                   (throw 'found buf))))))
      (if compile-buf
          (switch-to-buffer-other-window compile-buf)
        (call-interactively 'compile))))
  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))
  :config
  (add-hook 'compilation-filter-hook #'compilation-ansi-color-process-output))


(provide 'init-coding-helpers)
