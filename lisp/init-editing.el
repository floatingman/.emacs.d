(require 'saveplace)

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package unfill
  :ensure t)

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))

(when (eval-when-compile (version< "24.4" emacs-version))
  (electric-indent-mode 1))

(global-auto-revert-mode)

(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(transient-mark-mode t)

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode)


;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)


;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(define-key global-map (kbd "RET") 'newline-and-indent)

(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "s-<return>") 'sanityinc/newline-at-end-of-line)

(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package indent-guide
  :ensure t
  :diminish indent-guide-mode
  :init
  (add-hook 'prog-mode-hook 'indent-guide-mode))

(provide 'init-editing)
