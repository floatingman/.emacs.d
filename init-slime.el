(require-package 'slime)

;; There are 2 versions of Slime available as packages. The 2010* version
;; is for Clojure compatibility, and uses separate packages for slime-fuzzy
;; and slime-repl. The other version is the latest available, which
;; contains a complete "contrib" dir.
(mapc #'delete-file
      (file-expand-wildcards (concat user-emacs-directory "elpa/slime-2*/contrib/*.elc")))

(require-package 'ac-slime)
(require-package 'hippie-expand-slime)

;;; Lisp buffers

(defun sanityinc/slime-setup ()
  "Mode setup function for slime lisp buffers."
  (set-up-slime-hippie-expand)
  (set-up-slime-ac t))

(after-load 'slime
  (setq slime-protocol-version 'ignore)
  (setq slime-net-coding-system 'utf-8-unix)
  (slime-setup '(slime-repl slime-fuzzy))
  (setq slime-complete-symbol*-fancy t)
  (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)
  (add-hook 'slime-mode-hook 'sanity/slime-setup))


;;; REPL

(defun sanityinc/slime-repl-setup ()
  "Mode setup function for slime REPL."
  (sanityinc/lisp-setup)
  (set-up-slime-hippie-expand)
  (set-up-slime-ac t)
  (setq show-trailing-whitespace nil))

(after-load 'slime-repl
  ;; Stop SLIME's REPL from grabbing DEL, which is annoying when backspacing over a '('
  (define-key slime-repl-mode-map (read-kbd-macro paredit-backward-delete-key) nil)

  ;; Bind TAB to `indent-for-tab-command', as in regular Slime buffers.
  (define-key slime-repl-mode-map (kbd "TAB") 'indent-for-tab-command)

  (add-hook 'slime-repl-mode-hook 'sanityinc/slime-repl-setup))

(after-load 'auto-complete
  (add-to-list 'ac-modes 'slime-repl-mode))

(provide 'init-slime)
