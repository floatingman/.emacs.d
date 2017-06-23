(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

;; C-c C-v l : elint current buffer in clean environment.
;; C-c C-v L : elint current buffer by multiple emacs binaries.
;;             See `erefactor-lint-emacsen'
;; C-c C-v r : Rename symbol in current buffer.
;;             Resolve `let' binding as long as i can.
;; C-c C-v R : Rename symbol in requiring modules and current buffer.
;; C-c C-v h : Highlight current symbol in this buffer
;;             and suppress `erefacthr-highlight-mode'.
;; C-c C-v d : Dehighlight all by above command.
;; C-c C-v c : Switch prefix bunch of symbols.
;;             ex: '(hoge-var hoge-func) -> '(foo-var foo-func)
;; C-c C-v ? : Display flymake elint warnings/errors

(when (>= emacs-major-version 25)
  (eval-after-load 'bytecomp
    '(add-to-list 'byte-compile-not-obsolete-funcs
                  'preceding-sexp)))

(use-package erefactor
  :ensure t
  :config
  (define-key emacs-lisp-mode-map "\C-c\C-v" erefactor-map))

(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
(bind-key "C-c f" 'find-function)

(defun my/sort-sexps-in-region (beg end)
    "Can be handy for sorting out duplicates.
Sorts the sexps from BEG to END. Leaves the point at where it
couldn't figure things out (ex: syntax errors)."
    (interactive "r")
    (let ((input (buffer-substring beg end))
          list last-point form result)
      (save-restriction
        (save-excursion
          (narrow-to-region beg end)
          (goto-char (point-min))
          (setq last-point (point-min))
          (setq form t)
          (while (and form (not (eobp)))
            (setq form (ignore-errors (read (current-buffer))))
            (when form
              (add-to-list
               'list
               (cons
                (prin1-to-string form)
                (buffer-substring last-point (point))))
              (setq last-point (point))))
          (setq list (sort list (lambda (a b) (string< (car a) (car b)))))
          (delete-region (point-min) (point))
          (insert (mapconcat 'cdr list "\n"))))))

(use-package paredit
  :ensure t
  :commands paredit-mode
  :diminish paredit-mode
  :init (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
  :config
  (use-package paredit-ext
    :load-path "site-lisp")
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

(use-package redshank
  :defer t
  :ensure t
  :diminish "RS"
  :init (add-hook 'emacs-lisp-mode-hook 'redshank-mode))

(bind-key "M-:" 'pp-eval-expression)

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

(bind-key "C-x C-e" 'sanityinc/eval-last-sexp-or-region emacs-lisp-mode-map)

(provide 'init-lisp)
