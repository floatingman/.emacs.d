;; Standard location of personal dictionary
(setq ispell-personal-dictionary "~/.flydict")

;; Mostly taken from
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
;; if (aspell installed) { use aspell }
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
(setq ispell-program-name (executable-find "aspell"))
(setq ispell-extra-args
      (list "--sug-mode=fast" ;; ultra|fast|normal|bad-spellers
            "--lang=en_US"
            "--ignore=3"))

;; hunspell
;; (setq ispell-program-name "hunspell")
;; ;; just reset dictionary to the safe one "en_US" for hunspell.
;; ;; if we need use different dictionary, we specify it in command line arguments
;; (setq ispell-local-dictionary "en_US")
;; (setq ispell-local-dictionary-alist
;;       '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))

(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))

(defun my/enable-flyspell-prog-mode ()
  (interactive)
  (flyspell-prog-mode))

(use-package flyspell
  :defer t
  :diminish ""
  :init (add-hook 'prog-mode-hook #'my/enable-flyspell-prog-mode)
  :config
  (use-package helm-flyspell
    :init
    (define-key flyspell-mode-map (kbd "M-S") 'helm-flyspell-correct)))

(provide 'init-spelling)
