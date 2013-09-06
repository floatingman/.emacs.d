(require-package 'clojure-mode)
(require-package 'clojure-test-mode)
(require-package 'cljsbuild-mode)
(require-package 'elein)
(require-package 'nrepl)
(require-package 'slamhound)
(require-package 'ac-nrepl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slime with Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slime-clojure-repl-setup ()
  "Some REPL setup additional to that in durendal."
  (when (string-equal (slime-lisp-implementation-name) "clojure")
    (when (slime-inferior-process)
      (message "Setting up repl for clojure")
      (slime-redirect-inferior-output))

    (set-syntax-table clojure-mode-syntax-table)
    (setq lisp-indent-function 'clojure-indent-function)
    (let (font-lock-mode)
      (clojure-mode-font-lock-setup))))

(after-load 'slime-repl
  (add-hook 'slime-repl-mode-hook 'slime-clojure-repl-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nrepl with Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq nrepl-popup-stacktraces nil)

(after-load 'nrepl
  (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
  (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
  (after-load 'auto-complete
    '(add-to-list 'ac-modes 'nrepl-mode))

  (add-hook 'nrepl-mode-hook 'set-auto-complete-as-completion-at-point-function)
  (add-hook 'nrepl-interaction-mode-hook 'set-auto-complete-as-completion-at-point-function)
  (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
  (add-hook 'nrepl-mode-hook 'subword-mode)
  (add-hook 'nrepl-mode-hook 'paredit-mode)
  (define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)

  ;; nrepl isn't based on comint
  (add-hook 'nrepl-mode-hook
            (lambda () (setq show-trailing-whitespace nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc clojure tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'sanityinc/lisp-setup)
  (add-hook 'clojure-mode-hook 'subword-mode))



;; Use clojure-mode for clojurescript, since clojurescript-mode
;; pulls in Slime
(add-auto-mode 'clojure-mode "\\.cljs\\'")


(provide 'init-clojure)
