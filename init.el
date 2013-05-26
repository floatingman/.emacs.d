;;; init.el --- dnewman's configuration file

;; Turn off mouse interface early in startup to avoid momentary display
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; (when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen
;; (setq inhibit-startup-screen t)

(add-to-list 'load-path user-emacs-directory)
(require 'init-benchmarking)

(defconst *spell-check-support-enabled* nil)
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-carbon-emacs* (eq window-system 'mac))
(defconst *is-cocoa-emacs* (and *is-a-mac* (eq window-system 'ns)))
(defconst *is-windows* (eq system-type 'windows-nt))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-gui* (not (eq window-system nil)))

;;-----------------------------------------------------
;; initial libraries to load
;;-----------------------------------------------------
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-exec-path)

;;-----------------------------------------------------
;; Load configs for specific features and modes
;;-----------------------------------------------------

(require-package 'wgrep)
(require-package 'project-local-variables)
(require-package 'diminish)
(require-package 'scratch)
(require-package 'mwe-log-commands)

;; (require-package 'ace-jump-mode)
;; (require-package 'ag)
(require-package 'base16-theme)
(require-package 'browse-kill-ring)
(require-package 'clojure-mode)
;; (require-package 'coffee-mode)
;; (require-package 'color-theme)
(require-package 'deft)
;; (require-package 'dired+)
;; (require-package 'dired-details)
;; (require-package 'dired-single)(;(;
(require-package 'dropdown-list)
(require-package 'emms)
;; (require-package 'expand-region)
;; (require-package 'flymake-cursor)
;; (require-package 'git-commit-mode)
(require-package 'google-maps)
(require-package 'highlight-cl)
;; (require-package 'hl-sexp)
;; (require-package 'ido-ubiquitous)
(require-package 'iy-go-to-char)
(require-package 'jedi)
;;  (require-package 'js-comint)
;; (require-package 'js2-mode)
(require-package 'keyfreq)
(require-package 'lorem-ipsum)
;; (require-package 'magit)
;; (require-package 'markdown-mode+)
;; (require-package 'maxframe)
;; (require-package 'mo-git-blame)
;; (require-package 'multiple-cursors)
(require-package 'org)
;; (require-package 'paredit)
;(require-package 'php-mode)
(require-package 'powershell)
(require-package 'powershell-mode)
;; (require-package 'pretty-mode)
(require-package 'pylint)
;; (require-package 'rainbow-delimiters)
(require-package 'sass-mode)
(require-package 'smartrep)
(require-package 'smart-operator)
(require-package 'smart-tab)
;; (require-package 'smex)
(require-package 'tfs)
(require-package 'thesaurus)
(require-package 'tidy)
;; (require-package 'undo-tree)
(require-package 'yaml-mode)
(require-package 'yasnippet)


(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)
(require 'init-os-keys)
(require 'init-gui-frames)
(require 'init-maxframe)
(require 'init-dired)
(require 'init-isearch)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-flymake)

(require 'init-recentf)
(require 'init-ido)
(require 'init-hippie-expand)
(require 'init-auto-complete)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)
(require 'init-mmm)
(require 'init-growl)

(require 'init-editing-utils)

(require 'init-darcs)
(require 'init-git)

(require 'init-markdown)
(require 'init-csv)
(require 'init-erlang)
(provide 'init-javascript)
(provide 'init-sh)
(provide 'init-php)
(provide 'init-nxml)

(require 'init-paredit)
(require 'init-lisp)


(when *spell-check-support-enabled*
  (require 'init-spelling))

(require-package 'gnuplot)
(require-package 'lua-mode)
(require-package 'htmlize)
(require-package 'dsvn)
(require-package 'regex-tool) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow access from emacsclient ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'server)
(unless (server-running-p)
  (server-start))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables configured via the interactive 'customize' interface ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow users to provide an optional "init-local" containing personal settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'init-local nil t)


;;(setq site-lisp-dir (expand-file-name "vendor" user-emacs-directory))

;; ;; Set spell program if on windows
;; (if (eq system-type 'windows-nt)
;;     (setq ispell-program-name "~/.emacs.d/Aspell/bin/aspell.exe")
;;   )

;; load secret passwords
;;(ignore-errors (load-file "~/.emacs.d/secrets.el"))


;;;; package.el
			     ;(require 'package)
			     ;(setq package-user-dir "~/.emacs.d/elpa")
			     ;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives '("marmalade" .
;; "http://marmalade-repo.org/packages/") t)

;; may not be necessary
;;(setq url-proxy-services (quote (("http" . "localhost:9987"))))
			     ;(package-initialize)



;; (defun dn-install-rad-packages ()
;;   "Install packages if they are not already installed."
;;   (interactive)
;;   (package-refresh-contents)
;;   (mapc '(lambda (package)
;;	   (unless (package-installed-p package)
;;	     (package-install package)))
;;	'(ace-jump-mode
;;	  ag
;;	  base16-theme
;;	  browse-kill-ring
;;	  clojure-mode
;;	  coffee-mode
;;	  color-theme
;;	  deft
;;	  diminish
;;	  dired+
;;	  dired-details
;;	  dired-details+
;;	  dired-single
;;	  dropdown-list
;;	  emms
;;	  expand-region
;;	  flymake-cursor
;;	  git-commit-mode
;;	  google-maps
;;	  highlight-cl
;;	  hl-sexp
;;	  ido-ubiquitous
;;	  iy-go-to-char
;;	  jedi
;;	  js-comint
;;	  js2-mode
;;	  keyfreq
;;	  lorem-ipsum
;;	  magit
;;	  markdown-mode+
;;	  maxframe
;;	  mo-git-blame
;;	  multiple-cursors
;;	  org
;;	  paredit
;;	  php-mode
;;	  powershell
;;	  powershell-mode
;;	  pretty-mode
;;	  pylint
;;	  rainbow-delimiters
;;	  sass-mode
;;	  smartrep
;;	  smart-operator
;;	  smart-tab
;;	  smex
;;	  tfs
;;	  thesaurus
;;	  tidy
;;	  undo-tree
;;	  yaml-mode
;;	  yasnippet
;;	  )))

;;;; macros
;; (defmacro after (mode &rest body)
;;   "`eval-after-load' MODE evaluate BODY."
;;   (declare (indent defun))
;;   `(eval-after-load ,mode
;;      '(progn ,@body)))

;; ;;;; external libraries
;; (require 'checkdoc)
;; (require 'midnight)
;; (require 'misc)
;; (require 'recentf)
;; (require 'saveplace)
;; (require 'uniquify)

;; (setq dn-extra-paths
;;       '("~/.cabal/bin"
;;	"~/.bin/"
;;	))

;; (setenv "PATH"
;;	(mapconcat
;;	 'identity
;;	 (delete-dups
;;	  (append
;;	   (mapcar (lambda (path)
;;		     (if (string-match "^~" path)
;;			 (replace-match (getenv "HOME") nil nil path)
;;		       path)) dn-extra-paths)
;;	   (split-string (getenv "PATH") ":")))
;;	 ":"))

;; (mapc (lambda (path) (push path exec-path)) dn-extra-paths)

;; (ignore-errors (server-start))

;;;; aliases
;; (defalias 'qrr 'query-replace-regexp)
;; (defalias 'qr 'query-replace)
;; (defalias 'eshell/ff 'find-file)
;; (defalias 'eshell/ffow 'find-file-other-window)
;; (defalias 'yes-or-no-p 'y-or-n-p)

;;;; random number generator
;; (random t)



;; ;;;; remaps
;; (define-key key-translation-map (kbd "<C-tab>") (kbd "M-TAB"))
;; (define-key key-translation-map (kbd "C-x C-m") (kbd "M-x"))
;; (define-key key-translation-map (kbd "C-x C-d") (kbd "C-x d"))

;; ;;;; global key bindings
;; ;; F-Keys
;; (global-set-key [f1] 'menu-bar-mode)
;; (global-set-key [f2] 'ansi-term-visit-dwim)
;; (global-set-key [f4] 'goto-line)
;; (global-set-key [f5] 'bh/org-todo)
;; (global-set-key [S-f5] 'bh/widen)
;; (global-set-key (kbd "<C-f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
;; (global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))
;; (global-set-key [f7] 'bh/set-truncate-lines)
;; (global-set-key [f8] 'org-cycle-agenda-files)
;; (global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
;; (global-set-key (kbd "<f9> b") 'bbdb)
;; (global-set-key (kbd "<f9> c") 'calendar)
;; (global-set-key (kbd "<f9> f") 'boxquote-insert-file)
;; (global-set-key (kbd "<f9> g") 'gnus)
;; (global-set-key (kbd "<f9> h") 'bh/hide-other)
;; (global-set-key (kbd "<f9> n") 'org-narrow-to-subtree)
;; (global-set-key (kbd "<f9> w") 'widen)
;; (global-set-key (kbd "<f9> u") 'bh/narrow-up-one-level)
;; (global-set-key (kbd "<f9> I") 'bh/punch-in)
;; (global-set-key (kbd "<f9> O") 'bh/punch-out)
;; (global-set-key (kbd "<f9> o") 'bh/make-org-scratch)
;; (global-set-key (kbd "<f9> r") 'boxquote-region)
;; (global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)
;; (global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
;; (global-set-key (kbd "<f9> T") 'tabify)
;; (global-set-key (kbd "<f9> U") 'untabify)
;; (global-set-key (kbd "<f9> v") 'visible-mode)
;; (global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
;; (global-set-key (kbd "C-<f9>") 'previous-buffer)
;; (global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
;; (global-set-key (kbd "<f9> p") 'bh/phone-call)
;; (global-set-key (kbd "C-<f10>") 'next-buffer)
;; (global-set-key (kbd "<f11>") 'org-clock-goto)
;; (global-set-key (kbd "C-<f11>") 'org-clock-in)
;; (global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
;; (global-set-key [f12] 'org-agenda)

;; ;; a
;; (global-set-key (kbd "C-h a") 'apropos)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; ;; b
;; (global-set-key (kbd "C-x C-b") 'ibuffer)
;; (global-set-key (kbd "C-c b") 'org-iswitchb)
;; ;; c
;; (global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ;;copy
;; (global-set-key (kbd "M-c") 'calc-dispatch)
;; (global-set-key (kbd "C-c c")(lambda()(interactive)(djcb-duplicate-line t)))
;; ;; d
;; ;;(global-set-key (kbd "C-c d") 'insert-date)
;; ;;(global-set-key (kbd "C-c d") 'duplicate-line)
;; (global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; ;; e
;; ;; f
;; (global-set-key (kbd "C-c C-f") 'insert-file-name)
;; (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
;; (global-set-key (kbd "C-x f") 'recentf-ido-find-file)
;; ;; g
;; (global-set-key (kbd "C-c g c") 'mo-git-blame-current)
;; (global-set-key (kbd "C-c g f") 'mo-git-blame-file)

;; ;; h
;; 			     ;(global-set-key (kbd "C-M-h") 'backward-kill-word)
;; (global-set-key (kbd "s-h") 'windmove-left)
;; ;; i
;; (global-set-key (kbd "C-x C-i") 'imenu)
;; ;; j
;; (global-set-key (kbd "s-j") 'windmove-down)
;; ;; k
;; (global-set-key (kbd "M-k") 'kill-this-buffer)
;; (global-set-key (kbd "C-x k") 'kill-this-buffer)
;; (global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)
;; (global-set-key (kbd "C-x K") 'kill-other-buffer)
;; (global-set-key (kbd "C-x C-S-K") 'kill-other-buffer-and-window)
;; (global-set-key (kbd "s-k") 'windmove-up)
;; ;; l
;; 			     ;(global-set-key (kbd "C-c l") 'mark-line)
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "s-l") 'windmove-right)
;; ;; m
;; (global-set-key (kbd "C-x C-m") 'execute-extended-command)
;; (global-set-key (kbd "M-s m") 'multi-occur)
;; (global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)
;; ;; n
;; (global-set-key (kbd "M-n") 'next-error)
;; (global-set-key (kbd "C-c n") 'global-linum-mode)
;; (global-set-key (kbd "C-x n r") 'narrow-to-region)
;; ;; o
;; (global-set-key (kbd "M-o") 'other-window) ; was facemenu-keymap
;; (global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
;; (global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two
;; (define-key isearch-mode-map (kbd "C-o")
;;   (lambda () (interactive)
;;     (let ((case-fold-search isearch-case-fold-search))
;;       (occur (if isearch-regexp
;; 		 isearch-string
;; 	       (regexp-quote isearch-string))))))
;; (global-set-key (kbd "C-c o") 'occur)
;; ;; p
;; (global-set-key (kbd "M-p") 'previous-error)
;; (global-set-key (kbd "C-x C-p") 'find-file-at-point)
;; ;; q
;; ;; r
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-c r") 'revert-buffer)
;; (global-set-key (kbd "C-x C-r") 'rgrep)
;; (global-set-key (kbd "C-x r u") 'gse-number-rectangle)
;; (global-set-key (kbd "C-M-r") 'org-capture)
;; (global-set-key (kbd "C-c r") 'org-capture)
;; ;; s
;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; ;; t
;; (global-set-key (kbd "C-x t") 'deft)
;; ;; u
;; ;; v
;; (global-set-key (kbd "C-v") 'pager-page-down)
;; (global-set-key (kbd "s-v") 'clipboard-yank) ;;paste
;; ;; w
;; ;; x
;; (global-set-key (kbd "s-x") 'clipboard-kill-region) ;;cut
;; 			     ;(global-set-key (kbd "M-x") 'smex)
;; 			     ;(global-set-key (kbd "C-x x") 'smex)
;; 			     ;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; ;; y
;; (global-set-key (kbd "M-Y") 'yank-pop-backwards)
;; (global-set-key (kbd "C-c y") 'djcb-duplicate-line)
;; 			     ;(global-set-key (kbd "C-c y") 'bury-buffer)
;; ;; z
;; (global-set-key (kbd "C-z") 'undo)
;; ;; 1
;; (global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
;; ;; 2
;; (global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
;; ;; 3
;; (global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
;; ;; 4
;; ;; 5
;; ;; 6
;; ;; 7
;; ;; 8
;; ;; 9
;; ;; 0
;; (global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
;; ;; }
;; (global-set-key (kbd "C-M-}") 'forward-page)
;; ;; {
;; (global-set-key (kbd "C-M-{") 'backward-page)
;; ;; ;
;; (define-key global-map (kbd "C-;") 'iedit-mode)
;; (define-key isearch-mode-map (kbd "C-;") 'iedit-mode)
;; ;; up
;; (global-set-key (kbd "<C-S-up>")     'buf-move-up)
;; (global-set-key (kbd "<C-M-up>") 'move-text-up)
;; (global-set-key '[M-up] 'sacha/search-word-backward)
;; ;; down
;; (global-set-key (kbd "<C-S-down>")   'buf-move-down)
;; (global-set-key (kbd "<C-M-down>") 'move-text-down)
;; (global-set-key '[M-down] 'sacha/search-word-forward)
;; ;; left
;; (global-set-key (kbd "<C-S-left>")   'buf-move-left)
;; ;; right
;; (global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; ;; Misc keys
;; (global-set-key [next] 'pager-page-down)
;; (global-set-key "\ev" 'pager-page-up)
;; (global-set-key [prior] 'pager-page-up)
;; (global-set-key '[M-kp-8] 'pager-row-up)
;; (global-set-key '[M-kp-2] 'pager-row-down)
;; (global-set-key [mouse-3] 'mouse-buffer-menu)
;; (global-set-key (kbd "C-#") 'comment-region)
;; (global-set-key (kbd "C-\'") 'uncomment-region)
;; (global-set-key (kbd "C-x \\") 'align-regexp)
;; (global-set-key (kbd "M-/") 'hippie-expand)
;; (define-key global-map (kbd "C-+") 'text-scale-increase)
;; (define-key global-map (kbd "C--") 'text-scale-decrease)
;; (global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
;; (global-set-key (kbd "C-x ^") 'join-line)
;; (global-set-key (kbd "RET") 'newline-and-indent)

;; ;;;; advice
;; (defadvice kill-line (after kill-line-cleanup-whitespace activate compile)
;;   "cleanup whitespace on kill-line"
;;   (if (not (bolp))
;;       (delete-region (point) (progn (skip-chars-forward " \t") (point)))))

;;;; hooks
;;(add-hook 'write-file-functions 'time-stamp)

;;;; generic
;; (blink-cursor-mode nil)
;; (column-number-mode t)
;; (global-auto-revert-mode t)
;; (recentf-mode t)
;; (savehist-mode t)
;; (show-paren-mode t)
;; (visual-line-mode -1)
;; (winner-mode t)
;; (global-subword-mode t)
;; (delete-selection-mode t)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)
;; (which-function-mode t)

;;;; Windows specific


;;;; Linux specific

;;;; GUI settings
;; (when (display-graphic-p)
;;   (fringe-mode 1)
;;   (mouse-wheel-mode t)
;;   (menu-bar-mode t)
;;   (setq-default mac-option-modifier 'super)
;;   (setq-default mac-pass-command-to-system nil)
;;   (add-to-list 'initial-frame-alist '(left . 1))
;;   (add-to-list 'initial-frame-alist '(top . 1)))

;;;; faces
;; (make-face 'font-lock-number-face)
;; (set-face-attribute 'font-lock-number-face nil :inherit font-lock-constant-face)
;; (setq font-lock-number-face 'font-lock-number-face)
;; (defvar font-lock-number "[0-9-.]+\\([eE][+-]?[0-9]*\\)?")
;; (defvar font-lock-hexnumber "0[xX][0-9a-fA-F]+")
;; (defun add-font-lock-numbers (mode)
;;   (font-lock-add-keywords
;;    mode
;;    `((,(concat "\\<\\(" font-lock-number "\\)\\>" ) 0 font-lock-number-face)
;;      (,(concat "\\<\\(" font-lock-hexnumber "\\)\\>" ) 0 font-lock-number-face)
;;      )))

;; ;;;; hippie-expand
;; (setq hippie-expand-try-functions-list '(try-complete-file-name-partially
;; 					 try-complete-file-name
;; 					 try-expand-dabbrev
;; 					 try-expand-dabbrev-all-buffers
;; 					 try-expand-dabbrev-from-kill
;; 					 try-expand-all-abbrevs
;; 					 try-complete-lisp-symbol-partially
;; 					 try-complete-lisp-symbol))

;; ;;;; dired
;; (global-set-key (kbd "C-x C-j") 'dired-jump)
;; (define-key ctl-x-4-map (kbd "C-j") 'dired-jump-other-window)
;; (global-set-key (kbd "M-s f") 'find-name-dired)

;; (after 'dired
;;   (define-key dired-mode-map (kbd "M-p") 'dired-back-to-top)
;;   (define-key dired-mode-map (kbd "M-n") 'dired-jump-to-bottom)
;;   (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files))


;; ;;;; diminish
;; (after 'diminish-autoloads
;;   (after 'paredit (diminish 'paredit-mode " pe"))
;;   (after 'yasnippet (diminish 'yas-minor-mode " ys"))
;;   (after 'undo-tree (diminish 'undo-tree-mode " ut"))
;;   (after 'checkdoc (diminish 'checkdoc-minor-mode " cd")))


;; ;;;; browse-kill-ring
;; (after 'browse-kill-ring-autoloads
;;   (global-set-key (kbd "C-x C-y") 'browse-kill-ring))


;; ;;;; ibuffer
;; (setq ibuffer-saved-filter-groups
;;       '(("default"
;; 	 ("melpa" (filename . "melpa"))
;; 	 ("stonesoup" (filename . "stonesoup"))
;; 	 ("FYS" (filename . "FYS"))
;; 	 ("115" (filename . "115"))
;; 	 ("325" (filename . "325"))
;; 	 ("705" (filename . "705"))
;; 	 ("345" (filename . "345"))
;; 	 ("455" (filename . "455"))
;; 	 ("mulchn" (filename . "mulchn"))
;; 	 ("dirs" (or
;; 		  (mode . dired-mode)
;; 		  (mode . wdired-mode)))
;; 	 ("notes" (filename . "notes"))
;; 	 ("magit" (name . "\*magit"))
;; 	 ("help" (or (name . "\*Help\*")
;; 		     (name . "\*Apropos\*")
;; 		     (name . "\*info\*")))
;; 	 ("markdown" (mode . markdown-mode))
;; 	 ("econfig" (or (filename . ".emacs.d")
;; 			(filename . "init.el"))))))


;; (defun mp-ibuffer-hook ()
;;   (ibuffer-auto-mode 1)
;;   (ibuffer-switch-to-saved-filter-groups "default"))

;; (add-hook 'ibuffer-mode-hook 'mp-ibuffer-hook)

;; ;;;; surround-mode
;; (global-set-key (kbd "M-C") 'surround-change)

;; ;;;; change-inner
;; ;;global-set-key (kbd "M-I") 'change-inner)
;; ;;(global-set-key (kbd "M-O") 'change-outer)

;; ;;;; deft
;; (after 'deft
;;   (setq deft-directory (expand-file-name "~/git/org/deft"))
;;   (setq deft-text-mode 'org-mode)
;;   (setq deft-use-filename-as-title t))

;; ;;;; smartrep
;; (after 'smartrep-autoloads
;;   (require 'smartrep))

;; ;;;; term-mode
;; (setq system-uses-terminfo nil)

;; ;;;; helm
;; ;;(after 'helm-autoloads
;; ;;  (setq helm-ff-auto-update-initial-value nil)
;; ;;  (setq helm-quick-update t))


;; ;;;; ido
;; (ido-mode t)
;; (ido-everywhere t)
;; (setq ido-enable-flex-matching t)
;; (setq ido-auto-merge-work-directories-length nil)
;; (setq ido-create-new-buffer 'always)
;; (setq ido-everywhere t)
;; (setq ido-max-prospects 10)
;; (setq ido-read-file-name-non-ido nil)
;; (setq ido-use-filename-at-point nil)
;; (setq ido-use-virtual-buffers t)

;; (global-set-key (kbd "C-x f") 'find-file-in-project)
;; (define-key ctl-x-4-map (kbd "f") 'find-file-in-project-other-window)
;; (define-key ctl-x-4-map (kbd "s") 'shell-other-window)

;; (defun mp-ido-hook ()
;;   (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
;;   (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)
;;   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
;;   (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
;;   ;; (define-key ido-completion-map (kbd "C-e") 'mp-ido-edit-input)
;;   (define-key ido-completion-map [tab] 'ido-complete))

;; (add-hook 'ido-setup-hook 'mp-ido-hook)


;; ;;;; ido-ubiquitous
;; (after 'ido-ubiquitous-autoloads (ido-ubiquitous-mode t))
;; (after 'ido-ubiquitous (ido-ubiquitous-disable-in evil-ex))

;; (setq ido-ubiquitous-command-exceptions '(evil-ex execute-extended-command))
;; (setq ido-ubiquitous-function-exceptions '(grep-read-files ucs-insert))


;; ;;;; ido-vertical-mode
;; (after 'ido-vertical-mode-autoloads
;;   (ido-vertical-mode t))


;; ;;;; smex
;; (after 'smex-autoloads (smex-initialize))

;; (after 'smex
;;   (global-set-key (kbd "M-x") 'smex)
;;   (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;   (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))


;; ;;;; ispell
;; (setq ispell-list-command "list")

;; ;;;; diff commands
;; (add-to-list 'command-switch-alist '("-diff" . command-line-diff))

;; ;;;; org-mode
;; ;; (ignore-errors (load-file "~/.emacs.d/startup.d/62org.el"))

;; ;;;; yas/snippets
;; (after 'yasnippet
;;   (yas/reload-all)
;;   (setq yas/prompt-functions '(yas/ido-prompt yas/completing-prompt yas/no-prompt)))

;; (after 'yasnippet-autoloads
;;   (add-hook 'prog-mode-hook 'yas-minor-mode))

;; ;;;; js2-mode
;; (after 'js2-mode-autoloads
;;   (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))

;; ;;;; expand-region
;; (global-set-key (kbd "C-=") 'er/expand-region)

;; ;;;; jump-char
;; (after 'jump-char-autoloads
;;   (global-set-key (kbd "M-m") 'jump-char-forward)
;;   (global-set-key (kbd "M-M") 'jump-char-backward))

;; (after 'jump-char
;;   (setq jump-char-lazy-highlight-face nil))

;; ;;;; ace-jump-mode
;; (define-key global-map (kbd "C-;") 'ace-jump-mode)

;; ;;;; wrap-region
;; (after 'wrap-region-autoloads
;;   (setq wrap-region-only-with-negative-prefix t)
;;   (wrap-region-global-mode t))


;; ;;;; multiple-cursors
;; (after 'multiple-cursors-autoloads
;;   (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;   (global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
;;   (global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
;;   (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;   (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;   (global-set-key (kbd "C-<return>") 'mc/mark-more-like-this-extended)
;;   (global-set-key (kbd "C-S-SPC") 'set-rectangular-region-anchor)
;;   (global-set-key (kbd "C-M-=") 'mc/insert-numbers)
;;   (global-set-key (kbd "C-*") 'mc/mark-all-like-this)
;;   (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))

;; ;;;; rainbow-delimiters
;; (after 'rainbow-delimiters-autoloads
;;   (add-hook 'prog-mode-hook 'rainbow-delimiters-mode-enable))

;; ;;;; undo-tree
;; (after 'undo-tree-autoloads
;;   (global-undo-tree-mode t)
;;   (setq undo-tree-visualizer-relative-timestamps t)
;;   (setq undo-tree-visualizer-timestamps t))

;; ;;;; magit
;; (global-set-key (kbd "C-x g") 'magit-status)

;; (defun magit-toggle-whitespace ()
;;   (interactive)
;;   (if (member "-w" magit-diff-options)
;;       (magit-dont-ignore-whitespace)
;;     (magit-ignore-whitespace)))

;; (defun magit-ignore-whitespace ()
;;   (interactive)
;;   (add-to-list 'magit-diff-options "-w")
;;   (magit-refresh))

;; (defun magit-dont-ignore-whitespace ()
;;   (interactive)
;;   (setq magit-diff-options (remove "-w" magit-diff-options))
;;   (magit-refresh))

;; (defun magit-quit-session ()
;;   "Restore the previous window configuration and kill the magit buffer."
;;   (interactive)
;;   (kill-buffer)
;;   (jump-to-register :magit-fullscreen))

;; (after 'magit
;;   (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)
;;   (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;;   ;; full screen magit-status
;;   (defadvice magit-status (around magit-fullscreen activate)
;;     (window-configuration-to-register :magit-fullscreen)
;;     ad-do-it
;;     (delete-other-windows)))


;; ;;;; markdown
;; (setq auto-mode-alist
;;       (cons '("\\.te?xt\\'" . markdown-mode) auto-mode-alist))
;; (setq auto-mode-alist
;;       (cons '("\\.mm?d\\'" . markdown-mode) auto-mode-alist))
;; (setq markdown-command "pandoc -S")

;; (after 'markdown-mode
;;   (remove-hook 'text-mode-hook 'turn-on-auto-fill)
;;   (define-key markdown-mode-map (kbd "<backtab>") 'markdown-shifttab)
;;   (define-key markdown-mode-map (kbd "C-c r") 'markdown-copy-rtf)
;;   (define-key markdown-mode-map (kbd "C-c l") 'markdown-export-latex)
;;   (define-key markdown-mode-map (kbd "C-c v") 'marked)
;;   (define-key markdown-mode-map (kbd "C-c w") 'markdown-select-section-copy-paste)
;;   (define-key markdown-mode-map (kbd "C-c s") 'markdown-select-section)
;;   (define-key markdown-mode-map (kbd "C-c c") 'markdown-copy-html)
;;   (define-key markdown-mode-map (kbd "C-c p") 'markdown-export-pdf)
;;   (define-key markdown-mode-map (kbd "C-c q") 'markdown-copy-paste-html)
;;   (define-key markdown-mode-map (kbd "C-c =") 'markdown-cleanup-list-numbers))

;; (add-hook 'markdown-mode-hook 'abbrev-mode)
;; (add-hook 'markdown-mode-hook 'toggle-word-wrap)

;; ;;;; prog-mode
;; (defun mp-buffer-enable-whitespace-cleanup ()
;;   "enable whitespace-cleanup in the current buffer"
;;   (interactive)
;;   (add-hook 'before-save-hook 'whitespace-cleanup nil t))

;; (defun mp-buffer-disable-whitespace-cleanup ()
;;   "enable whitespace-cleanup in the current buffer"
;;   (interactive)
;;   (remove-hook 'before-save-hook 'whitespace-cleanup t))

;; (add-hook 'prog-mode-hook 'mp-buffer-enable-whitespace-cleanup)
;; (add-hook 'prog-mode-hook 'whitespace-mode)
;; ;; (add-hook 'prog-mode-hook 'hl-line-mode)
;; ;; (add-hook 'prog-mode-hook 'toggle-truncate-lines)

;; ;;;; emacs lisp
;; (defun imenu-elisp-sections ()
;;   (setq imenu-prev-index-position-function nil)
;;   (add-to-list 'imenu-generic-expression '("Sections" "^;;;; \\(.+\\)$" 1) t))

;; (add-hook 'emacs-lisp-mode-hook 'imenu-elisp-sections)
;; (add-hook 'emacs-lisp-mode-hook 'hl-sexp-mode)

;; (font-lock-add-keywords
;;  'emacs-lisp-mode
;;  '(("'\\([0-9a-zA-Z-]*\\)" (1 'font-lock-variable-name-face))))
;; (add-font-lock-numbers 'emacs-lisp-mode)

;; (defun mp-buffer-enable-reindent ()
;;   "Enable `indent-buffer' on the current buffer."
;;   (interactive)
;;   (add-hook 'before-save-hook 'indent-buffer nil t))

;; (defun mp-buffer-disable-reindent ()
;;   "Enable `indent-buffer' on the current buffer."
;;   (interactive)
;;   (remove-hook 'before-save-hook 'indent-buffer t))

;; (add-hook 'emacs-lisp-mode-hook 'mp-buffer-enable-reindent)
;; (add-hook 'emacs-lisp-mode-hook 'checkdoc-minor-mode)

;; ;;;; clojure
;; (add-hook 'clojure-mode-hook 'mp-buffer-enable-reindent)

;; ;;;; paredit
;; (after 'paredit-autoloads (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
;; (after 'paredit-autoloads (add-hook 'clojure-mode-hook 'paredit-mode))


;; ;;;; auto-complete
;; (after 'auto-complete
;;   (setq ac-auto-show-menu nil)
;;   (setq ac-use-menu-map t)
;;   (define-key ac-menu-map (kbd "C-p") 'ac-previous)
;;   (define-key ac-menu-map (kbd "C-n") 'ac-next)
;;   ;; (define-key ac-menu-map "\C-p" 'ac-previous)
;;   ;; (define-key ac-menu-map "\C-n" 'ac-next)
;;   (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict"))

;; (after 'auto-complete-config
;;   ;; (ac-config-default)
;;   (add-hook 'ein:notebook-multilang-mode-hook 'auto-complete-mode)
;;   (setq-default ac-sources (append '(ac-source-yasnippet ac-source-imenu) ac-sources))
;;   (when (file-exists-p (expand-file-name "~/.emacs.d/elisp/Pymacs"))
;;     (ac-ropemacs-initialize)
;;     (ac-ropemacs-setup)))

;; (after 'auto-complete-autoloads
;;   (require 'auto-complete-config))

;; (when (file-exists-p (expand-file-name "~/.emacs.d/elisp/Pymacs"))
;;   (setq ropemacs-enable-autoimport t)
;;   (add-to-list 'load-path "~/.emacs.d/elisp/Pymacs"))

;; ;;;; html
;; (add-font-lock-numbers 'html-mode)

;; ;;;; php
;; (setq auto-mode-alist
;;       (cons '("\\.php[345]?\\'\\|\\.phtml\\." . php-mode) auto-mode-alist))


;;;; defun
;; (defvar yank-indent-modes '(prog-mode
;; 			    js2-mode)
;;   "Modes in which to indent regions that are yanked (or yank-popped)")

;; (defvar yank-advised-indent-threshold 1000
;;   "Threshold (# chars) over which indentation does not automatically occur.")

;; (defun yank-advised-indent-function (beg end)
;;   "Do indentation, as long as the region isn't too large."
;;   (if (<= (- end beg) yank-advised-indent-threshold)
;;       (indent-region beg end nil)))

;; (defadvice yank (after yank-indent activate)
;;   "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
;;   (if (and (not (ad-get-arg 0))
;; 	   (member major-mode yank-indent-modes))
;;       (let ((transient-mark-mode nil))
;; 	(yank-advised-indent-function (region-beginning) (region-end)))))

;; (defadvice yank-pop (after yank-pop-indent activate)
;;   "If current mode is one of 'yank-indent-modes, indent yanked text (with prefix arg don't indent)."
;;   (if (and (not (ad-get-arg 0))
;; 	   (member major-mode yank-indent-modes))
;;       (let ((transient-mark-mode nil))
;; 	(yank-advised-indent-function (region-beginning) (region-end)))))

;; (defun yank-unindented ()
;;   (interactive)
;;   (yank 1))

;; (defun increment-number-at-point ()
;;   (interactive)
;;   (skip-chars-backward "0123456789")
;;   (or (looking-at "[0123456789]+")
;;       (error "No number at point"))
;;   (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;; (defun decrement-number-at-point ()
;;   (interactive)
;;   (skip-chars-backward "0123456789")
;;   (or (looking-at "[0123456789]+")
;;       (error "No number at point"))
;;   (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

;; (defun delete-current-buffer-file ()
;;   "Deletes current buffer and file it is visiting."
;;   (interactive)
;;   (let ((filename (buffer-file-name)))
;;     (if (not (and filename (file-exists-p filename)))
;; 	(error "Buffer '%s' is not visiting a file!" name)
;;       (delete-file filename)
;;       (kill-this-buffer))))

;; (defun rename-current-buffer-file ()
;;   "Renames current buffer and file it is visiting."
;;   (interactive)
;;   (let ((name (buffer-name))
;; 	(filename (buffer-file-name)))
;;     (if (not (and filename (file-exists-p filename)))
;; 	(error "Buffer '%s' is not visiting a file!" name)
;;       (let ((new-name (read-file-name "New name: " filename)))
;; 	(cond ((get-buffer new-name)
;; 	       (error "A buffer named '%s' already exists!" new-name))
;; 	      (t
;; 	       (rename-file filename new-name 1)
;; 	       (rename-buffer new-name)
;; 	       (set-visited-file-name new-name)
;; 	       (set-buffer-modified-p nil)
;; 	       (message "File '%s' successfully renamed to '%s'"
;; 			name (file-name-nondirectory new-name))))))))


;; (defun command-line-diff (switch)
;;   (let ((file1 (pop command-line-args-left))
;; 	(file2 (pop command-line-args-left)))
;;     (ediff file1 file2)))

;; (defun paste-previous-osx-app ()
;;   "paste the current buffer into the previous OS X application.
;; either the one specified in the '.meta' file or the previously
;; used app."
;;   (interactive)
;;   (do-applescript
;;    (concat
;;     (let ((metafn (concat (buffer-file-name) ".meta")))
;;       (cond
;;        ((and (buffer-file-name) (file-exists-p metafn))
;; 	(save-buffer)
;; 	(with-temp-buffer
;; 	  (insert-file-contents-literally metafn)
;; 	  (goto-char (point-min))
;; 	  (do-applescript
;; 	   (concat
;; 	    "tell application \""
;; 	    (buffer-substring-no-properties (point-at-bol) (point-at-eol))
;; 	    "\" to activate"))))
;;        (t
;; 	"
;; tell application \"System Events\" to keystroke tab using {command down}
;; delay 0.2"
;; 	)))
;;     "
;; tell application \"System Events\" to keystroke \"a\" using {command down}
;; tell application \"System Events\" to keystroke \"v\" using {command down}")))


;; (defun markdown-select-section-copy-paste (level)
;;   "Select the LEVEL or current, when nil, section of markdown and copy and paste it."
;;   (interactive "P")
;;   (markdown-select-section level)
;;   (copy-paste))

;; (defun copy-paste ()
;;   "Copy the buffer and paste it into the previous buffer or that determined by the '.meta' file."
;;   (interactive)
;;   (save-excursion
;;     (let ((begin-region)
;; 	  (end-region))
;;       (if (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
;; 	  (setq begin-region (region-beginning)
;; 		end-region (region-end))
;; 	(setq begin-region (point-min)
;; 	      end-region (point-max)))
;;       (kill-ring-save begin-region end-region))
;;     (paste-previous-osx-app)))



;; (defun close-frame-or-client (&optional args)
;;   (interactive "P")
;;   (if (> (length (frame-list)) 1)
;;       (progn (save-some-buffers)
;; 	     (delete-frame))))


;; (defun unfill-paragraph ()
;;   "Takes a multi-line paragraph and makes it into a single line of text."
;;   (interactive)
;;   (let ((fill-column (point-max)))
;;     (fill-paragraph nil)))


;; (defun comment-dwim-line (&optional arg)
;;   "Replacement for the comment-dwim command.
;;    If no region is selected and current line is not blank and we
;;    are not at the end of the line, then comment current line.
;;    Replaces default behaviour of comment-dwim, when it inserts
;;    comment at the end of the line."
;;   (interactive "*P")
;;   (comment-normalize-vars)
;;   (if (not (region-active-p))
;;       (comment-or-uncomment-region
;;        (line-beginning-position) (line-end-position))
;;     (comment-dwim arg)))


;; (defun duplicate-current-line-or-region (arg)
;;   "Duplicates the current line or region ARG times.
;; If there's no region, the current line will be duplicated."
;;   (interactive "p")
;;   (save-excursion
;;     (if (region-active-p)
;; 	(duplicate-region arg)
;;       (duplicate-current-line arg))))


;; (defun duplicate-region (num &optional start end)
;;   "Duplicates the region bounded by START and END NUM times.
;; If no START and END is provided, the current region-beginning and
;; region-end is used."
;;   (interactive "p")
;;   (let* ((start (or start (region-beginning)))
;; 	 (end (or end (region-end)))
;; 	 (region (buffer-substring start end)))
;;     (goto-char start)
;;     (dotimes (i num)
;;       (insert region))))


;; (defun duplicate-current-line (num)
;;   "Duplicate the current line NUM times."
;;   (interactive "p")
;;   (when (eq (point-at-eol) (point-max))
;;     (goto-char (point-max))
;;     (newline)
;;     (forward-char -1))
;;   (duplicate-region num (point-at-bol) (1+ (point-at-eol))))



;; (defun finder ()
;;   "Open the current working directory in finder."
;;   (interactive)
;;   (shell-command (concat "open " (shell-quote-argument default-directory))))

;; (defun marked ()
;;   "Open the current file in Marked."
;;   (interactive)
;;   (when (buffer-file-name)
;;     (save-buffer)
;;     (shell-command (concat "open -a Marked "
;; 			   (shell-quote-argument buffer-file-name)))))

;; (defun make-executable ()
;;   "Make the current file loaded in the buffer executable"
;;   (interactive)
;;   (if (buffer-file-name)
;;       (start-file-process "Make Executable" nil "/bin/bash" "-c"
;; 			  (concat "chmod u+x " (file-name-nondirectory buffer-file-name)))
;;     (message "Buffer has no filename.")))


;; (defun width-80 ()
;;   (interactive)
;;   (set-window-margins (selected-window) 0 0)
;;   (let ((marginwidth (/ (- (window-width) 80) 2)))
;;     (set-window-margins (selected-window) marginwidth marginwidth)))


;; (defun setup-local-iterm ()
;;   "locally define C-c C-c to run the iterm-run-previous-command"
;;   (interactive)
;;   (local-set-key (kbd "C-c C-c") 'iterm-run-previous-command))


;; (defun iterm-run-previous-command ()
;;   "applescript to switch to iTerm and run the previously run command"
;;   (interactive)
;;   (save-buffer)
;;   (do-applescript "
;; tell application \"Terminal\"
;; activate
;; tell application \"System Events\"
;; keystroke \"p\" using {control down}
;; keystroke return
;; end tell
;; end tell"))


;; (defun align-to-equals (begin end)
;;   "Align region to equal signs"
;;   (interactive "r")
;;   (align-regexp begin end "\\(\\s-*\\)=" 1 1 ))



;; (defun mpround ()
;;   "round the current floating-point"
;;   (interactive)
;;   (save-excursion
;;     (let* ((start (point)) (end (point)))
;;       (forward-word 2)
;;       (setq end (point))
;;       (insert
;;        (number-to-string
;; 	(/ (round
;; 	    (* (string-to-number
;; 		(buffer-substring-no-properties start end)) 1000.0))  1000.0)))
;;       (delete-region start end))))



;; (defun what-face (pos)
;;   (interactive "d")
;;   (let ((face (or (get-char-property (point) 'read-face-name)
;; 		  (get-char-property (point) 'face))))
;;     (if face (message "Face: %s" face) (message "No face at %d" pos))))


;; (defun open-previous-line (arg)
;;   "Open a new line before the current one.
;;      See also `newline-and-indent'."
;;   (interactive "p")
;;   (when (eolp)
;;     (save-excursion
;;       (delete-region (point)
;; 		     (progn (skip-chars-backward " \t") (point)))))
;;   (beginning-of-line)
;;   (open-line arg)
;;   (indent-according-to-mode))


;; (defun open-next-line (arg)
;;   "Move to the next line and then opens a line.
;;     See also `newline-and-indent'."
;;   (interactive "p")
;;   (end-of-line)
;;   (newline-and-indent))


;; (defun open-line-indent (n)
;;   "Insert a new line and leave point before it. With arg N insert N newlines."
;;   (interactive "*p")
;;   (save-excursion
;;     (newline n)
;;     (indent-according-to-mode)))


;; (defun new-line-in-between ()
;;   (interactive)
;;   (newline)
;;   (save-excursion
;;     (newline)
;;     (indent-for-tab-command))
;;   (indent-for-tab-command))


;; (defun kmacro-edit-lossage ()
;;   "Edit most recent 300 keystrokes as a keyboard macro."
;;   (interactive)
;;   (kmacro-push-ring)
;;   (edit-kbd-macro 'view-lossage))


;; (defun TeX-compile ()
;;   "Start a viewer without confirmation.
;; The viewer is started either on region or master file,
;; depending on the last command issued."
;;   (interactive)
;;   (TeX-save-document (TeX-master-file))
;;   (TeX-command "LaTeX" 'TeX-active-master 0))


;; (defun compile-make ()
;;   (interactive)
;;   (save-buffer)
;;   (compile "make -k"))


;; (defun font-lock-restart ()
;;   (interactive)
;;   (setq font-lock-mode-major-mode nil)
;;   (font-lock-fontify-buffer))

;; (defun c-snug-if (syntax pos)
;;   "Dynamically calculate brace hanginess for do-while statements.
;; Using this function, `while' clauses that end a `do-while' block will
;; remain on the same line as the brace that closes that block.

;; See `c-hanging-braces-alist' for how to utilize this function as an
;; ACTION associated with `block-close' syntax."
;;   (save-excursion
;;     (let (langelem)
;;       (if (and (eq syntax 'substatement-open)
;; 	       (setq langelem (assq 'substatement-open c-syntactic-context))
;; 	       (progn (goto-char (c-langelem-pos langelem))
;; 		      (if (eq (char-after) ?{)
;; 			  (c-safe (c-forward-sexp -1)))
;; 		      (looking-at "\\<if\\>[^_]")))
;; 	  '(after)
;; 	'(before after)))))

;; (defun swap-windows ()
;;   "If you have 2 windows, it swaps them."
;;   (interactive)
;;   (cond ((not (= (count-windows) 2))
;; 	 (message "You need exactly 2 windows to do this."))
;; 	(t
;; 	 (let* ((w1 (first (window-list)))
;; 		(w2 (second (window-list)))
;; 		(b1 (window-buffer w1))
;; 		(b2 (window-buffer w2))
;; 		(s1 (window-start w1))
;; 		(s2 (window-start w2)))
;; 	   (set-window-buffer w1 b2)
;; 	   (set-window-buffer w2 b1)
;; 	   (set-window-start w1 s2)
;; 	   (set-window-start w2 s1)))))


;; (defun orgtbl-to-pandoc-cell (val colwidth align)
;;   "Convert an `org-mode' table cell to pandoc.

;; Format VAL for COLWIDTH column and specified ALIGN."
;;   (setq colwidth (+ 2 colwidth))
;;   (if align
;;       (concat (make-string (- colwidth (length val)) ? ) val)
;;     (concat val (make-string (- colwidth (length val)) ? ))))


;; (defun orgtbl-to-pandoc (table params)
;;   "Convert `orgtbl' TABLE from to a pandoc table with given PARAMS."
;;   (let* ((splicep (plist-get params :splice))
;; 	 (html-table-tag org-export-html-table-tag)
;; 	 html)
;;     ;; Just call the formatter we already have
;;     ;; We need to make text lines for it, so put the fields back together.
;;     (concat "\n"
;; 	    (mapconcat
;; 	     'identity
;; 	     (mapcar
;; 	      (lambda (x)
;; 		(if (eq x 'hline)
;; 		    (mapconcat
;; 		     'identity
;; 		     (mapcar
;; 		      (lambda (colwidth)
;; 			(make-string (1+ colwidth) ?-))
;; 		      org-table-last-column-widths) " ")
;; 		  (mapconcat
;; 		   'identity
;; 		   (mapcar*
;; 		    'orgtbl-to-pandoc-cell
;; 		    x
;; 		    org-table-last-column-widths
;; 		    org-table-last-alignment) " ")))
;; 	      table)
;; 	     "\n")
;; 	    "\n")))


;; (defvar wikipedia-url "http://en.wikipedia.org/wiki/%s" "Wikipedia URL")


;; (defun wikicase (str)
;;   "change string to wikipedia case"
;;   (mapconcat 'capitalize (split-string str) "_"))


;; (defun markdown-wikipedia-link ()
;;   "Insert a link to wikipedia based on the name of the current keyword link."
;;   (interactive)
;;   (save-excursion
;;     (back-to-indentation)
;;     (re-search-forward "\\[\\(.+\\)\\]:" (point-at-eol))
;;     (end-of-line)
;;     (insert (format wikipedia-url (wikicase (match-string 1))))))

;; ;; kill region if active, otherwise kill backward word
;; (defun kill-region-or-backward-word (arg)
;;   (interactive "p")
;;   (if (region-active-p)
;;       (kill-region (region-beginning) (region-end))
;;     (call-interactively (key-binding (kbd "M-<DEL>")) t (this-command-keys-vector))))

;; (defun kill-to-beginning-of-line ()
;;   (interactive)
;;   (kill-region (save-excursion (beginning-of-line) (point))
;; 	       (point)))

;; (defun kill-and-retry-line ()
;;   "Kill the entire current line and reposition point at indentation"
;;   (interactive)
;;   (back-to-indentation)
;;   (kill-line))

;; ;; copy region if active
;; ;; otherwise copy to end of current line
;; ;;   * with prefix, copy N whole lines

;; (defun copy-to-end-of-line ()
;;   (interactive)
;;   (kill-ring-save (point)
;; 		  (line-end-position))
;;   (message "Copied to end of line"))

;; (defun copy-whole-lines (arg)
;;   "Copy ARG lines to the kill ring"
;;   (interactive "p")
;;   (kill-ring-save (line-beginning-position)
;; 		  (line-beginning-position (+ 1 arg)))
;;   (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

;; (defun copy-line (arg)
;;   "Copy to end of line, or ARG lines."
;;   (interactive "P")
;;   (if (null arg)
;;       (copy-to-end-of-line)
;;     (copy-whole-lines (prefix-numeric-value arg))))

;; (defun save-region-or-current-line (arg)
;;   (interactive "P")
;;   (if (region-active-p)
;;       (kill-ring-save (region-beginning) (region-end))
;;     (copy-line arg)))


;; ;; M-up is nicer in dired if it moves to the third line - straight to the ".."
;; (defun dired-back-to-top ()
;;   (interactive)
;;   (beginning-of-buffer)
;;   (next-line 2)
;;   (dired-back-to-start-of-files))

;; ;; M-down is nicer in dired if it moves to the last file
;; (defun dired-jump-to-bottom ()
;;   (interactive)
;;   (end-of-buffer)
;;   (next-line -1)
;;   (dired-back-to-start-of-files))

;; ;; C-a is nicer in dired if it moves back to start of files
;; (defun dired-back-to-start-of-files ()
;;   (interactive)
;;   (backward-char (- (current-column) 2)))

;; (defun create-scratch-buffer nil
;;   "create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
;;   (interactive)
;;   (let ((n 0)
;; 	bufname)
;;     (while (progn
;; 	     (setq bufname (concat "*scratch"
;; 				   (if (= n 0) "" (int-to-string n))
;; 				   "*"))
;; 	     (setq n (1+ n))
;; 	     (get-buffer bufname)))
;;     (switch-to-buffer (get-buffer-create bufname))
;;     (emacs-lisp-mode)))

;; (defun toggle-window-split ()
;;   (interactive)
;;   (if (= (count-windows) 2)
;;       (let* ((this-win-buffer (window-buffer))
;; 	     (next-win-buffer (window-buffer (next-window)))
;; 	     (this-win-edges (window-edges (selected-window)))
;; 	     (next-win-edges (window-edges (next-window)))
;; 	     (this-win-2nd (not (and (<= (car this-win-edges)
;; 					 (car next-win-edges))
;; 				     (<= (cadr this-win-edges)
;; 					 (cadr next-win-edges)))))
;; 	     (splitter
;; 	      (if (= (car this-win-edges)
;; 		     (car (window-edges (next-window))))
;; 		  'split-window-horizontally
;; 		'split-window-vertically)))
;; 	(delete-other-windows)
;; 	(let ((first-win (selected-window)))
;; 	  (funcall splitter)
;; 	  (if this-win-2nd (other-window 1))
;; 	  (set-window-buffer (selected-window) this-win-buffer)
;; 	  (set-window-buffer (next-window) next-win-buffer)
;; 	  (select-window first-win)
;; 	  (if this-win-2nd (other-window 1))))))

;; (defun rotate-windows ()
;;   "Rotate your windows"
;;   (interactive)
;;   (cond ((not (> (count-windows)1))
;; 	 (message "You can't rotate a single window!"))
;; 	(t
;; 	 (setq i 1)
;; 	 (setq numWindows (count-windows))
;; 	 (while  (< i numWindows)
;; 	   (let* (
;; 		  (w1 (elt (window-list) i))
;; 		  (w2 (elt (window-list) (+ (% i numWindows) 1)))

;; 		  (b1 (window-buffer w1))
;; 		  (b2 (window-buffer w2))

;; 		  (s1 (window-start w1))
;; 		  (s2 (window-start w2))
;; 		  )
;; 	     (set-window-buffer w1  b2)
;; 	     (set-window-buffer w2 b1)
;; 	     (set-window-start w1 s2)
;; 	     (set-window-start w2 s1)
;; 	     (setq i (1+ i)))))))


;; (defun run-prog-mode-hook ()
;;   (run-hooks 'prog-mode-hook))

;; (defun move-line-down ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (next-line)
;;       (transpose-lines 1))
;;     (next-line)
;;     (move-to-column col)))

;; (defun move-line-up ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (next-line)
;;       (transpose-lines -1))
;;     (move-to-column col)))

;; (defun untabify-buffer ()
;;   (interactive)
;;   (untabify (point-min) (point-max)))

;; (defun tabify-buffer ()
;;   (interactive)
;;   (tabify (point-min) (point-max)))

;; (defun indent-buffer ()
;;   (interactive)
;;   (indent-region (point-min) (point-max)))

;; (defun cleanup-buffer-safe ()
;;   "Perform a bunch of safe operations on the whitespace content of a buffer.
;; Does not indent buffer, because it is used for a before-save-hook, and that
;; might be bad."
;;   (interactive)
;;   (untabify-buffer)
;;   (delete-trailing-whitespace)
;;   (set-buffer-file-coding-system 'utf-8))

;; (defun cleanup-buffer ()
;;   "Perform a bunch of operations on the whitespace content of a buffer.
;; Including indent-buffer, which should not be called automatically on save."
;;   (interactive)
;;   (cleanup-buffer-safe)
;;   (indent-buffer))

;; (defun other-window-reverse ()
;;   "Select the other window but in reverse."
;;   (interactive)
;;   (other-window -1))

;; (defun shell-other-window (&optional buffer)
;;   (interactive
;;    (list
;;     (and current-prefix-arg
;; 	 (prog1
;; 	     (read-buffer "Shell buffer: "
;; 			  (generate-new-buffer-name "*shell*"))
;; 	   (if (file-remote-p default-directory)
;; 	       ;; It must be possible to declare a local default-directory.
;; 	       ;; FIXME: This can't be right: it changes the default-directory
;; 	       ;; of the current-buffer rather than of the *shell* buffer.
;; 	       (setq default-directory
;; 		     (expand-file-name
;; 		      (read-directory-name
;; 		       "Default directory: " default-directory default-directory
;; 		       t nil))))))))
;;   (let ((buffer (save-window-excursion
;; 		  (shell buffer))))
;;     (switch-to-buffer-other-window buffer)))

;; (defun find-file-in-project-other-window ()
;;   "Find a file in the current project in the other window."
;;   (interactive)
;;   (let ((buffer (save-window-excursion (find-file-in-project))))
;;     (switch-to-buffer-other-window buffer)))

;; (defun eval-and-replace ()
;;   "Replace the preceding sexp with its value."
;;   (interactive)
;;   (backward-kill-sexp)
;;   (condition-case nil
;;       (prin1 (eval (read (current-kill 0)))
;; 	     (current-buffer))
;;     (error (message "Invalid expression")
;; 	   (insert (current-kill 0)))))


;; (defun hippie-expand-line ()
;;   (interactive)
;;   (let ((hippie-expand-try-functions-list '(try-expand-line
;; 					    try-expand-line-all-buffers)))
;;     (hippie-expand nil)))


;; ;; toggle quotes
;; (defun current-quotes-char ()
;;   (nth 3 (syntax-ppss)))

;; (defalias 'point-is-in-string-p 'current-quotes-char)

;; (defun move-point-forward-out-of-string ()
;;   (while (point-is-in-string-p) (forward-char)))

;; (defun move-point-backward-out-of-string ()
;;   (while (point-is-in-string-p) (backward-char)))

;; (defun alternate-quotes-char ()
;;   (if (eq ?' (current-quotes-char)) ?\" ?'))

;; (defun toggle-quotes ()
;;   (interactive)
;;   (if (point-is-in-string-p)
;;       (let ((old-quotes (char-to-string (current-quotes-char)))
;; 	    (new-quotes (char-to-string (alternate-quotes-char)))
;; 	    (start (make-marker))
;; 	    (end (make-marker)))
;; 	(save-excursion
;; 	  (move-point-forward-out-of-string)
;; 	  (backward-delete-char 1)
;; 	  (set-marker end (point))
;; 	  (insert new-quotes)
;; 	  (move-point-backward-out-of-string)
;; 	  (delete-char 1)
;; 	  (insert new-quotes)
;; 	  (set-marker start (point))
;; 	  (replace-string new-quotes (concat "\\" new-quotes) nil start end)
;; 	  (replace-string (concat "\\" old-quotes) old-quotes nil start end)))
;;     (error "Point isn't in a string")))

;; End:
