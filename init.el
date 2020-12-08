;; turn on emacs debugging to diagnose errors
;;  (setq debug-on-init t)
;;  (setq debug-on-error t)

  (let ((minver "24.5"))
    (when (version< emacs-version minver)
      (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
  (when (version< emacs-version "25.1")
    (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(defun sanityinc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))


(defvar sanityinc/require-times nil
  "A list of (FEATURE LOAD-START-TIME LOAD-DURATION).
LOAD-DURATION is the time taken in milliseconds to load FEATURE.")

(defun sanityinc/require-times-wrapper (orig feature &rest args)
  "Note in `sanityinc/require-times' the time taken to require each feature."
  (let* ((already-loaded (memq feature features))
         (require-start-time (and (not already-loaded) (current-time))))
    (prog1
        (apply orig feature args)
      (when (and (not already-loaded) (memq feature features))
        (let ((time (sanityinc/time-subtract-millis (current-time) require-start-time)))
          (add-to-list 'sanityinc/require-times
                       (list feature require-start-time time)
                       t))))))

(advice-add 'require :around 'sanityinc/require-times-wrapper)

(define-derived-mode sanityinc/require-times-mode tabulated-list-mode "Require-Times"
  "Show times taken to `require' packages."
  (setq tabulated-list-format
        [("Start time (ms)" 20 sanityinc/require-times-sort-by-start-time-pred)
         ("Feature" 30 t)
         ("Time (ms)" 12 sanityinc/require-times-sort-by-load-time-pred)])
  (setq tabulated-list-sort-key (cons "Start time (ms)" nil))
  ;; (setq tabulated-list-padding 2)
  (setq tabulated-list-entries #'sanityinc/require-times-tabulated-list-entries)
  (tabulated-list-init-header)
  (when (fboundp 'tablist-minor-mode)
    (tablist-minor-mode)))

(defun sanityinc/require-times-sort-by-start-time-pred (entry1 entry2)
  (< (string-to-number (elt (nth 1 entry1) 0))
     (string-to-number (elt (nth 1 entry2) 0))))

(defun sanityinc/require-times-sort-by-load-time-pred (entry1 entry2)
  (> (string-to-number (elt (nth 1 entry1) 2))
     (string-to-number (elt (nth 1 entry2) 2))))

(defun sanityinc/require-times-tabulated-list-entries ()
  (cl-loop for (feature start-time millis) in sanityinc/require-times
           with order = 0
           do (incf order)
           collect (list order
                         (vector
                          (format "%.3f" (sanityinc/time-subtract-millis start-time before-init-time))
                          (symbol-name feature)
                          (format "%.3f" millis)))))

(defun sanityinc/require-times ()
  "Show a tabular view of how long various libraries took to load."
  (interactive)
  (with-current-buffer (get-buffer-create "*Require Times*")
    (sanityinc/require-times-mode)
    (tabulated-list-revert)
    (display-buffer (current-buffer))))



(defun sanityinc/show-init-time ()
  (message "init completed in %.2fms"
           (sanityinc/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'sanityinc/show-init-time)

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))
(defconst *is-gui* (display-graphic-p))
(defconst my/gui? (display-graphic-p))
(defvar bootstrap-version nil
  "Used by the straight package manager.")
(defconst my/emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")
(defconst my/local-dir
  (concat my/emacs-dir ".local/")
  "Root directory for local storage. Must end with a slash.")
(defconst my/cache-dir (concat my/local-dir "cache/")
  "Directory for volatile local storage. Must end with a slash.")
(defconst my/leader "SPC"
  "Global prefix used in `general' keybindings.")

(defconst my/color-cyan "#93E0E3")
(defconst my/color-gray "#5F5F5F")

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))


(package-initialize)
(unless package-archive-contents
    (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
    (package-install 'use-package))
(require 'use-package)

;; Uncomment this to get a reading on packages that get loaded at startup
;; (setq use-package-verbose t)

;; ensure packages by default
;; (setq use-package-always-ensure t)

(use-package fullframe
  :ensure t)
(fullframe list-packages quit-window)


(let ((package-check-signature nil))
  (use-package gnu-elpa-keyring-update
    :ensure t))


(defun sanityinc/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format
	     when (string= col-name (car column))
	     do (setf (elt column 1) width))))

(defun sanityinc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (sanityinc/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
  (sanityinc/set-tabulated-list-column-width "Archive" longest-archive-name))))

(add-hook 'package-menu-mode-hook 'sanityinc/maybe-widen-package-menu-columns)

;; Remove top menubar
(menu-bar-mode -1)

;; Remove top tool bar (only respected in GUI Emacs).
(when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; Remove scroll bar (only respected in GUI Emacs).
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))


;; Never save backup files
(setq make-backup-files nil)

;; UTF-8 as the default coding system.
(set-charset-priority 'unicode)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      selection-coding-system 'utf-8)

;; Smooth scrolling
(setq scroll-margin 2
      scroll-conservatively 9999
      scroll-step 1)

;; Do not use lockfiles to avoid editing collisions.
(setq create-lockfiles nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; Keep track of saved places in ~/.emacs.d/places
(save-place-mode 1)

;; Do not automatically save changes.
(setq auto-save-default nil
      auto-save-list-file-name (concat my/cache-dir "autosave"))

;; Enable folding by indentation, just like Vim when using Evil
(add-hook 'prog-mode-hook #'hs-minor-mode)

;; No cursor blinking
(blink-cursor-mode 0)

;; No word-wrap
(set-default 'truncate-lines t)

;; Enable winner mode so that I can undo/redo window changes.
(winner-mode 1)

;; Don't stretch the cursor to fit wide charactires, it is disorienting
;; especially for tabs
(setq x-stretch-cursor nil)

;; Full path in title bar.
;; %b -- print buffer name.
;; %f -- print visited file name.
(setq-default frame-title-format "%b (%f)")

;; No bell.
(setq ring-bell-function 'ignore)

;; Changes all yes/no questions to y/n type.
(fset 'yes-or-no-p 'y-or-n-p)

;; Highlight matching parenthesis.
(add-hook 'prog-mode-hook #'show-paren-mode)

;; Don't highlight trailing whitespace
(setq-default show-trailing-whitespace nil)

;; Remove trailing whitespace before saving.
(add-hook 'before-save-hook #'whitespace-cleanup)

;; Enable mouse in terminal Emacs
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

;; Remove hscroll-margin in shells, otherwise it causes jumpiness
(dolist (mode '(eshell-mode-hook term-mode-hook vterm-mode-hook))
  (add-hook mode #'my/remove-horizontal-scroll-margin-in-shells))

;; Always avoid GUI.
(setq use-dialog-box nil)

;; Split ediff windows side-by-side (similar to vimdiff)
(setq ediff-split-window-function #'split-window-horizontally)

;; Follow symlinks without asking.
(setq vc-follow-symlinks t
      find-file-visit-truename t)

;; Save custom settings in the cache directory.
(setq custom-file (concat my/cache-dir "custom.el"))

;; Indentation
(setq-default tab-width 4
              tab-always-indent t
              indent-tabs-mode nil
              fill-column 80)

(define-obsolete-function-alias 'after-load 'with-eval-after-load "")

;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; Like diminish, but for major modes
(defun sanityinc/set-major-mode-name (name)
  "Override the major mode NAME in this buffer."
  (setq-local mode-name name))

(defun sanityinc/major-mode-lighter (mode name)
  (add-hook (derived-mode-hook-name mode)
            (apply-partially 'sanityinc/set-major-mode-name name)))

;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))
(defun my/remove-horizontal-scroll-margin-in-shells ()
  "Remove scroll margin to prevent jumpiness in shell(s) mode."
  (setq-local hscroll-margin 0))

(defun my/buffer-file-name ()
  "Return the current buffer file name taking into account dired-mode."
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun my/copy-file-absolute-name-to-clipboard ()
  "Copy the current buffer absolute file name to the clipboard."
  (interactive)
  (kill-new (my/buffer-file-name)))

(eval-when-compile (require 'cl))
(defun sanityinc/add-subdirs-to-load-path (parent-dir)
  "Adds every non-hidden subdir of PARENT-DIR to `load-path'."
  (let* ((default-directory parent-dir))
    (progn
      (setq load-path
            (append
             (remove-if-not
              (lambda (dir) (file-directory-p dir))
              (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
             load-path)))))

(sanityinc/add-subdirs-to-load-path
 (expand-file-name "site-lisp/" user-emacs-directory))

;;; Utilities for grabbing upstream libs

(defun site-lisp-dir-for (name)
  (expand-file-name (format "site-lisp/%s" name) user-emacs-directory))

(defun site-lisp-library-el-path (name)
  (expand-file-name (format "%s.el" name) (site-lisp-dir-for name)))

(defun download-site-lisp-module (name url)
  (let ((dir (site-lisp-dir-for name)))
    (message "Downloading %s from %s" name url)
    (unless (file-directory-p dir)
      (make-directory dir t))
    (add-to-list 'load-path dir)
    (let ((el-file (site-lisp-library-el-path name)))
      (url-copy-file url el-file t nil)
      el-file)))

(defun straight-lib-from-url (name url)
  (unless (site-lisp-library-loadable-p name)
    (byte-compile-file (download-site-lisp-module name url))))

(defun site-lisp-library-loadable-p (name)
  "Return whether or not the library `name' can be loaded from a
source file under ~/.emacs.d/site-lisp/name/"
  (let ((f (locate-library (symbol-name name))))
    (and f (string-prefix-p (file-name-as-directory (site-lisp-dir-for name)) f))))

(use-package exec-path-from-shell
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'exec-path-from-shell
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
      (add-to-list 'exec-path-from-shell-variables var)))


  (when (or (memq window-system '(mac ns x))
            (unless (memq system-type '(ms-dos windows-nt))
              (daemonp)))
    (exec-path-from-shell-initialize)))

(use-package diminish
:ensure t)
  (use-package scratch
:ensure t)
  (use-package command-log-mode
:ensure t)

;; More convenient key definitions.
(use-package general
  :ensure t
  :demand t

  :config
  (general-define-key
   ;; Kill the current buffer by default.
   "C-x k" #'kill-this-buffer))

;; Let's be honest here, there's nothing more productive than vi
;; key bindings in the right hands.
(use-package evil
  :ensure t

  :preface
  ;; Do not load evil keybindings, because we'll use
  ;; from the evil-collection package.
  (setq evil-want-keybinding nil)

  :init
  (defun my/evil-vim-split ()
    "Splits the current window horizontally and switch to the new window."
    (interactive)
    (evil-window-split)
    (evil-window-down 1))

  (defun my/evil-vim-vsplit ()
    "Splits the current window vertically and switch to the new window."
    (interactive)
    (evil-window-vsplit)
    (evil-window-right 1))

  (defun my/evil-enable-visual-line-navigation ()
    "Simulate evil navigation in `visual-line-mode'."
    (define-key evil-motion-state-map "0" #'evil-beginning-of-visual-line)
    (define-key evil-motion-state-map "$" #'evil-end-of-visual-line)
    (define-key evil-motion-state-map "j" #'evil-next-visual-line)
    (define-key evil-motion-state-map "k" #'evil-previous-visual-line))

  ;; Allows jumping back and forth between special buffers too.
  (setq evil--jumps-buffer-targets "\\*")

  ;; Always start in the normal mode. This is required, for example, to not enter
  ;; the git commit mode in insert mode. More often than not I have to navigate
  ;; across the diff before knowing what to write in the commit message.
  (add-hook 'with-editor-mode-hook #'evil-normal-state)

  ;; With visual-line-mode enabled it's better to navigate by visual line.
  (add-hook 'visual-line-mode-hook #'my/evil-enable-visual-line-navigation)

  ;; Always center current line while searching.
  (defadvice evil-search-next
      (after advice-for-evil-search-next activate)
    (evil-scroll-line-to-center (line-number-at-pos)))
  (defadvice evil-search-previous
      (after advice-for-evil-search-previous activate)
    (evil-scroll-line-to-center (line-number-at-pos)))

  :config
  ;; Split like Vim, i.e. moves to the newly created window.
  (evil-ex-define-cmd "split"  #'my/evil-vim-split)
  (evil-ex-define-cmd "vsplit" #'my/evil-vim-vsplit)

  (general-define-key
   :keymaps 'evil-normal-state-map
   "C-]" #'evil-goto-definition
   ;; Remove bindings conflicting with default Emacs behavior.
   "M-." nil
   "C-p" nil
   "C-n" nil)

  (evil-mode 1))


;; Add evil bindings beyond the default like calendar and help-mode.
(use-package evil-collection
  :ensure t

  :after
  (evil)

  :config
  (evil-collection-init))

(setq user-full-name "Daniel Newman"
      user-mail-address "dwnewman78@gmail.com")

(use-package diminish
  :ensure t
  :init (diminish 'auto-fill-function ""))

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)
(with-eval-after-load 'autorevert
  (diminish 'auto-revert-mode))

(use-package list-unicode-display
  :ensure t)
(defun sanityinc/locale-var-encoding (v)
  "Return the encoding portion of the locale string V, or nil if missing."
  (when v
    (save-match-data
      (let ((case-fold-search t))
        (when (string-match "\\.\\([^.]*\\)\\'" v)
          (intern (downcase (match-string 1 v))))))))

(dolist (varname '("LC_ALL" "LANG" "LC_CTYPE"))
  (let ((encoding (sanityinc/locale-var-encoding (getenv varname))))
    (unless (memq encoding '(nil utf8 utf-8))
      (message "Warning: non-UTF8 encoding in environment variable %s may cause interop problems with this Emacs configuration." varname))))

(delete-selection-mode 1)

(setq large-file-warning-threshold (* 25 1024 1024))

(transient-mark-mode 1)

(setq-default indicate-empty-lines nil)
(setq-default indicate-buffer-boundaries nil)

(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

(when (fboundp 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode))

(use-package goto-line-preview
  :ensure t
  :config
  (global-set-key [remap goto-line] 'goto-line-preview))
(when (fboundp 'display-line-numbers-mode)
  (defun sanityinc/with-display-line-numbers (f &rest args)
    (let ((display-line-numbers t))
      (apply f args)))
  (advice-add 'goto-line-preview :around #'sanityinc/with-display-line-numbers))

(line-number-mode 1)
(column-number-mode 1)

(use-package page-break-lines
  :ensure t
  :diminish t
  :hook (after-init . global-page-break-lines-mode))

(setq read-file-name-completion-ignore-case t)

(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

(setq line-move-visual t)

(setq make-pointer-invisible t)

(setq-default fill-column 80)
(setq-default default-tab-width 2)

(setq system-uses-terminfo nil)

(setq-default find-file-visit-truename t)

(setq require-final-newline t)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
;; This is usually bound to `C-M-l', but that locks the screen on linux, so bind
;; it to something I can use
(global-set-key (kbd "M-L") 'reposition-window)

(global-set-key (kbd "C-x k") #'kill-this-buffer)

(setq sentence-end-double-space nil)

(setq split-height-threshold nil)
(setq split-width-threshold 180)

(set-default 'imenu-auto-rescan t)

(random t)

(setq diff-switches "-u")

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defvar-local sanityinc/suspended-modes-during-cua-rect nil
  "Modes that should be re-activated when cua-rect selection is done.")

(eval-after-load 'cua-rect
  (advice-add 'cua--deactivate-rectangle :after
              (lambda (&rest _)
                (dolist (m sanityinc/suspended-modes-during-cua-rect)
                  (funcall m 1)
                  (setq sanityinc/suspended-modes-during-cua-rect nil)))))

(defun sanityinc/suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (eval-after-load 'cua-rect
    (advice-add 'cua--activate-rectangle :after
                (lambda (&rest _)
                  (when (bound-and-true-p mode-name)
                    (add-to-list 'sanityinc/suspended-modes-during-cua-rect mode-name)
                    (funcall mode-name 0))))))

(sanityinc/suspend-mode-during-cua-rect-selection 'whole-line-or-region-local-mode)

(setq calc-display-sci-low -5)

(use-package immortal-scratch
  :ensure t
  :hook (after-init . immortal-scratch-mode))

(when (boundp 'global-prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (push '("lambda" . ?λ) prettify-symbols-alist)))
  (add-hook 'clojure-mode-hook
            (lambda ()
              (push '("fn" . ?ƒ) prettify-symbols-alist)))
  (global-prettify-symbols-mode +1))

(setq tls-program
      ;; Defaults:
      ;; '("gnutls-cli --insecure -p %p %h"
      ;;   "gnutls-cli --insecure -p %p %h --protocols ssl3"
      ;;   "openssl s_client -connect %h:%p -no_ssl2 -ign_eof")
      '(;;"gnutls-cli -p %p %h"
        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

(global-set-key (kbd "C-x C-l") #'toggle-truncate-lines)

(setq save-interprogram-paste-before-kill t)

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'org-mode-hook #'ws-butler-mode)
  (add-hook 'text-mode-hook #'ws-butler-mode))

(use-package auto-indent-mode
  :ensure t)

(use-package shrink-whitespace
  :ensure t
  :bind ("M-SPC" . shrink-whitespace))

(use-package anzu
  :ensure t
  :defer t
  :bind ("M-%" . anzu-query-replace-regexp)
  :config
  (progn
    (use-package thingatpt)
    (setq anzu-mode-lighter ""
          ;; spaceline already takes care of this
          anzu-cons-mode-line-p nil)
    (set-face-attribute 'anzu-mode-line nil :foreground "yellow")))

(add-hook 'prog-mode-hook #'anzu-mode)
(add-hook 'org-mode-hook #'anzu-mode)

(defun isearch-yank-symbol ()
  (interactive)
  (isearch-yank-internal (lambda () (forward-symbol 1) (point))))

(define-key isearch-mode-map (kbd "C-M-w") #'isearch-yank-symbol)

;; IMPORTANT: if defered, eyebrowse won't work hydra.
(use-package eyebrowse
  :ensure t
  :init
  ;; Use the scratch buffer when creating new tabs.
  (setq eyebrowse-new-workspace t)

  ;; Cycle through tabs.
  (setq eyebrowse-wrap-around t)

  :config
  (eyebrowse-mode t))

(use-package buffer-move
  :ensure t
  :defer t)

(use-package deadgrep
  :ensure t
  :commands (deadgrep)
  :config
  (with-eval-after-load 'evil
    ;; Update jump list before leaving the deadgrep buffer.
    (evil-add-command-properties #'deadgrep-visit-result :jump t)))

(use-package hydra
  :ensure t
  :defer t

  :init
  (defun my/counsel-projectile-switch-project-action-dired (project)
    "Open dired when switching projects with counsel-projectile."
    (let ((projectile-switch-project-action
           (lambda ()
             (projectile-dired))))
      (counsel-projectile-switch-project-by-name project)))

  (defun my/counsel-projectile-switch-project-dotfiles ()
    "Open my dotfiles project straightaway."
    (interactive)
    (my/counsel-projectile-switch-project-action-dired "~/.dotfiles"))

  (defun my/dired-dotfiles-toggle ()
    "Show/hide dotfiles"
    (interactive)
    (when (equal major-mode 'dired-mode)
      ;; If currently showing
      (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p)
          (progn
            (set (make-local-variable 'dired-dotfiles-show-p) nil)
            (dired-mark-files-regexp "^\\\.")
            (dired-do-kill-lines))
        ;; Otherwise just revert to re-show
        (progn (revert-buffer)
               (set (make-local-variable 'dired-dotfiles-show-p) t)))))

  (defun my/async-shell-command-no-window (command)
    "Execute string COMMAND asynchronously without opening buffer."
    (interactive "sAsync shell command: ")
    (let* ((buffer-name "*Async Shell Command*")
           (output-buffer (get-buffer-create buffer-name))
           (process (let ((display-buffer-alist (list (list buffer-name #'display-buffer-no-window))))
                      (async-shell-command command output-buffer)
                      (get-buffer-process output-buffer)))
           (sentinel `(lambda (process signal)
                        (when (memq (process-status process) '(exit signal))
                          (shell-command-sentinel process signal)
                          ;; Here you could run arbitrary code when the
                          ;; command is successful.
                          ;; (when (zerop (process-exit-status process))
                          ;;   (message "%s" ,cmd))
                          ))))
      (when (process-live-p process)
        (set-process-sentinel process sentinel))))

  (defun my/async-shell-region-no-window (begin end)
    "Execute the REGION as a COMMAND asynchronously without opening buffer."
    (interactive "r")
    (my/async-shell-command-no-window
     (buffer-substring-no-properties begin end)))

  (defun my/projectile-run-async-shell-command-no-window-in-root ()
    "Invoke `my/async-shell-command-no-window' in the project's root."
    (interactive)
    (projectile-with-default-dir (projectile-ensure-project (projectile-project-root))
      (call-interactively 'my/async-shell-command-no-window)))

  (defun my/window-resize-right (arg)
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun my/window-resize-left (arg)
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun my/window-resize-up (arg)
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
      (enlarge-window arg)
      (shrink-window arg)))

  (defun my/window-resize-down (arg)
    (interactive "p")
    (if (let ((windmove-wrap-around))
          (windmove-find-other-window 'up))
      (shrink-window arg)
      (enlarge-window arg)))

  :config
  ;; For more hydra examples, have a look at:
  ;; https://github.com/abo-abo/hydra/blob/master/hydra-examples.el

  (defhydra hydra-projectile (:hint nil :foreign-keys nil :exit t :idle 0.5)
    "
PROJECT: %(projectile-project-root)
 ^Search^                ^Buffers^                    ^Cache^                 ^Command
^^^^^^^----------------------------------------------------------------------------------------------
 _s_: rg (mini-buffer)   _a_: alternate               _c_: cache clear        _r_ run async
 _S_: rg (deadgrep)      _A_: alternate (other win)   _i_: include project    _R_ run async no window
 ^ ^                     _b_: switch to buffer        _x_: remove project     _C_ compile
 ^ ^                     _d_: dired                   _X_: cleanup projects   _T_ test
 ^ ^                     _K_: kill all buffers        _z_: cache current
"
    ("A" projectile-find-implementation-or-test-other-window)
    ("C" projectile-compile-project)
    ("K" projectile-kill-buffers)
    ("R" my/projectile-run-async-shell-command-no-window-in-root)
    ("S" deadgrep)
    ("T" projectile-test-project)
    ("X" projectile-cleanup-known-projects)
    ("a" projectile-toggle-between-implementation-and-test)
    ("b" counsel-projectile-switch-to-buffer)
    ("c" projectile-invalidate-cache)
    ("d" projectile-dired)
    ("i" projectile-add-known-project)
    ("r" projectile-run-async-shell-command-in-root)
    ("s" counsel-projectile-rg)
    ("x" projectile-remove-known-project)
    ("z" projectile-cache-current-file)

    ("p" counsel-projectile-switch-project "switch project")
    ("." my/counsel-projectile-switch-project-dotfiles "switch dotfiles")
    ("q" nil "quit"))

  (defhydra hydra-dired (:hint nil :foreign-keys run :exit nil)
    "
_v_: view         _m_: mark           _l_: redisplay       _i_: insert subdir   wdired
_V_: view other   _u_: unmark         _g_: refresh         _$_: hide subdir     C-x C-q: edit
_o_: open other   _U_: unmark all     _=_: diff            _w_: kill subdir     C-c C-c: commit
_M_: chmod        _t_: toggle marks   _s_: sort            _X_: shell command   C-c ESC: abort
_G_: chgrp        _S_: symlink        _H_: toggle hidden
_O_: chown        _Z_: zip/unzip
^ ^
"
    ("$" diredp-hide-subdir-nomove)
    ("=" diredp-ediff)
    ("G" dired-do-chgrp)
    ("H" my/dired-dotfiles-toggle)
    ("M" dired-do-chmod)
    ("O" dired-do-chown)
    ("S" dired-do-symlink)
    ("T" dired-hide-details-mode)
    ("U" dired-unmark-all-marks)
    ("V" dired-display-file)
    ("X" dired-do-shell-command)
    ("Z" dired-do-compress)
    ("e" dired-ediff-files)
    ("g" revert-buffer)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)
    ("m" dired-mark)
    ("o" dired-find-file-other-window)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)
    ("w" dired-kill-subdir)

    ("C" dired-do-copy          "copy")
    ("D" dired-do-delete        "remove")
    ("+" dired-create-directory "mkdir")
    ("R" dired-do-rename        "rename")
    ("q" nil                    "quit"))

  (defhydra hydra-window (:hint nil :exit nil)
    ("=" balance-windows "balance windows")
    ("h" buf-move-left "swap left")
    ("j" buf-move-down "swap down")
    ("k" buf-move-up "swap up")
    ("l" buf-move-right "swap right")
    ("u" winner-undo "undo")
    ("r" winner-redo "redo")
    ("q" nil "quit"))

  (defhydra hydra-tab (:hint nil :exit nil)
    ("+" eyebrowse-create-window-config "create")
    ("-" eyebrowse-close-window-config "remove")
    ("l" eyebrowse-next-window-config "next")
    ("h" eyebrowse-prev-window-config "previous")
    ("q" nil "quit"))

  (general-define-key
   :prefix "C-c"
   "w" #'hydra-window/body
   "p" #'hydra-projectile/body
   "t" #'hydra-tab/body
   "s" #'deos/hydra-skeleton/body
   "u" #'hydra-undo-tree/undo-tree-undo
   "A" #'deos/hydra-about-emacs/body
    )

  (general-define-key
   :keymaps 'dired-mode-map
   "C-c d" #'hydra-dired/body))

(defhydra deos/hydra-about-emacs ()
  "
    About Emacs                                                        [_q_] quit
    ^^--------------------------------------------------------------------------
    PID:             %s(emacs-pid)
    Uptime:          %s(emacs-uptime)
    Init time:       %s(emacs-init-time)
    Directory:       %s(identity user-emacs-directory)
    Invoked from:    %s(concat invocation-directory invocation-name)
    Version:         %s(identity emacs-version)

    User Info
    ^^--------------------------------------------------------------------------
    User name:       %s(user-full-name)
    Login (real):    %s(user-login-name) (%s(user-real-login-name))
      UID (real):    %s(user-uid) (%s(user-real-uid))
      GID (real):    %s(group-gid) (%s(group-real-gid))
    Mail address:    %s(identity user-mail-address)

    System Info
    ^^--------------------------------------------------------------------------
    System name:     %s(system-name)
    System type:     %s(identity system-type)
    System config:   %s(identity system-configuration)
    "
  ("q" nil nil))

(defhydra deos/hydra nil
"
╭────────────────────────────────────────────────────────╯
  [_E_] ERC       [_m_] Mail
  [_t_] Toggle map       [_T_] Twitter
  [_g_] Gnus
  [_p_] Proced           [_W_] Weather   [(] Macros
  [_c_] Multi-compile    [_R_] RSS       [`] Errors
  [_d_] Downloads        [_D_] Debbugs

  [_q_] quit
"
  ("t" deos/hydra-toggle-map/body :exit t)
  ("T" deos/start-or-jump-to-twitter :exit t)
  ("g" gnus :exit t)
  ("d" deos/popup-downloads :exit t)
  ("D" debbugs-gnu :exit t)
  ("m" deos/switch-to-mail :exit t)
  ("c" multi-compile-run :exit t)
  ("E" (when (y-or-n-p "Really start ERC?") (start-erc)) :exit t)
  ("R" elfeed :exit t)
  ("p" proced :exit t)
  ("W" wttrin :exit t)
  ("q" nil :exit t))

;; Bind the main EOS hydra to M-t
(global-set-key (kbd "M-t") 'deos/hydra/body)

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode t)
  :defer t
  :diminish ""
  :config
  (progn
    (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize)
    (define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)))
(defhydra hydra-undo-tree (:color yello :hint nil)
  "
  _p_: undo _n_: redo _s_: save _l_: load  "
  ("p"   undo-tree-undo)
  ("n"   undo-tree-redo)
  ("s"   undo-tree-save-history)
  ("l"   undo-tree-load-history)
  ("u"   undo-tree-visualize "visualize" :color blue)
  ("q"   nil "quit" :color blue))

(defun tsp/gpg-version ()
  "Return the version of gpg as a string"
  (save-window-excursion
    (with-temp-buffer
      (shell-command (concat epg-gpg-program " --version") (current-buffer))
      (goto-char (point-min))
      (string-match "gpg (GnuPG) \\(.*\\)" (buffer-string))
      (tsp/str-chomp
       (match-string 1)))))

(setq epg-gpg-program "gpg2")

(require 'ispell)
;; Standard location of personal dictionary
(setq ispell-personal-dictionary "~/.flydict")
(when (executable-find ispell-program-name)
  ;; Add spell-checking in comments for all programming language modes
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-;") nil)
    (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(defun find-config ()
  "Edit config.org"
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c I") 'find-config)

(defun sanityinc/maybe-suspend-frame ()
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'sanityinc/maybe-suspend-frame)

(setq use-file-dialog nil)
(setq inhibit-startup-screen t)

(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(when (and *is-a-mac* (fboundp 'toggle-frame-fullscreen))
  ;; Command-Option-f to toggle fullscreen mode
  ;; Hint: Customize `ns-use-native-fullscreen'
  (global-set-key (kbd "M-ƒ") 'toggle-frame-fullscreen))

;; TODO: use seethru package instead?
(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))


(when *is-a-mac*
  (when (use-package ns-auto-titlebar
:ensure t)
    (ns-auto-titlebar-mode)))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (setq line-spacing 0)))

(use-package recentf
  :no-require t
  :hook (kill-emacs . recentf-cleanup)
  :init
  (setq recentf-save-file (concat my/cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-max-menu-items 0
        recentf-max-saved-items 200)

  :config
  ;; This hook should be in the config section because otherwise I get a
  ;; "function definition is void" error for the `recentf-add-file' function.
  (defun my/recentf-add-dired-directory ()
    "Add dired directory to recentf file list."
    (recentf-add-file default-directory))

  (add-hook 'dired-mode-hook #'my/recentf-add-dired-directory)

  (recentf-mode +1))

(use-package helpful
  :ensure t
  :commands (helpful--read-symbol)
  :init
  (general-define-key
   [remap describe-key] #'helpful-key
   "C-h ." #'helpful-at-point))

(use-package color-theme-sanityinc-solarized
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package zenburn-theme
  :ensure t
  :defer t)

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(sanityinc-tomorrow-eighties))

;; Make certain that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))


(use-package dimmer
  :ensure t
  :init
  (setq-default dimmer-fraction 0.15)
  :hook (after-init . dimmer-mode)
  :config
  (with-eval-after-load 'dimmer
    ;; TODO: file upstream as a PR
    (advice-add 'frame-set-background-mode :after (lambda (&rest args) (dimmer-process-all))))
  (with-eval-after-load 'dimmer
    ;; Don't dim in terminal windows. Even with 256 colours it can
    ;; lead to poor contrast.  Better would be to vary dimmer-fraction
    ;; according to frame type.
    (defun sanityinc/display-non-graphic-p ()
      (not (display-graphic-p)))
    (add-to-list 'dimmer-exclusion-predicates 'sanityinc/display-non-graphic-p)))

;; A minimal and modern mode-line.
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . my/init-load-theme)
  :preface
  ;; I'm not happy on where custom-set-faces are being set inside of
  ;; doom-modeline.
  (defun my/init-load-theme ()
    (doom-modeline-mode +1)
    (load-theme 'zenburn t nil)

    (custom-set-faces
     `(ivy-current-match              ((t :background ,my/color-gray :foreground nil :underline unspecified :weight unspecified)))
     `(ivy-highlight-face             ((t :background nil :foreground nil :underline unspecified :weight unspecified)))
     `(ivy-minibuffer-match-face-1    ((t :background nil :inherit bold)))
     `(ivy-minibuffer-match-face-2    ((t :background nil :foreground ,my/color-cyan :underline t)))
     `(ivy-minibuffer-match-face-3    ((t :background nil :foreground ,my/color-cyan :underline t)))
     `(ivy-minibuffer-match-face-4    ((t :background nil :foreground ,my/color-cyan :underline t)))
     `(ivy-minibuffer-match-highlight ((t :background ,my/color-gray :foreground nil :underline unspecified :weight unspecified)))
     `(ivy-subdir                     ((t :background nil :underline unspecified :weight unspecified))))

    (custom-theme-set-faces
     'zenburn
     ;; Removes the annoying secondary color in the buffer divider --
     ;; called fringe.
     `(fringe ((t (:background "#3F3F3F"))))))
:init
  ;; Determines the style used by `doom-modeline-buffer-file-name'.
  ;;
  ;; Given ~/Projects/FOSS/emacs/lisp/comint.el
  ;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
  ;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
  ;;   truncate-with-project => emacs/l/comint.el
  ;;   truncate-except-project => ~/P/F/emacs/l/comint.el
  ;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
  ;;   truncate-all => ~/P/F/e/l/comint.el
  ;;   relative-from-project => emacs/lisp/comint.el
  ;;   relative-to-project => lisp/comint.el
  ;;   file-name => comint.el
  ;;   buffer-name => comint.el<2> (uniquify buffer name)
  ;;
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)

  ;; Whether show `all-the-icons' or not (when nil nothing will be
  ;; showed).
  (setq doom-modeline-icon nil)

  ;; How tall the mode-line should be (only respected in GUI Emacs).
  (setq doom-modeline-height 25)

  ;; How wide the mode-line bar should be (only respected in GUI
  ;; Emacs).
  (setq doom-modeline-bar-width 1)

  ;; Whether display minor modes or not. Non-nil to display in
  ;; mode-line.
  (setq doom-modeline-minor-modes nil)

  ;; If non-nil, the mode-line is displayed with the `variable-pitch'
  ;; face.
  (setq doom-modeline-enable-variable-pitch nil)

  :config
  (doom-modeline-mode 1))

;; The original font height (so it can be restored too at a later time)
(setq deos/original-height 180)

(defun deos/setup-fonts ()
(when *is-gui*
;; default font and variable-pitch fonts
(set-face-attribute 'default nil
                    :family "Iosevka"
                    :height deos/original-height)
    (dolist (face '(mode-line mode-line-inactive minibuffer-prompt))
      (set-face-attribute face nil :family "Iosevka"
                          :height deos/original-height))
    (set-face-attribute 'variable-pitch nil
                        :family "DejaVu Sans" :height deos/original-height)
    ;; font for all unicode characters
    ;;(set-fontset-font t 'unicode "DejaVu Sans Mono" nil 'prepend)
    ))

(when *is-gui*
  (add-hook 'after-init-hook #'deos/setup-fonts))

;; Easily adjust the font size in all Emacs frames.
(use-package default-text-scale
  :when my/gui?
  :ensure t
  :defer t
  :init
  (general-define-key
   "M-=" #'default-text-scale-increase
   "M--" #'default-text-scale-decrease
   "M-0" #'default-text-scale-reset))

(use-package all-the-icons
  :when my/gui?
  :ensure t
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon)

  :config
  ;; IMPORTANT: changing the variables below may require restarting
  ;; Emacs.
  ;; IMPORTANT: if placeholders are being displayed instead of icons
  ;; see https://github.com/domtronn/all-the-icons.el#troubleshooting

  (setq all-the-icons-ivy-rich-icon-size 1.0)

  ;; Icons by file name.
  (add-to-list 'all-the-icons-icon-alist '("\\.conf$" all-the-icons-octicon "settings" :height 1.0 :v-adjust 0.0 :face all-the-icons-dyellow))
  (add-to-list 'all-the-icons-icon-alist '("\\.service$" all-the-icons-octicon "settings" :height 1.0 :v-adjust 0.0 :face all-the-icons-dyellow))
  (add-to-list 'all-the-icons-icon-alist '("^config$" all-the-icons-octicon "settings" :height 1.0 :v-adjust 0.0 :face all-the-icons-dyellow))

  ;; Icons by directory name.
  (add-to-list 'all-the-icons-dir-icon-alist '("emacs" all-the-icons-fileicon "emacs"))
  (add-to-list 'all-the-icons-dir-icon-alist '("emacs\\.d" all-the-icons-fileicon "emacs"))
  (add-to-list 'all-the-icons-dir-icon-alist '("spec" all-the-icons-fileicon "test-dir")))


;; Ivy/counsel integration for `all-the-icons'.
(use-package all-the-icons-ivy
  :when my/gui?
  :ensure t
  :after (ivy counsel-projectile)
  :config
  ;; Adds icons to counsel-projectile-find-file as well.
  (setq all-the-icons-ivy-file-commands '(counsel-projectile-find-file))

  (all-the-icons-ivy-setup))


;; Displays icons for all buffers in Ivy.
(use-package all-the-icons-ivy-rich
  :when my/gui?
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))


;; Adds dired support to all-the-icons.
(use-package all-the-icons-dired
  :when my/gui?
  :ensure t
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

(defvar deos/height-modifier 15
  "Default value to increment the size by when jacking into a monitor.")

(defun deos/monitor-jack-in ()
  "Increase the font size by `deos/height-modifier' amount, for
when you jack into an external monitor."
  (interactive)
  (dolist (face '(default
                   mode-line
                   mode-line-inactive
                   minibuffer-prompt
                   variable-pitch))
    (set-face-attribute face nil :height (+ (face-attribute face :height)
                                            deos/height-modifier))))

(defun deos/monitor-jack-out ()
  "Decreas the font size by `deos/height-modifier' amount, for
when you jack out of an external monitor."
  (interactive)
  (dolist (face '(default
                   mode-line
                   mode-line-inactive
                   minibuffer-prompt
                   variable-pitch))
    (set-face-attribute face nil :height (- (face-attribute face :height)
                                            deos/height-modifier))))

(defun deos/monitor-reset ()
  "Go back to the default font size and `line-spacing'"
  (interactive)
  (dolist (face '(default
                   mode-line
                   mode-line-inactive
                   minibuffer-prompt
                   variable-pitch))
    (set-face-attribute face nil :height deos/original-height))
  (text-scale-adjust 0)
  (when (fboundp 'minimap-mode)
    (condition-case err
        (minimap-mode 0)
      ('error 0)))
  (setq line-spacing 0))

(defun deos/code-reading-mode ()
  "Do a bunch of fancy stuff to make reading/browsing code
easier. When you're done, `deos/monitor-jack-out' is a great way
to go back to a normal setup."
  (interactive)
  (delete-other-windows)
  (text-scale-increase 1)
  (setq line-spacing 5)
  (use-package minimap :ensure t)
  (when (not minimap-mode)
    (minimap-mode 1)))

(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(NOTE\\|FIXME\\|TODO\\|BUG\\|HACK\\|REFACTOR\\|THE HORROR\\)" 1 font-lock-warning-face t)))))

(global-set-key (kbd "C-c r") #'revert-buffer)

;; ==== Window switching ====
(defun deos/other-window-backwards ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "M-'") #'other-window)
(global-set-key (kbd "M-\"") #'deos/other-window-backwards)
(global-set-key (kbd "H-'") #'other-window)
(global-set-key (kbd "H-\"") #'deos/other-window-backwards)
(global-set-key (kbd "<C-tab>") #'other-window)
(global-set-key (kbd "C-x C-o") #'other-window)

(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(global-set-key (kbd "C-x 4 t") 'transpose-buffers)

(global-set-key (kbd "C-x C-l") 'toggle-truncate-lines)

;; join line to next line
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

;; Start or switch to eshell
(global-set-key (kbd "C-x C-m") 'eshell)

;; If you want to be able to M-x without meta (phones, etc)
(global-set-key (kbd "C-c C-x") 'execute-extended-command)

(use-package hl-anything
  :ensure t
  :diminish hl-highlight-mode
  :commands hl-highlight-mode
  :init
  (global-set-key (kbd "<f7> <f7>") 'hl-highlight-thingatpt-local)
  (global-set-key (kbd "<f7> u") 'hl-unhighlight-all-local)
  (global-set-key (kbd "<f7> U") 'hl-unhighlight-all-global)
  (global-set-key (kbd "<f7> n") 'hl-find-next-thing)
  (global-set-key (kbd "<f7> p") 'hl-find-prev-thing))

; Use regex searches by default.
;;(global-set-key (kbd "C-s") 'isearch-forward-regexp)
;;(global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; Case-fold regex by default
(setq search-default-mode 'character-fold-to-regexp)
;; Non regex search gets the meta also
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(defun deos/add-watchword (string)
  "Highlight whatever `string' is in the current buffer
permanently."
  (font-lock-add-keywords
   nil `((,(if isearch-regexp isearch-string (regexp-quote isearch-string))
          1 '((:background "yellow") (:weight bold)) t))))

(define-key isearch-mode-map (kbd "M-h")
  (lambda () (interactive)
    (deos/add-watchword
     (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

;; mouse integration
(require 'mouse)
(xterm-mouse-mode t)
(global-set-key [mouse-4] '(lambda ()
                           (interactive)
                           (scroll-down 1)))
(global-set-key [mouse-5] '(lambda ()
                           (interactive)
                           (scroll-up 1)))
(setq mouse-sel-mode t)
(defun track-mouse (e))

(use-package move-text
  :ensure t
  :init (move-text-default-bindings))

(use-package origami
  :ensure t
  :bind (:map origami-mode-map
              ("C-c f" . origami-recursively-toggle-node)
              ("C-c F" . origami-toggle-all-nodes)))

(when *is-a-mac*
    (use-package grab-mac-link
      :ensure t))

  (use-package org-cliplink
    :ensure t)

  (define-key global-map (kbd "C-c l") 'org-store-link)
  (define-key global-map (kbd "C-c a") 'org-agenda)

  (defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
    "A keymap for handy global access to org helpers, particularly clocking.")

  (define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-jump-to-current-clock)
  (define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
  (define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
  (define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)
  (define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map)


  ;; Various preferences
  (setq org-log-done t
        org-edit-timestamp-down-means-later t
        org-hide-emphasis-markers t
        org-catch-invisible-edits 'show
        org-export-coding-system 'utf-8
        org-fast-tag-selection-single-key 'expert
        org-html-validation-link nil
        org-export-kill-product-buffer-when-displayed t
        org-tags-column 80)


  ;; Lots of stuff from http://doc.norang.ca/org-mode.html

  ;; Re-align tags when window shape changes
  (with-eval-after-load 'org-agenda
    (add-hook 'org-agenda-mode-hook
              (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))

  ;; Disables auto indentation in BEGIN blocks. Let me handle it.
  (add-hook 'org-mode-hook (lambda () (electric-indent-mode -1)))

;; Supplemental evil-mode key-bindings to org-mode.
(use-package evil-org
  :ensure t

  :after
  (org evil)

  :commands
  (evil-org evil-org-agenda)

  :init
  (add-hook 'org-mode-hook 'evil-org-mode))

(use-package writeroom-mode
    :ensure t)

  (define-minor-mode prose-mode
    "Set up a buffer for prose editing.
  This enables or modifies a number of settings so that the
  experience of editing prose is a little more like that of a
  typical word processor."
    nil " Prose" nil
    (if prose-mode
        (progn
          (when (fboundp 'writeroom-mode)
            (writeroom-mode 1))
          (setq truncate-lines nil)
          (setq word-wrap t)
          (setq cursor-type 'bar)
          (when (eq major-mode 'org)
            (kill-local-variable 'buffer-face-mode-face))
          (buffer-face-mode 1)
          ;;(delete-selection-mode 1)
          (setq-local blink-cursor-interval 0.6)
          (setq-local show-trailing-whitespace nil)
          (setq-local line-spacing 0.2)
          (setq-local electric-pair-mode nil)
          (ignore-errors (flyspell-mode 1))
          (visual-line-mode 1))
      (kill-local-variable 'truncate-lines)
      (kill-local-variable 'word-wrap)
      (kill-local-variable 'cursor-type)
      (kill-local-variable 'blink-cursor-interval)
      (kill-local-variable 'show-trailing-whitespace)
      (kill-local-variable 'line-spacing)
      (kill-local-variable 'electric-pair-mode)
      (buffer-face-mode -1)
      ;; (delete-selection-mode -1)
      (flyspell-mode -1)
      (visual-line-mode -1)
      (when (fboundp 'writeroom-mode)
        (writeroom-mode 0))))

  ;;(add-hook 'org-mode-hook 'buffer-face-mode)


  (setq org-support-shift-select t)
  
  ;;; Capturing

  (global-set-key (kbd "C-c c") 'org-capture)

  (setq org-capture-templates
        `(("t" "todo" entry (file "")  ; "" => `org-default-notes-file'
           "* NEXT %?\n%U\n" :clock-resume t)
          ("n" "note" entry (file "")
           "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
          ))


  
  ;;; Refiling

  (setq org-refile-use-cache nil)

  ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

  (with-eval-after-load 'org-agenda
    (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

  (advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

  ;; Exclude DONE state tasks from refile targets
  (defun sanityinc/verify-refile-target ()
    "Exclude todo keywords with a done state from refile targets."
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

  (defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
    "A version of `org-refile' which allows refiling to any subtree."
    (interactive "P")
    (let ((org-refile-target-verify-function))
      (org-refile goto default-buffer rfloc msg)))

  (defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
    "A version of `org-agenda-refile' which allows refiling to any subtree."
    (interactive "P")
    (let ((org-refile-target-verify-function))
      (org-agenda-refile goto rfloc no-update)))

  ;; Targets start with the file name - allows creating level 1 tasks
  ;;(setq org-refile-use-outline-path (quote file))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)

  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  
  ;;; To-do settings

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
                (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
        org-todo-repeat-to-state "NEXT")

  (setq org-todo-keyword-faces
        (quote (("NEXT" :inherit warning)
                ("PROJECT" :inherit font-lock-string-face))))


  
  ;;; Agenda views

  (setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


  (let ((active-project-match "-INBOX/PROJECT"))

    (setq org-stuck-projects
          `(,active-project-match ("NEXT")))

    (setq org-agenda-compact-blocks t
          org-agenda-sticky t
          org-agenda-start-on-weekday nil
          org-agenda-span 'day
          org-agenda-include-diary nil
          org-agenda-sorting-strategy
          '((agenda habit-down time-up user-defined-up effort-up category-keep)
            (todo category-up effort-up)
            (tags category-up effort-up)
            (search category-up))
          org-agenda-window-setup 'current-window
          org-agenda-custom-commands
          `(("N" "Notes" tags "NOTE"
             ((org-agenda-overriding-header "Notes")
              (org-tags-match-list-sublevels t)))
            ("g" "GTD"
             ((agenda "" nil)
              (tags "INBOX"
                    ((org-agenda-overriding-header "Inbox")
                     (org-tags-match-list-sublevels nil)))
              (stuck ""
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-todo-ignore-scheduled 'future)))
              (tags-todo "-INBOX"
                         ((org-agenda-overriding-header "Next Actions")
                          (org-agenda-tags-todo-honor-ignore-options t)
                          (org-agenda-todo-ignore-scheduled 'future)
                          (org-agenda-skip-function
                           '(lambda ()
                              (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                  (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                          (org-tags-match-list-sublevels t)
                          (org-agenda-sorting-strategy
                           '(todo-state-down effort-up category-keep))))
              (tags-todo ,active-project-match
                         ((org-agenda-overriding-header "Projects")
                          (org-tags-match-list-sublevels t)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "-INBOX/-NEXT"
                         ((org-agenda-overriding-header "Orphaned Tasks")
                          (org-agenda-tags-todo-honor-ignore-options t)
                          (org-agenda-todo-ignore-scheduled 'future)
                          (org-agenda-skip-function
                           '(lambda ()
                              (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                  (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                          (org-tags-match-list-sublevels t)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "/WAITING"
                         ((org-agenda-overriding-header "Waiting")
                          (org-agenda-tags-todo-honor-ignore-options t)
                          (org-agenda-todo-ignore-scheduled 'future)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "/DELEGATED"
                         ((org-agenda-overriding-header "Delegated")
                          (org-agenda-tags-todo-honor-ignore-options t)
                          (org-agenda-todo-ignore-scheduled 'future)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              (tags-todo "-INBOX"
                         ((org-agenda-overriding-header "On Hold")
                          (org-agenda-skip-function
                           '(lambda ()
                              (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                  (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                          (org-tags-match-list-sublevels nil)
                          (org-agenda-sorting-strategy
                           '(category-keep))))
              ;; (tags-todo "-NEXT"
              ;;            ((org-agenda-overriding-header "All other TODOs")
              ;;             (org-match-list-sublevels t)))
              )))))


  (add-hook 'org-agenda-mode-hook 'hl-line-mode)

  
  ;;; Org clock

  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (with-eval-after-load 'org
    (org-clock-persistence-insinuate))
  (setq org-clock-persist t)
  (setq org-clock-in-resume t)

  ;; Save clock data and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Save state changes in the LOGBOOK drawer
  (setq org-log-into-drawer t)
  ;; Removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; Show clock sums as hours and minutes, not "n days" etc.
  (setq org-time-clocksum-format
        '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))


  
  ;;; Show the clocked-in task - if any - in the header line
  (defun sanityinc/show-org-clock-in-header-line ()
    (setq-default header-line-format '((" " org-mode-line-string " "))))

  (defun sanityinc/hide-org-clock-from-header-line ()
    (setq-default header-line-format nil))

  (add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
  (add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
  (add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

  (with-eval-after-load 'org-clock
    (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
    (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))


  
  (when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
    (add-hook 'org-clock-in-hook
              (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                       (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
    (add-hook 'org-clock-out-hook
              (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                       "tell application \"org-clock-statusbar\" to clock out"))))


  
  ;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
  ;; TODO: nested projects!


  
  ;;; Archiving

  (setq org-archive-mark-done nil)
  (setq org-archive-location "%s_archive::* Archive")



  

  (use-package org-pomodoro
    :ensure t)
  (setq org-pomodoro-keep-killed-pomodoro-time t)
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


  ;; ;; Show iCal calendars in the org agenda
  ;; (when (and *is-a-mac* (require 'org-mac-iCal nil t))
  ;;   (setq org-agenda-include-diary t
  ;;         org-agenda-custom-commands
  ;;         '(("I" "Import diary from iCal" agenda ""
  ;;            ((org-agenda-mode-hook #'org-mac-iCal)))))

  ;;   (add-hook 'org-agenda-cleanup-fancy-diary-hook
  ;;             (lambda ()
  ;;               (goto-char (point-min))
  ;;               (save-excursion
  ;;                 (while (re-search-forward "^[a-z]" nil t)
  ;;                   (goto-char (match-beginning 0))
  ;;                   (insert "0:00-24:00 ")))
  ;;               (while (re-search-forward "^ [a-z]" nil t)
  ;;                 (goto-char (match-beginning 0))
  ;;                 (save-excursion
  ;;                   (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
  ;;                 (insert (match-string 0))))))


  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
    (when *is-a-mac*
      (define-key org-mode-map (kbd "M-h") nil)
      (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link)))

  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     `((R . t)
       (ditaa . t)
       (dot . t)
       (emacs-lisp . t)
       (gnuplot . t)
       (haskell . nil)
       (latex . t)
       (ledger . t)
       (ocaml . nil)
       (octave . t)
       (plantuml . t)
       (python . t)
       (ruby . t)
       (screen . nil)
       (,(if (locate-library "ob-sh") 'sh 'shell) . t)
       (sql . t)
       (sqlite . t))))

(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Since we don't want to disable org-confirm-babel-evaluate all
;; of the time, do it around the after-save-hook
(defun dn/org-babel-tangle-dont-ask ()
  ;; Dynamic scoping to the rescue
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dn/org-babel-tangle-dont-ask
                                              'run-at-end 'only-in-org-mode)))

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

(use-package org-make-toc
  :ensure t
  :hook (org-mode . org-make-toc-mode))

(use-package deft
  :ensure t
  :bind ("C-x t" . deft)
  :config
  (setq deft-extension '("org" "txt" "tex" "text" "md")
        deft-directory "~/personal/org/deft/"
        deft-recursive t
        deft-use-filter-string-for-filename t
        deft-text-mode 'org-mode))

;; NOTE: It require https://github.com/sharkdp/fd
(use-package projectile
  :ensure t

  ;; Defer because it'll be loaded by counsel-projectile.
  :defer t

  :init
  (defun my/copy-file-relative-name-to-clipboard ()
    "Copy current buffer relative file name to the clipboard."
    (interactive)
    (kill-new (file-relative-name buffer-file-name (projectile-project-root))))

  (setq projectile-cache-file (concat my/cache-dir "projectile.cache")
        projectile-enable-caching nil
        projectile-ignored-projects '("~/" "/tmp")
        projectile-known-projects-file (concat my/cache-dir "projectile-bookmarks.eld")
        ;; Enable Projectile in every directory (even without the presence
        ;; of project file). This works well with fd, given how much faster
        ;; it is compared to find.
        projectile-require-project-root t
        projectile-completion-system 'ivy)

  (global-set-key [remap evil-jump-to-tag] #'projectile-find-tag)
  (global-set-key [remap find-tag]         #'projectile-find-tag)

  ;; It's recommended to use fd as a replacement for both git ls-files
  ;; and find.
  (setq projectile-generic-command "fd . --color=never --type f -0 -H -E .git"
        projectile-git-command projectile-generic-command)

  ;; Skip warnings about unsafe variables in .dir-locals.el
  (put 'projectile-project-type 'safe-local-variable #'symbolp)

  ;; Always open the top-level project directory after switching projects.
  (setq projectile-switch-project-action #'projectile-dired)

  :config
  (general-define-key
   :prefix my/leader
   :states 'normal
   :keymaps 'prog-mode-map
   "jA" #'projectile-find-implementation-or-test-other-window
   "ja" #'projectile-toggle-between-implementation-and-test))

(use-package counsel
    :ensure t
    :defer t

    :init
    (setq counsel-describe-function-function #'helpful-callable
          counsel-describe-variable-function #'helpful-variable)

    ;; Use custom configurations, but most importantly, pipe the filtering
    ;; from the "fdfind" output. See the `dotfiles/shell/vars' file for
    ;; more details
    (setq counsel-fzf-cmd
          (concat (getenv "FZF_CTRL_T_COMMAND") " | " "fzf -f \"%s\""))

    (general-define-key
     [remap bookmark-jump] #'counsel-bookmark
     [remap describe-variable] #'counsel-describe-variable
     [remap find-file] #'counsel-find-file
     [remap org-set-tags-command] #'counsel-org-tag
     [remap execute-extended-command] #'counsel-M-x)

    (general-define-key
     :prefix my/leader
     :states 'normal
     :keymaps 'override
     "f" #'counsel-projectile-find-file))

  ;; Ivy for searching and matching
  (use-package ivy
    :ensure t
    :defer t
    :hook (emacs-startup . ivy-mode)
    :init
    ;; Avoid using fuzzy searches everywhere. For example, cousel-rg
    ;; with fuzzy enabled brings a lot of useless results.
    ;; Remember you can switch modes in the ivy minibuffer with <C-o S-m>
    (setq ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
                                  (t . ivy--regex-plus)))

    ;; Do not display the total number of candidates.
    (setq ivy-count-format "")

    ;; Only show the current directory.
    (setq ivy-extra-directories '("./"))

    ;; Do not close the minibuffer when there's no text left to delete.
    (setq ivy-on-del-error-function #'ignore)

    ;; Enable bookmarks and recentf
    (setq ivy-use-virtual-buffers t)

    (setq ivy-initial-inputs-alist nil)

    (general-define-key
     [remap switch-to-buffer] #'ivy-switch-buffer
     [remap list-buffers] #'ivy-switch-buffer
     [remap switch-to-buffer-other-window] #'ivy-switch-buffer-other-window))

  (use-package amx
    :ensure t
    :init
    (setq amx-save-file (concat my/cache-dir "amx")
          amx-history-length 10))

  ;; Gives an overview of the current regex search candidates.
  (use-package swiper
    :ensure t
    :defer t
    :init
    (setq swiper-action-recenter t)
    (general-define-key
     [remap switch-to-buffer] #'ivy-switch-buffer
     [remap list-buffers] #'ivy-switch-buffer
     [remap switch-to-buffer-other-window] #'ivy-switch-buffer-other-window
     [remap bookmark-jump] #'counsel-bookmark
     [remap describe-variable] #'counsel-describe-variable
     [remap describe-function] #'counsel-describe-function
     [remap find-file] #'counsel-find-file
     [remap execute-extended-command] #'counsel-M-x)

    (general-define-key
     :prefix my/leader
     :states 'normal
     :keymaps 'override
     "/" #'swiper))

;; Ivy UI for Projectile.
(use-package counsel-projectile
  :ensure t

  :defer 0

  :config
  ;; Counsel-Projectile mode turns on Projectile mode, thus enabling all
  ;; projectile key bindings, and adds the counsel-projectile key bindings on
  ;; top of them.
  (counsel-projectile-mode +1)

  ;; Always open the top-level project directory after switching projects.
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((default counsel-projectile-switch-project-action-dired))))
;; More friendly interface for ivy (discoverability).
(use-package ivy-rich
  :ensure t
  :defer t
  :config
  ;; These configurations were adapted from the README:
  ;; https://github.com/Yevgnen/ivy-rich
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate
             (:width 30))
            ((lambda (candidate)
               (file-name-directory (ivy-rich-switch-buffer-path candidate)))
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate (lambda (candidate)
                        (get-buffer candidate)))

          counsel-M-x
          (:columns
           ((counsel-M-x-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))

          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))

          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer
             (:width 40))
            (ivy-rich-counsel-variable-docstring
             (:face font-lock-doc-face))))))

  :init
  (ivy-rich-mode))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :commands (company-complete-common
             company-manual-begin
             company-grab-line))

(use-package yasnippet
    :ensure t
    :diminish yas-minor-mode
    :config
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
    (yas-global-mode)
    (global-set-key (kbd "M-/") 'company-yasnippet))

(require 'skeleton)

(define-skeleton deos/org-header
  "Insert a standard header for org-mode files"
  "Title: "
  "#+TITLE: " str \n
  "#+AUTHOR: " (user-full-name) \n
  "#+EMAIL: " user-mail-address \n
  "#+SETUPFILE: ~/deos/setupfiles/emacs.setup

| *Author* | {{{author}}} ({{{email}}})    |
| *Date*   | {{{time(%Y-%m-%d %H:%M:%S)}}} |

* Introduction
" \n)

(define-skeleton deos/org-wrap-elisp
  "Wrap text with #+BEGIN_SRC / #+END_SRC for the emacs-lisp code"
  nil
  > "#+BEGIN_SRC emacs-lisp " \n
  > _ \n
  > "#+END_SRC" \n)

  (define-skeleton deos/org-wrap-source
  "Wrap text with #+BEGIN_SRC / #+END_SRC for a code type"
  "Language: "
  > "#+BEGIN_SRC " str "" \n
  > _ \n
  > "#+END_SRC" \n)

  (define-skeleton deos/es-make-index
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

  (define-skeleton deos/java-try-catch
  "Wrap code in a Java try/catch"
  nil
  > "try {" \n
  > _
  > "} catch (Exception e) {" \n
  > "throw e;" \n
  > "}" \n)

(defhydra deos/hydra-skeleton nil
  "Insert Skeleton"
  ("e" deos/org-wrap-elisp "Wrap as elisp" :exit t)
  ("s" deos/org-wrap-source "Wrap as source" :exit t)
  ("i" deos/es-make-index "ES Index" :exit t)
  ("h" deos/org-header "Org Header" :exit t)
  ("t" deos/java-try-catch "Wrap with try/catch" :exit t))

(use-package markdown-mode
  :ensure t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("github\\.com.*\\.txt\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh"))
  (add-hook 'markdown-mode-hook #'flyspell-mode))

;; Comment/uncomment lines efficiently. Like Nerd Commenter in Vim.
(use-package evil-nerd-commenter
  :ensure t

  :after (evil)

  :commands (evilnc-comment-or-uncomment-lines)

  :init
  ;; Improved toggle comment/uncomment lines.
  (general-define-key
    "M-;" #'evilnc-comment-or-uncomment-lines))

;; Emulates Surround.vim for Evil. Everything about "surroundings":
;; parentheses, brackets, quotes, XML tags, and more.
(use-package evil-surround
  :ensure t

  :after (evil)

  :defer t

  :init
  (add-hook 'prog-mode-hook #'global-evil-surround-mode)

  :config
  (add-to-list 'evil-surround-operator-alist '(evil-cp-change . change))
  (add-to-list 'evil-surround-operator-alist '(evil-cp-delete . delete)))

(use-package adoc-mode
  :ensure t)

(use-package synosaurus
  :ensure t
  :init
  (setq-default synosaurus-backend 'synosaurus-backend-wordnet)
  (add-hook 'after-init-hook #'synosaurus-mode))

(use-package fill-column-indicator
  :ensure t
  :config
  ;; fix for org -> html export
  (defun fci-mode-override-advice (&rest args))
  (use-package org)
  (advice-add 'org-html-fontify-code :around
              (lambda (fun &rest args)
                (advice-add 'fci-mode :override #'fci-mode-override-advice)
                (let ((result (apply fun args)))
                  (advice-remove 'fci-mode #'fci-mode-override-advice)
                  result)))
  (defvar deos/fci-disabled nil)
  (make-variable-buffer-local 'deos/fci-disabled)
   ;; Add a hook that disables fci if enabled when the window changes and it
  ;; isn't wide enough to display it.
  (defun deos/maybe-disable-fci ()
    (interactive)
    ;; Disable FCI if necessary
    (when (and fci-mode
               (< (window-width) (or fci-rule-column fill-column)))
      (fci-mode -1)
      (setq-local deos/fci-disabled t))
    ;; Enable FCI if necessary
    (when (and deos/fci-disabled
               (eq fci-mode nil)
               (> (window-width) (or fci-rule-column fill-column)))
      (fci-mode 1)
      (setq-local deos/fci-disabled nil)))

  (defun deos/add-fci-disabling-hook ()
    (interactive)
    (add-hook 'window-configuration-change-hook
              #'deos/maybe-disable-fci))
  (add-hook 'prog-mode-hook #'deos/add-fci-disabling-hook))

;; A subset, only training lines and whitespace
(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))

(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal.
      '(;;(space-mark 32 [183] [46])
        ;; (newline-mark 10 [172 10]) ;; the paragraph sign
        (newline-mark 10 [172 10]) ;; mathematical "not"
        (tab-mark 9 [187 9] [92 9])))

(defun deos/turn-on-whitespace-mode ()
  (interactive)
  (setq-local whitespace-line-column fill-column)
  (whitespace-mode +1)
  (diminish 'whitespace-mode)
  (whitespace-newline-mode 1)
  (diminish 'whitespace-newline-mode))

(add-hook 'prog-mode-hook #'deos/turn-on-whitespace-mode)

(use-package org
  :config
  (advice-add 'org-html-fontify-code :around
              (lambda (fun &rest args)
                (whitespace-mode -1)
                (let ((result  (apply fun args)))
                  (whitespace-mode +1)
                  result))))

(use-package highlight-indent-guides
  :ensure t
  :init
  (setq highlight-indent-guides-method 'character)
  (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))

(use-package flycheck
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :init
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
      (or (buffer-file-name) default-directory)
        "node_modules"))
        (eslint (and root
          (expand-file-name "node_modules/eslint/bin/eslint.js"
            root))))
        (when (and eslint (file-executable-p eslint))
    (setq-local flycheck-javascript-eslint-executable eslint))))
   :config
   (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
   (add-to-list 'flycheck-checkers 'proselint)
   (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
   (use-package flycheck-color-mode-line
     :ensure t)
     :hook (flycheck-mode . flycheck-color-mode-line-mode))

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))

(global-set-key (kbd "RET") 'newline-and-indent)
(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(add-hook 'after-init-hook 'electric-indent-mode)

;; Utility package for jumping to visible text across windows and frames.
(use-package avy
  :ensure t
  :defer t
  :init
  (general-define-key
   :states 'normal
   :keymaps 'override
   :prefix my/leader
   "aw" #'avy-goto-word-0
   "ac" #'avy-goto-char
   "al" #'avy-goto-line))

(use-package dumb-jump
  :ensure t
  :init
  ;; Use ivy instead of the default popup for multiple options.
  (setq dumb-jump-selector 'ivy)

  ;; When set to rg it will still use git-grep if it's a git project (because
  ;; it's the fastest), but will you use whatever you set here in any
  ;; other situation.
  (setq dumb-jump-prefer-searcher 'rg)

  ;; Adds dumb-jump to xref backend so I can use it with `M-.'.
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(defun deos/add-watchwords ()
  "Highlight FIXME, TODO, and NOCOMMIT in code TODO"
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\(?:(.*)\\)?:?\\)\\>"  1 'warning prepend)
         ("\\<\\(FIXME\\(?:(.*)\\)?:?\\)\\>" 1 'error prepend)
         ("\\<\\(NOCOMMIT\\(?:(.*)\\)?:?\\)\\>"  1 'error prepend))))

(add-hook 'prog-mode-hook #'deos/add-watchwords)

(use-package editorconfig
  :ensure t
  :defer t
  :hook (prog-mode . editorconfig-mode))

(use-package highlight-escape-sequences
  :ensure t
  :hook (after-init . hes-mode))

(use-package highlight-symbol
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode ruby-mode) . highlight-symbol-mode)
  :init
  ;; Reduce default idle delay of 1.5s.
  (setq highlight-symbol-idle-delay 0.5))

(use-package browse-kill-ring
  :ensure t
  :bind (("M-Y" . browse-kill-ring)
         (:map browse-kill-ring-mode-map
               ("C-g" . browse-kill-ring-quit)
               ("M-n" . browse-kill-ring-forward)
               ("M-p" . browse-kill-ring-previous)))
  :init
  (setq browse-kill-ring-separator "\f")
  :config
  (add-to-list 'page-break-lines-modes 'browse-kill-ring-mode))

(use-package whole-line-or-region
  :ensure t
  :diminish t
  :hook (after-init . whole-line-or-region-global-mode))

(add-auto-mode
 'nxml-mode
 (concat "\\."
         (regexp-opt
          '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss"
            "gpx" "tcx" "plist"))
         "\\'"))
(setq magit-mode-alist (cons '("<\\?xml " . nxml-mode) magic-mode-alist))
(fset 'xml-mode 'nxml-mode)
(setq nxml-slash-auto-complete-flag t)

;; See: http://sinewalker.wordpress.com/2008/06/26/pretty-printing-xml-with-emacs-nxml-mode/
(defun sanityinc/pp-xml-region (beg end)
  "Pretty format XML markup in region. The function inserts
linebreaks to separate tags that have nothing but whitespace
between them. It then indents the markup by using nxml's
indentation rules."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  ;; Use markers because our changes will move END
  (setq beg (set-marker (make-marker) beg)
        end (set-marker (make-marker) end))
  (save-excursion
    (goto-char beg)
    (while (search-forward-regexp "\>[ \\t]*\<" end t)
      (backward-char) (insert "\n"))
    (nxml-mode)
    (indent-region beg end)))

;;----------------------------------------------------------------------------
;; Integration with tidy for html + xml
;;----------------------------------------------------------------------------

(defun sanityinc/tidy-buffer-xml (beg end)
  "run \"tidy -xml\" on the region from BEG to END, or whole buffer."
  (interactive "r")
  (unless (use-region-p)
    (setq beg (point-min)
          end (point-max)))
  (shell-command-on-region beg end "tidy -xml -q -i" (current-buffer) t "*tidy-errors*" t))

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook 'sanityinc/set-mode-for-new-scripts)

(defun sanityinc/set-mode-for-new-scripts ()
  "Invoke `normal-mode' if this file is a script and in `fundamental-mode'."
  (and
   (eq major-mode 'fundamental-mode)
   (>= (buffer-size) 2)
   (save-restriction
     (widen)
     (string= "#!" (buffer-substring (point-min) (+ 2 (point-min)))))
   (normal-mode)))

(use-package info-colors
  :ensure t
  :hook (Info-selection . info-colors-fontify-node))

(use-package regex-tool
  :ensure t
  :config
  (setq-default regex-tool-backend 'perl))

(with-eval-after-load 're-builder
  ;; Support a slightly more idiomatic quit binding in re-builder
  (define-key reb-mode-map (kbd "C-c C-k") 'reb-quit))

(use-package dired
  :init
  (defun my/dired-hidden-toggle ()
    "Show/hide dot-files."
    (interactive)
    (when (equal major-mode 'dired-mode)
      (if (or (not (boundp 'my/dired-hidden-show-p))
              my/dired-hidden-show-p)
          ;; If currently showing.
          (progn
            (setq-local my/dired-hidden-show-p nil)
            (dired-mark-files-regexp "^\\\.")
            (dired-do-kill-lines))
        ;; Otherwise just revert to re-show.
        (progn (revert-buffer)
               (setq-local my/dired-hidden-show-p t)))))

  (setq dired-listing-switches
        (string-join '("-l"
                       "--almost-all"
                       "--classify"
                       "--dired"
                       "--group-directories-first"
                       "--human-readable")
                     " "))

  ;; Always copy/delete recursively.
  (setq dired-recursive-copies  'always
        dired-recursive-deletes 'top)

  ;; Where to store image caches.
  (setq image-dired-dir (concat my/cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image"))

  (setq dired-auto-revert-buffer t
        ;; Suggest a target for moving/copying intelligently
        dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil)

  ;; Disable the prompt about whether I want to kill the Dired buffer for a
  ;; deleted directory.
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  ;; Screens are larger nowadays, we can afford slightly larger thumbnails
  (setq image-dired-thumb-size 150)

  (add-hook 'dired-mode-hook #'dired-hide-details-mode)

  :config
  (general-define-key
   [remap dired] #'counsel-dired
   "C-x C-j" #'dired-jump
   "C-x 4 j" #'dired-jump-other-window))

(use-package quick-preview
  :ensure t
  :init
  (global-set-key (kbd "C-c q") 'quick-preview-at-point)
  (define-key dired-mode-map (kbd "Q") 'quick-preview-at-point))

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-c n") 'neotree-toggle)
  (setq neo-theme 'arrow)
  (setq neo-window-fixed-size nil)
  (setq neo-smart-open t)
  (setq neo-window-width 40)
  (setq neo-default-system-application "open"))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (setq-default magit-diff-refine-hunk t)
  (defun sanityinc/magit-or-vc-log-file (&optional prompt)
    (interactive "P")
    (if (and (buffer-file-name)
             (eq 'Git (vc-backend (buffer-file-name))))
        (if prompt
            (magit-log-buffer-file-popup)
          (magit-log-buffer-file t))
      (vc-print-log)))

  (with-eval-after-load 'vc
    (define-key vc-prefix-map (kbd "l") 'sanityinc/magit-or-vc-log-file))

  (with-eval-after-load 'magit
    (define-key magit-status-mode-map (kbd "C-M-<up>") 'magit-section-up))

  (use-package magit-todos
    :ensure t)

  (use-package fullframe
    :ensure t
    :config
    (with-eval-after-load 'magit
      (fullframe magit-status magit-mode-quit-window)))

  (when *is-a-mac*
    (with-eval-after-load 'magit
      (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))

  (with-eval-after-load 'vc
    (define-key vc-prefix-map (kbd "f") 'vc-git-grep))

  (with-eval-after-load 'compile
    (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                        '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
      (add-to-list 'compilation-error-regexp-alist-alist defn)
      (add-to-list 'compilation-error-regexp-alist (car defn))))

  (defvar git-svn--avialable-commands nil "Cached list of git svn subcommands")
  (defun git-svn--available-commands ()
    (or git-svn--available-commands
        (setq git-svn--available-commands
              (sanityinc/string-all-matches
               "^  \\([a-z\\-]+\\) +"
               (shell-command-to-string "git svn help") 1))))
  (autoload 'vc-git-root "vc-git")

  (defun git-svn (dir command)
    "Run a git svn subcommand in DIR."
    (interactive (list (read-directory-name "Directory: ")
                       (completing-read "git-svn command: " (git-svn--available-commands) nil t nil nil (git-svn--available-commands))))
    (let* ((default-directory (vc-git-root dir))
           (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
      (compile (concat "git svn " command))))
  )
;; Evil keybindings for Magit.
(use-package evil-magit
  :ensure t
  :defer t

  :hook (magit-mode . evil-magit-init)

  :init
  (setq evil-magit-state 'normal))

(use-package git-blamed
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package gitconfig-mode
  :ensure t)

(use-package git-timemachine
  :ensure t
  :bind (("C-x v t" . git-timemachine-toggle))
  )
(use-package git-commit
  :ensure t
  :hook (git-commit-mode . goto-address-mode))

(use-package yagist
  :ensure t)
(use-package bug-reference-github
  :ensure t
  :hook (prog-mode . bug-reference-prog-mode))

(use-package github-clone
  :ensure t)

(use-package forge
  :ensure t)

(use-package github-review
  :ensure t)

(use-package diff-hl
  :ensure t
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (after-init . global-diff-hl-mode))
  :config
  (with-eval-after-load 'diff-hl
    (define-key diff-hl-mode-map
      (kbd "<left-fringe> <mouse-1>")
      'diff-hl-diff-goto-hunk)))

(use-package browse-at-remote
  :ensure t)

(use-package python
  :config
  (setq-default python-indent-offset 2)
  (define-key python-mode-map (kbd "<backtab>") 'python-back-indent))

(use-package virtualenvwrapper
  :ensure t
  :defer t
  :init
  (progn
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)
    (setq venv-location (or (getenv "WORKON_HOME")
                            "~/.virtualenvs"))))
;; this requires some setup pip install jedi flake8 autopep8 yapf
;; (use-package elpy
;;   :ensure t
;;   :defer t
;;   :init
;;   (elpy-enable)
;;   :config
;;   (setq elpy-test-django-with-manage t))

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package kivy-mode
  :ensure t
  :mode ("\\.kv\\'" . kivy-mode))

(use-package irony
  :ensure t
  :hook (c-mode . irony-mode))
(use-package company-irony
  :ensure t
  :config
  (add-to-list 'company-backends 'company-irony))
(use-package flycheck-irony
  :ensure t
  :hook (flycheck-mode . flycheck-irony-setup))

(use-package csharp-mode
    :ensure t)

(use-package omnisharp
  :ensure t
  :hook ((csharp-mode . omnisharp-mode)
         ;; TODO: 'before-save runs globally - make this buffer local?
         (before-save . omnisharp-code-format-entire-file))
  :config
  (add-to-list 'company-backends 'company-omnisharp))

(use-package groovy-mode
  :ensure t
  :config
  (setq groovy-indent-offset 2))

(setq deos/use-meghanada 1)
(setq deos/use-eclim nil)
(setq deos/use-lsp nil)

(use-package shut-up :ensure t)

(defun deos/setup-java ()
  (interactive)
  (define-key java-mode-map (kbd "M-,") 'pop-tag-mark)
  (define-key java-mode-map (kbd "C-c M-i") 'java-imports-add-import-dwim)
  (define-key java-mode-map (kbd "C-c C-b") 'bool-flip-do-flip)
  (c-set-style "google-c-style" t)
  (subword-mode 1)
  (shut-up (toggle-truncate-lines 1))
  (setq-local fci-rule-column 99)
  (setq-local fill-column 100)
  (when (fboundp 'deos/turn-on-whitespace-mode)
    (whitespace-mode -1)
    (deos/turn-on-whitespace-mode)))
(add-hook 'java-mode-hook #'deos/setup-java)
(require 'compile)
(add-to-list 'compilation-error-regexp-alist
             '("^:[^/.\n]+\\(/.+\\):\\([[:digit:]]+\\):" 1 2))

(use-package smart-backspace
  :ensure t
  :bind ("<C-M-backspace>" . smart-backspace))

(use-package java-imports
  :ensure t
  :config
  (setq java-imports-find-block-function 'java-imports-find-place-sorted-block)
  (add-hook 'java-mode-hook 'java-imports-scan-file))

(use-package eclim
  :ensure t
  :disabled t
  :init
  ;; only show errors
  (setq-default eclim--problems-filter "e")
  (when deos/use-eclim
    (add-hook 'java-mode-hook #'eclim-mode)
    (use-package company-emacs-eclim
      :ensure t
      :init (company-emacs-eclim-setup))))

(add-to-list 'tramp-remote-path "/home/dnewman/.sdkman/candiates/gradle/current/bin")
(add-to-list 'tramp-remote-path "/home/dnewman/.sdkman/candiates/groovy/current/bin")

(defun deos/setup-helm-gtags ()
  (interactive)
  ;; this variables must be set before load helm-gtags
  ;; you can change to any prefix key of your choice
  (setq helm-gtags-prefix-key "\C-cg")
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-suggested-key-mapping t)
  (use-package helm-gtags
    :ensure t
    ;; not needed because of smart-jump
    :init (helm-gtags-mode t)
    :diminish ""
    :bind (:map helm-gtags-mode-map
                ("M-S" . helm-gtags-select)
                ("M-." . helm-gtags-dwim)
                ("M-," . helm-gtags-pop-stack)
                ("C-c <" . helm-gtags-previous-history)
                ("C-c >" . helm-gtags-next-history)))
  (helm-gtags-mode t))

(defun deos/setup-ggtags ()
  (interactive)
  (ggtags-mode 1)
  ;; turn on eldoc with ggtags
  (setq-local eldoc-documentation-function #'ggtags-eldoc-function)
  ;; add ggtags to the hippie completion
  (setq-local hippie-expand-try-functions-list
              (cons 'ggtags-try-complete-tag
                    hippie-expand-try-functions-list))
  ;; use helm for completion
  (setq ggtags-completing-read-function nil))

(use-package ggtags
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'c-mode-common-hook
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
                  (deos/setup-semantic-mode)
                  ;; helm-gtags
                  (deos/setup-helm-gtags)
                  ;; regular gtags
                  ;;(my/setup-ggtags)
                  )))))

(use-package realgud
  :ensure t)

(use-package meghanada
  :ensure t
  :init
  ;; Don't auto-start
  (setq meghanada-auto-start nil)
  (when deos/use-meghanada
    (add-hook 'java-mode-hook #'meghanada-mode)
    (add-hook 'java-mode-hook 'flycheck-mode)
    (bind-key "C-c M-." 'meghanada-jump-declaration java-mode-map)))

(when (file-exists-p "~/.emacs.d/eclipse.jdt.ls")
  (use-package lsp-mode
    :ensure t
    :init
    (when deos/use-lsp
      (use-package lsp-java
        :ensure t
        :init
        (require 'lsp-java)
        (require 'lsp-mode)
        (add-hook 'java-mode-hook #'lsp-java-enable)))
    :config
    (use-package lsp-ui
      :ensure t)))

(use-package js2-mode :ensure t)
  (use-package typescript-mode :ensure t)
  (use-package prettier-js :ensure t)

  ;; Need to first remove from list if present, since elpa adds entries too, which
  ;; may be in an arbitrary order

  (add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js-mode))

  ;; js2-mode

  ;; Change some defaults: customize them to override
  (setq-default js2-bounce-indent-p nil)
  (with-eval-after-load 'js2-mode
    ;; Disable js2 mode's syntax error highlighting by default...
    (setq-default js2-mode-show-parse-errors nil
                  js2-mode-show-strict-warnings nil)
    ;; ... but enable it if flycheck can't handle javascript
    (autoload 'flycheck-get-checker-for-buffer "flycheck")
    (defun sanityinc/enable-js2-checks-if-flycheck-inactive ()
      (unless (flycheck-get-checker-for-buffer)
        (setq-local js2-mode-show-parse-errors t)
        (setq-local js2-mode-show-strict-warnings t)
        (when (derived-mode-p 'js-mode)
          (js2-minor-mode 1))))
    (add-hook 'js-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive)
    (add-hook 'js2-mode-hook 'sanityinc/enable-js2-checks-if-flycheck-inactive)

    (js2-imenu-extras-setup))

  (setq-default js-indent-level 2)
  ;; In Emacs >= 25, the following is an alias for js-indent-level anyway
  (setq-default js2-basic-offset 2)


  (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))

  (with-eval-after-load 'js2-mode
    (sanityinc/major-mode-lighter 'js2-mode "JS2")
    (sanityinc/major-mode-lighter 'js2-jsx-mode "JSX2"))
  (with-eval-after-load 'js
    (sanityinc/major-mode-lighter 'js-mode "JS")
    (sanityinc/major-mode-lighter 'js-jsx-mode "JSX"))



  (when (and (or (executable-find "rg") (executable-find "ag"))
             (use-package xref-js2
:ensure t))
    (when (executable-find "rg")
      (setq-default xref-js2-search-program 'rg))
    (defun sanityinc/enable-xref-js2 ()
      (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
    (with-eval-after-load 'js
      (define-key js-mode-map (kbd "M-.") nil)
      (add-hook 'js-mode-hook 'sanityinc/enable-xref-js2))
    (with-eval-after-load 'js2-mode
      (define-key js2-mode-map (kbd "M-.") nil)
      (add-hook 'js2-mode-hook 'sanityinc/enable-xref-js2)))

  ;; ---------------------------------------------------------------------------
  ;; Run and interact with an inferior JS via js-comint.el
  ;; ---------------------------------------------------------------------------

  (when (use-package js-comint :ensure t)
    (setq js-comint-program-command "node")

    (defvar inferior-js-minor-mode-map (make-sparse-keymap))
    (define-key inferior-js-minor-mode-map "\C-x\C-e" 'js-send-last-sexp)
    (define-key inferior-js-minor-mode-map "\C-cb" 'js-send-buffer)

    (define-minor-mode inferior-js-keys-mode
      "Bindings for communicating with an inferior js interpreter."
      nil " InfJS" inferior-js-minor-mode-map)

    (dolist (hook '(js2-mode-hook js-mode-hook))
      (add-hook hook 'inferior-js-keys-mode))
    (setq inferior-js-program-command "node")
    (setq inferir-js-mode-hook
          (lambda ()
            ;; We lke nice colors
            (ansi-color-for-comint-mode-on)
            (add-to-list 'comint-preoutput-filter-functions
                         (lambda (output)
                           (replace-regexp-in-string ".*1G\.\.\..*5G" "..."
                                                     (replace-regexp-in-string ".*1G.*3G" " > " output))))))
    (global-set-key (kbd "M-g n") 'run-js)
    (global-set-key (kbd "M-g m") 'js-send-region))

  ;; ---------------------------------------------------------------------------
  ;; Alternatively, use skewer-mode
  ;; ---------------------------------------------------------------------------

  (when (use-package skewer-mode
:ensure t)
    (with-eval-after-load 'skewer-mode
      (add-hook 'skewer-mode-hook
                (lambda () (inferior-js-keys-mode -1)))))

  (when (use-package add-node-modules-path
:ensure t)
    (dolist (mode '(typescript-mode js-mode js2-mode coffee-mode))
      (add-hook (derived-mode-hook-name mode) 'add-node-modules-path)))

(use-package emacs-lisp-mode
  :no-require t

  :config
  (defun my/emacs-lisp-mode-hook ()
    (setq prettify-symbols-alist '(("lambda" . ?λ)))
    (prettify-symbols-mode 1))

  (defun my/elisp-run-file-tests ()
    "Run all tests in the current buffer."
    (interactive)
    (let* ((base-name (file-name-base buffer-file-name))
            (prefix (progn
                      (string-match "^\\(.+-\\)test$" base-name)
                      (match-string 1 base-name))))
      (ert (concat "^" prefix))))

  (defun my/elisp-run-project-tests ()
    "Run all tests prefixed with the current project name."
    (interactive)
    (let* ((root-path (projectile-project-root))
            (dir-name (file-name-nondirectory (directory-file-name root-path))))
      (ert (format "^%s-" dir-name))))

  (defun my/pp-eval-defun-as-json-other-window ()
    "Pretty-print eval'ed JSON string in another buffer."
    (interactive)
    (let ((result (let ((inhibit-message t))
                    (elisp--eval-defun))))
      (with-current-buffer
        (switch-to-buffer-other-window "*Pretty-print JSON*")
        (read-only-mode -1)
        (erase-buffer)
        (insert result)
        (json-mode)
        (call-interactively #'json-pretty-print-buffer)
        (read-only-mode +1))))

  (add-hook 'emacs-lisp-mode-hook #'my/emacs-lisp-mode-hook)

  (general-define-key
    :prefix my/leader
    :states 'normal
    :keymaps 'emacs-lisp-mode-map
    "e" #'eval-defun
    "E" #'eval-last-sexp
    "P" #'pp-eval-last-sexp
    "tf" #'my/elisp-run-file-tests
    "tp" #'my/elisp-run-project-tests))

(use-package dockerfile-mode
  :ensure t
  ;; Any file starting with "Dockerfile" should enable this mode.
  :mode (("^Dockerfile" . dockerfile-mode)))

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package csv-mode
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :defer t)

(use-package graphql-mode
  :ensure t
  :defer t)

(use-package plantuml-mode
  :ensure t
  :mode ("\\.puml\\'" . plantuml-mode)
  :init
  (setq plantuml-executable-path "~/.local/bin/plantuml")
  (setq plantuml-default-exec-mode 'executable)

  ;; org-babel uses the jar path instead of the executable.
  (setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar"))

(use-package web-mode
  :ensure t
  :mode "\\.\\(?:\\(?:h\\(?:bs\\|tml\\)\\|liquid\\|tmpl\\)\\)\\'"
  :init
  (setq web-mode-markup-indent-offset 2)

  :config
  (general-define-key
   :keymaps 'evil-normal-state-map
   [remap evil-toggle-fold] #'web-mode-fold-or-unfold))

(use-package tide
  :ensure t
  :hook (typescript-mode . tide-setup)
  :mode ("\\.\\(ts\\|tsx\\)\\'" . typescript-mode)
  :config
  (add-to-list 'company-backends '(company-tide company-files)))

(global-set-key (kbd "C-x m") 'browse-url-at-point)

(use-package eww
  :defer t
  :init
  (setq browse-url-browser-function
        '((".*google.*maps.*" . browse-url-generic)
          ;; Github goes to firefox, but not gist
          ("corp.digitalreasoning.com" . browse-url-generic)
          ("http.*\/\/github.com" . browse-url-generic)
          ("groups.google.com" . browse-url-generic)
          ("docs.google.com" . browse-url-generic)
          ("melpa.org" . browse-url-generic)
          ("build.*\.elastic.co" . browse-url-generic)
          (".*-ci\.elastic.co" . browse-url-generic)
          ("internal-ci\.elastic\.co" . browse-url-generic)
          ("zendesk\.com" . browse-url-generic)
          ("salesforce\.com" . browse-url-generic)
          ("stackoverflow\.com" . browse-url-generic)
          ("apache\.org\/jira" . browse-url-generic)
          ("thepoachedegg\.net" . browse-url-generic)
          ("zoom.us" . browse-url-generic)
          ("t.co" . browse-url-generic)
          ("twitter.com" . browse-url-generic)
          ("\/\/a.co" . browse-url-generic)
          ("youtube.com" . browse-url-generic)
          ("amazon.com" . browse-url-generic)
          ("." . eww-browse-url)))
  (setq shr-external-browser 'browse-url-generic)
  (if *is-a-mac*
        (setq browse-url-generic-program "open")
    (setq browse-url-generic-program (executable-find "firefox")))
  (add-hook 'eww-mode-hook #'toggle-word-wrap)
  (add-hook 'eww-mode-hook #'visual-line-mode)
  :config
  (use-package s :ensure t)
  (define-key eww-mode-map "o" 'eww)
  (define-key eww-mode-map "O" 'eww-browse-with-external-browser)
  (define-key eww-mode-map "j" 'next-line)
  (define-key eww-mode-map "k" 'previous-line)

  (use-package eww-lnum
    :ensure t
    :config
    (bind-key "f" #'eww-lnum-follow eww-mode-map)
    (bind-key "U" #'eww-lnum-universal eww-mode-map)))

(use-package link-hint
  :ensure t
  :bind ("C-c f" . link-hint-open-link))

;; Hooks
   (setq initial-scratch-message ";; ╔═╗┌─┐┬─┐┌─┐┌┬┐┌─┐┬ ┬\n;; ╚═╗│  ├┬┘├─┤ │ │  ├─┤\n;; ╚═╝└─┘┴└─┴ ┴ ┴ └─┘┴ ┴\n")

  (when (fboundp 'global-eldoc-mode)
    (add-hook 'after-init-hook 'global-eldoc-mode))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))
