(defun my/key-chord-define (keymap keys command)
  "Define in KEYMAP, a key-chord of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

MODIFICATION: Do not define the transposed key chord.
"
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (define-key keymap (vector 'key-chord key1 key2) command)))
(fset 'key-chord-define 'my/key-chord-define)

(defun my/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my/org-check-agenda ()
  "Peek at agenda."
  (interactive)
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (if (window-parent) (delete-window) (bury-buffer)))
   ((get-buffer "*Org Agenda*")
    (switch-to-buffer-other-window "*Org Agenda*"))
   (t (org-agenda nil "a"))))

(defun my/goto-random-char ()
  (interactive)
  (goto-char (random (point-max))))

(use-package hydra :ensure t
  :config
  (defhydra my/goto-random-char-hydra ()
    ("r" my/goto-random-char))

  (defhydra my/window-movement ()
    ("<left>" windmove-left)
    ("<right>" windmove-right)
    ("<down>" windmove-down)
    ("<up>" windmove-up)
    ("y" other-window "other")
    ("h" switch-window "switch-window")
    ("f" find-file "file")
    ("F" find-file-other-window "other file")
    ("v" (progn (split-window-right) (windmove-right)))
    ("o" delete-other-windows :color blue)
    ("a" ace-window)
    ("s" ace-swap-window)
    ("d" delete-window "delete")
    ("D" ace-delete-window "ace delete")
    ("i" ace-maximize-window "maximize")
    ("b" helm-buffers-list)
    ("q" nil))
  (defhydra join-lines ()
    ("<up>" join-line)
    ("<down>" (join-line 1))
    ("t" join-line)
    ("n" (join-line 1)))
  
  
  (defhydra my/key-chord-commands ()
    "Main"
    ("k" kill-sexp)
    ("b" helm-buffers-list :color blue)
    ("f" find-file :color blue)
    ("." repeat)
    ("C-t" transpose-chars)
    ("w" my/engine-mode-hydra/body "web" :exit t)
    ("m" imenu :color blue)
    ("+" text-scale-increase)
    ("-" text-scale-decrease))
  (defhydra my/engine-mode-hydra (:color blue)
    "Engine mode"
    ("b" engine/search-my-blog "blog")
    ("f" engine/search-my-photos "flickr")
    ("m" engine/search-mail "mail")
    ("g" engine/search-google "google")
    ("e" engine/search-emacswiki "emacswiki"))
  )


;; going to try out this key chord thing
(use-package key-chord
  :ensure t
  :init
  (progn
    (fset 'key-chord-define 'my/key-chord-define)
    (setq key-chord-one-key-delay 0.16)
    (key-chord-mode 1)
    (key-chord-define-global "uu"    'undo)
    (key-chord-define-global "jr"    'my/goto-random-char-hydra/my/goto-random-char)
    (key-chord-define-global "kk"     'my/org/body)
    (key-chord-define-global "jj"     'avy-goto-word-1)
    (key-chord-define-global "yy"    'my/window-movement/body)
    (key-chord-define-global "jw"     'switch-window)
    (key-chord-define-global "jl"     'avy-goto-line)
    (key-chord-define-global "j."     'join-lines/body)
    ;(key-chord-define-global "jZ"     'avy-zap-to-char)
    (key-chord-define-global "FF"     'find-file)
    (key-chord-define-global "qq"     'my/quantified-hydra/body)
    (key-chord-define-global "hh"     'my/key-chord-commands/body)
    (key-chord-define-global "xx"     'er/expand-region)
    (key-chord-define-global "  "     'my/insert-space-or-expand)
    (key-chord-define-global "JJ"     'my/switch-to-previous-buffer)))

;; quickly jump to different points in current view
(use-package avy :ensure t)
  ;; I use the jj key-chord for this; see the definitions for key-chord
(use-package avy-zap
  :ensure t
  :bind
  (("M-z" . avy-zap-up-to-char-dwim)
   ("M-Z" . avy-zap-to-char-dwim)))
(provide 'init-keybinding)
