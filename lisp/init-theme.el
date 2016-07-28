(use-package smart-mode-line
  :init
  (progn
    (setq sml/theme my/background)
    (sml/setup))
  :config
  (setq sml/shorten-directory t
        sml/shorten-modes t)
  (add-to-list 'sml/replacer-regexp-list '("^~/es/x-plugins/" ":X:"))
  (add-to-list 'sml/replacer-regexp-list '("^~/es/elasticsearch/" ":ES:") t))

;set line highlight to a more subtle gray
(set-face-background hl-line-face "gray13")

(setq ns-use-srgb-colorspace t)

(defun dakrone-dark ()
  (interactive)
  (use-package moe-theme
    :disabled t
    :commands (moe-dark moe-light)
    :init
    ;; Show highlighted buffer-id as decoration. (Default: nil)
    (setq moe-theme-highlight-buffer-id t)
    (moe-dark))
  (use-package material-theme
    :disabled t
    :init (load-theme 'material t))
  (use-package apropospriate-theme
    :disabled t
    :init (load-theme 'apropospriate-dark t))
  (use-package color-theme-sanityinc-tomorrow
    :init (load-theme 'sanityinc-tomorrow-night t))
  (use-package tao-theme
    :disabled t
    :init (load-theme 'tao-yin t)))

(defun dakrone-light ()
  (interactive)
  (use-package leuven-theme
    :disabled t
    :init (load-theme 'leuven t))
  (use-package color-theme-sanityinc-tomorrow
    :disabled t
    :init (load-theme 'sanityinc-tomorrow-day t))
  (use-package solarized-theme
    :disabled t
    :init (load-theme 'solarized-light t))
  (use-package material-theme
    :disabled t
    :init (load-theme 'material-light t))
  (use-package tao-theme
    :init (load-theme 'tao-yang t)))

(if (eq my/background 'dark)
    (dakrone-dark)
  (dakrone-light))

(defun my/setup-x11-fonts ()
  (interactive)
  (when (eq window-system 'x)
    ;; Font family
    (set-frame-font "DejaVu Sans Mono")
    ;; (set-frame-font "Ubuntu Mono")
    ;; (set-frame-font "Hack")
    ;; (set-frame-font "Fantasque Sans Mono")
    ;; (set-frame-font "Anonymous Pro")
    ;; (set-frame-font "Inconsolata")
    (set-face-attribute 'default nil :height 105)))

(when (eq window-system 'x)
  (add-hook 'after-init-hook #'my/setup-x11-fonts))

(defun my/set-fringe-background ()
  "Set the fringe background to the same color as the regular background."
  (interactive)
  (setq my/fringe-background-color
        (face-background 'default))
  (custom-set-faces
   `(fringe ((t (:background ,my/fringe-background-color))))))

(add-hook 'after-init-hook #'my/set-fringe-background)

;; Indicate where a buffer stars and stops
(setq-default indicate-buffer-boundaries 'right)

(use-package symon
  :if window-system
  :disabled t
  :init
  (setq symon-refresh-rate 2
        symon-delay 5)
  (symon-mode 1)
  :config
  (setq symon-sparkline-type 'bounded))

(add-to-list
 'comint-preoutput-filter-functions
 (lambda (output)
   (replace-regexp-in-string "\\[0-9]+[GK]" "" output)))

(provide 'init-theme)
