;;Set keys for different operating systems


;; any graphical desktop
(when *is-gui*
  (setq mouse-wheel-scroll-amount '(0.001)))


;; linux
(when *is-linux*
  ;; when in a desktop environment
  (when *is-gui*))

;; windows
(when *is-windows*)

;; mac
(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq default-input-method "MacOSX")
  (when *is-cocoa-emacs*
    (global-set-key (kbd "M-`") 'ns-next-frame)
    (global-set-key (kbd "M-h") 'ns-do-hide-emacs)
    (eval-after-load 'nxml-mode
      '(define-key nxml-mode-map (kbd "M-h") nil))
    (global-set-key (kbd "M-_") 'ns-do-hide-others)
    ))

(provide 'init-os-keys)
