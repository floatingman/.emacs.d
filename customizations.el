(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(bbdb-file "~/.emacs.private/dot-bbdb")
 '(ido-case-fold t)
 '(ido-confirm-unique-completion t)
 '(ido-enable-last-directory-history t)
 '(ido-enable-prefix t)
 '(ido-enable-tramp-completion nil)
 '(ido-everywhere t)
 '(ido-ignore-buffers (quote ("\\` " "^*Mess" "^*Back" ".*Completion" "^*Ido" "^*trace" "^*compilation" "^*GTAGS" "^session.*" "^*")))
 '(ido-max-prospects 8)
 '(ido-max-work-directory-list 30)
 '(ido-max-work-file-list 50)
 '(ido-mode (quote both) nil (ido))
 '(ido-record-ftp-work-directories nil)
 '(ido-show-dot-for-dired nil)
 '(ido-use-filename-at-point nil)
 '(ido-use-url-at-point nil)
 '(ido-work-directory-list (quote ("~/" "~/code" "C:/Users/dnewman/Desktop")) t)
 '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(server-mode t)
 '(temp-buffer-resize-mode t)
 '(tabbar-background-color "black")
 '(tabbar-home-button (quote (("[o]") "[x]")))
 '(tabbar-mode t nil (tabbar))
 '(tabbar-mwheel-mode t nil (tabbar))
 '(tabbar-scroll-left-button (quote (("<") "=")))
 '(tabbar-scroll-right-button (quote ((">") "=")))
 '(tabbar-separator (quote ("|")))
 '(tabbar-use-images nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "SystemWindow" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "outline" :family "Consolas"))))
 '(highlight ((t (:background "cyan"))))
 '(hl-line ((t (:inherit highlight :background "darkseagreen2"))))
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t)
 '(secondary-selection ((t (:background "blue")))))
