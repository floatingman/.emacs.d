(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-insert (quote other))
 '(auto-insert-alist (quote ((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header") . ["template.h" c++-mode my/autoinsert-yas-expand]) (("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ source") . ["template.cc" my/autoinsert-yas-expand]) (("\\.sh\\'" . "Shell script") . ["template.sh" my/autoinsert-yas-expand]) (("\\.el\\'" . "Emacs Lisp") . ["template.el" my/autoinsert-yas-expand]) (("\\.pl\\'" . "Perl script") . ["template.pl" my/autoinsert-yas-expand]) (("\\.pm\\'" . "Perl module") . ["template.pm" my/autoinsert-yas-expand]) (("\\.py\\'" . "Python script") . ["template.py" my/autoinsert-yas-expand]) (("[mM]akefile\\'" . "Makefile") . ["Makefile" my/autoinsert-yas-expand]) (("\\.tex\\'" . "TeX/LaTeX") . ["template.tex" my/autoinsert-yas-expand]))))
 '(auto-insert-directory "~/.emacs.d/autoinsert/")
 '(bbdb-file "~/.emacs.private/dot-bbdb")
 ;; '(ido-case-fold t)
 ;; '(ido-confirm-unique-completion t)
 ;; '(ido-enable-last-directory-history t)
 ;; '(ido-enable-prefix t)
 ;; '(ido-enable-tramp-completion nil)
 ;; '(ido-everywhere t)
 ;; '(ido-ignore-buffers (quote ("\\` " "^*Mess" "^*Back" ".*Completion" "^*Ido" "^*trace" "^*compilation" "^*GTAGS" "^session.*" "^*")))
 ;; '(ido-max-prospects 8)
 ;; '(ido-max-work-directory-list 30)
 ;; '(ido-max-work-file-list 50)
 ;; '(ido-mode (quote both) nil (ido))
 ;; '(ido-record-ftp-work-directories nil)
 ;; '(ido-save-directory-list-file "~/emacs-meta/.ido.last")
 ;; '(ido-show-dot-for-dired nil)
 ;; '(ido-use-filename-at-point nil)
 ;; '(ido-use-url-at-point nil)
 ;; '(ido-work-directory-list (quote ("~/" "~/code" "C:/Users/dnewman/Desktop")) t)
 ;; '(mouse-avoidance-mode (quote animate) nil (avoid))
 '(server-mode t)
 '(smex-save-file "~/emacs-meta/.smex-items")
 '(tabbar-background-color "black")
 '(tabbar-home-button (quote (("[o]") "[x]")))
 '(tabbar-mode t nil (tabbar))
 '(tabbar-mwheel-mode t nil (tabbar))
 '(tabbar-scroll-left-button (quote (("<") "=")))
 '(tabbar-scroll-right-button (quote ((">") "=")))
 '(tabbar-separator (quote ("|")))
 '(tabbar-use-images nil)
 '(temp-buffer-resize-mode t)
 '(tidy-temp-directory "~/emacs-meta/"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t))
