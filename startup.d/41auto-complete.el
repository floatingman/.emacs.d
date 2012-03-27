;(defun ac-common-setup ()
;  (add-to-list 'ac-sources 'ac-source-filename 'ac-source-yasnippet)
;)
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/dict")
;; Do What I Mean mode
(setq ac-dwim t)

(setq ac-modes (quote (emacs-lisp-mode lisp-interaction-mode c-mode cc-mode c++-mode clojure-mode java-mode perl-mode cperl-mode python-mode ruby-mode ecmascript-mode javascript-mode js-mode js2-mode php-mode css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode haml-mode sass-mode)))

(setq ac-quick-help-delay 1.0)

;; custom keybindings to use tab, enter and up and down arrows
(define-key ac-complete-mode-map "\t" 'ac-expand)
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\M-n" 'ac-next)
(define-key ac-complete-mode-map "\M-p" 'ac-previous)
