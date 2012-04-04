;; apache-mode.el
(add-to-list 'auto-mode-alist '("apache2\\.conf\\'"  . apache-mode))

;; cvs-mode.el
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)
  
;; maplev
(autoload 'maplev-mode "maplev" "Maple editing mode" t)
(autoload 'cmaple      "maplev" "Start maple process" t)
(add-to-list 'auto-mode-alist '("\\.mpl\\'" . maplev-mode))

;; matlab
(defcustom matlab-auto-mode nil
  "*Enter matlab-mode when editing .m files.
Technically, this adjusts the `auto-mode-list' when set.
To unset, you will have to restart Emacs."
  :type 'boolean
  :set (lambda (symbol value)
         (set-default symbol value)
         (cond
          (value
           (add-to-list 'auto-mode-alist '("\\.m\\'" . matlab-mode)))))
  :load 'matlab
  :require 'matlab)
  
;; slang-mode.el
(setq auto-mode-alist
      (append '(("\\.sl\\'" . slang-mode)) auto-mode-alist))

;; upstart-mode.el
(when (not (featurep 'xemacs))
  (autoload 'upstart-mode "upstart-mode"
    "major mode for .upstart files."
    t)
  (add-to-list 'auto-mode-alist '("\\.upstart\\'" . upstart-mode)))
  
