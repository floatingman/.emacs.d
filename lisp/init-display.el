(eval-when-compile
  (defvar emacs-min-height)
  (defvar emacs-min-width))

(defvar display-name
  (let ((width (display-pixel-width)))
    (cond ((>= width 2560) 'retina-imac)
          ((= width 1920) 'macbook-pro-vga)
          ((= width 1680) 'macbook-pro)
          ((= width 1440) 'retina-macbook-pro))))

(defvar emacs-min-top 23)
(defvar emacs-min-left
  (cond ((eq display-name 'retina-imac) 975)
        ((eq display-name 'macbook-pro-vga) 837)
        (t 521)))
(defvar emacs-min-height
  (cond ((eq display-name 'retina-imac) 57)
        ((eq display-name 'macbook-pro-vga) 54)
        ((eq display-name 'macbook-pro) 47)
        (t 44)))
(defvar emacs-min-width 100)

(if running-alternate-emacs
    (setq emacs-min-top 22
          emacs-min-left 5
          emacs-min-height 57
          emacs-min-width 90))


(defvar emacs-min-font
  (cond
   ((eq display-name 'retina-imac)
    (if running-alternate-emacs
        "-*-Myriad Pro-normal-normal-normal-*-20-*-*-*-p-0-iso10646-1"
      ;; "-*-Source Code Pro-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"
      "-*-Hack-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1"
      ))
   ((eq display-name 'macbook-pro)
    (if running-alternate-emacs
        "-*-Myriad Pro-normal-normal-normal-*-20-*-*-*-p-0-iso10646-1"
      ;; "-*-Source Code Pro-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"
      "-*-Hack-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"
      ))
   ((eq display-name 'macbook-pro-vga)
    (if running-alternate-emacs
        "-*-Myriad Pro-normal-normal-normal-*-20-*-*-*-p-0-iso10646-1"
      ;; "-*-Source Code Pro-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"
      "-*-Hack-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1"
      ))
   ((string= (system-name) "ubuntu")
    ;; "-*-Source Code Pro-normal-normal-normal-*-20-*-*-*-m-0-iso10646-1"
    "-*-Hack-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
    )
   (t
    (if running-alternate-emacs
        "-*-Myriad Pro-normal-normal-normal-*-17-*-*-*-p-0-iso10646-1"
      ;; "-*-Source Code Pro-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1"
      "-*-Hack-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1"
      ))))


(let ((frame-alist
       (list (cons 'top    emacs-min-top)
             (cons 'left   emacs-min-left)
             (cons 'height emacs-min-height)
             (cons 'width  emacs-min-width)
             (cons 'font   emacs-min-font))))
  (setq initial-frame-alist frame-alist))

(defun emacs-min ()
  (interactive)

  (set-frame-parameter (selected-frame) 'fullscreen nil)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil)

  (set-frame-parameter (selected-frame) 'top emacs-min-top)
  (set-frame-parameter (selected-frame) 'left emacs-min-left)
  (set-frame-parameter (selected-frame) 'height emacs-min-height)
  (set-frame-parameter (selected-frame) 'width emacs-min-width)

  (set-frame-font emacs-min-font))

(if window-system
    (add-hook 'after-init-hook 'emacs-min))

(defun emacs-max ()
  (interactive)
  (set-frame-parameter (selected-frame) 'fullscreen 'fullboth)
  (set-frame-parameter (selected-frame) 'vertical-scroll-bars nil)
  (set-frame-parameter (selected-frame) 'horizontal-scroll-bars nil))

(defun emacs-toggle-size ()
  (interactive)
  (if (> (cdr (assq 'width (frame-parameters))) 100)
      (emacs-min)
    (emacs-max)))

(bind-key "C-c m" #'emacs-toggle-size)

(provide 'init-display)
