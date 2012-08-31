;; By Fabián Ezequiel Gallina
(defun copy-line ()
  "Copy line from point"
  (interactive)
  (save-excursion
    (let ((start (point-marker)))
      (end-of-line)
      (kill-ring-save start (point-marker)))))

;; http://www.emacswiki.org/emacs/BackwardDeleteWord
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

;; http://www.emacswiki.org/emacs/BackwardDeleteWord
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

;; http://www.emacswiki.org/emacs/JorgenSchaefersEmacsConfig
;; Credits to Jorgen Schaefers
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%d.%m.%Y")
                 ((equal prefix '(4)) "%Y-%m-%d")
                 ((equal prefix '(16)) "%A, %d. %B %Y"))))
    (insert (format-time-string format))))

(defun su ()
  "Reopen current file as root"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/su::" buffer-file-name))))


(defun sudo ()
  "Reopen current file as sudoer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:" buffer-file-name))))

;; http://www.emacswiki.org/emacs/DefaultKillingAndYanking
(defun yank-pop-backwards ()
  "Yank backwards"
  (interactive)
  (yank-pop -1)
  (message "yanking backwards"))

;; Replace regex all buffers (query) By Fabián Ezequiel Gallina
(defun query-replace-regexp-all-buffers (regexp replace)
  "Runs query replace regexp in all open buffers"
  (interactive
   (list
    (read-string "Regexp to replace in all buffers: " "" nil "")
    (read-string "Replace: " "" nil "")))
  (let ((buffers ()))
    ;; Get all the buffers we are interested in
    (dolist (buffer (buffer-list))
      (if (and (not (string= (substring (buffer-name buffer) 0 1) " "))
               (not (null (buffer-file-name buffer))))
          (add-to-list 'buffers buffer)))
    ;; Run query replace in all buffers...
    (save-excursion
      (dolist (buffer buffers)
        (save-restriction
          (widen)
          (beginning-of-buffer)
          (query-replace-regexp regexp replace)
          (switch-to-buffer buffer))))))

;; http://www.emacswiki.org/emacs/InsertFileName
(defun insert-file-name (arg filename)
  "Insert name of file FILENAME into buffer after point.
Set mark after the inserted text.

Prefixed with \\[universal-argument], expand the file name to
its fully canocalized path.

See `expand-file-name'."
  ;; Based on insert-file in Emacs -- ashawley 2008-09-26
  (interactive "*P\nfInsert file name: ")
  (if arg
      (insert (expand-file-name filename))
    (insert filename)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Increase/Decrease font size on the fly
;;; Taken from: http://is.gd/iaAo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ryan/increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun ryan/decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                  (face-attribute 'default :height)))))

;; code borrowed from http://emacs-fu.blogspot.com/2010/01/duplicating-lines-and-commenting-them.html
(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the
original" (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
    (comment-region (region-beginning) (region-end)))
    (insert-string
      (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

;; Mark whole line
(defun mark-line (&optional arg)
  "Marks a line"
  (interactive "p")
  (beginning-of-line)
  (push-mark (point) nil t)
  (end-of-line))

; code copied from http://stackoverflow.com/questions/2423834/move-line-region-up-and-down-in-emacs
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

; patches by balle
; http://www.datenterrorist.de
(defun balle-python-shift-left ()
  (interactive)
  (let (start end bds)
    (if (and transient-mark-mode
	   mark-active)
	(setq start (region-beginning) end (region-end))
      (progn
	(setq bds (bounds-of-thing-at-point 'line))
	(setq start (car bds) end (cdr bds))))
  (python-indent-shift-left start end))
  (setq deactivate-mark nil)
)

(defun balle-python-shift-right ()
  (interactive)
  (let (start end bds)
    (if (and transient-mark-mode
	   mark-active)
	(setq start (region-beginning) end (region-end))
      (progn
	(setq bds (bounds-of-thing-at-point 'line))
	(setq start (car bds) end (cdr bds))))
  (python-indent-shift-right start end))
  (setq deactivate-mark nil)
)

(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map (kbd "M-<right>")
	      'balle-python-shift-right)
	    (define-key python-mode-map (kbd "M-<left>")
	      'balle-python-shift-left))
	  )

;;auto indent pasted text into a buffer
;;http://www.emacswiki.org/emacs/AutoIndentation#SmartPaste

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

; defining line duplication hotkey
(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (newline)
  (yank)
)

(defun save-macro (name)
  "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
  (interactive "SName of the macro :")  ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file "~/.emacs.d/startup.d/04macros.el")                   ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer


;; Insert date ask for format
(defun insert-date (format)
  "Wrapper around format-time-string."
  (interactive "MFormat: ")
  (insert (format-time-string format)))

;; Insert date in standard format
(defun insert-standard-date ()
  "Inserts standard date time string."
  (interactive)
  (insert (format-time-string "%c")))

;;insert a date from the calendar or a date a # of days from today
(require 'calendar)

(defun insdate-insert-any-date (date)
  "Insert DATE using the current locale."
  (interactive (list (calendar-read-date)))
  (insert (calendar-date-string date)))

(defun insdate-insert-date-from (&optional days)
  "Insert date that is DAYS from current."
  (interactive "p*")
  (insert
   (calendar-date-string
    (calendar-gregorian-from-absolute
     (+ (calendar-absolute-from-gregorian (calendar-current-date))
        days)))))


;;using idle timer to kill "Shell Command Output" can be customized to kill
;;other buffers
(if completion-ignore-case
    (defvar reaper
      (run-with-idle-timer 5 t (lambda ()
                                 (let ((victim (get-buffer "*Shell Command Output*")))
                                   (when (and victim (not (get-buffer-window victim t)))
                                     (message "Killing buffer %s" (buffer-name victim))
                                     (kill-buffer victim)))))
      "idle-timer (see \\[run-with-idle-timer]) that deletes the
      *Shell Command Output* buffer -- that buffer's name is so similar to
      *shell* that completion makes the latter hard to type.  Use
      \\[cancel-timer] to cancel it."))


(defun starter-kit-local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun starter-kit-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun starter-kit-turn-on-save-place-mode ()
  (setq save-place t))

(defun starter-kit-turn-on-whitespace ()
  (whitespace-mode t))

(add-hook 'starter-kit-coding-hook 'starter-kit-local-column-number-mode)

(add-hook 'starter-kit-coding-hook 'starter-kit-local-comment-auto-fill)


(when (window-system)
  (add-hook 'starter-kit-coding-hook 'starter-kit-pretty-lambdas))

(defun run-starter-kit-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'starter-kit-coding-hook))

(defun recentf-ido-find-file ()
    "Find a recent file using Ido."
    (interactive)
    (let* ((file-assoc-list
            (mapcar (lambda (x)
                      (cons (file-name-nondirectory x)
                            x))
                    recentf-list))
           (filename-list
            (remove-duplicates (mapcar #'car file-assoc-list)
                               :test #'string=))
           (filename (ido-completing-read "Choose recent file: "
                                          filename-list
                                          nil
                                          t)))
      (when filename
        (find-file (cdr (assoc filename
                               file-assoc-list))))))
