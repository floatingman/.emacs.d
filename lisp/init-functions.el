(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginnning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((org-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
		'my/smarter-move-beginning-of-line)

(defun sanityinc/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(bind-key "C-M-<backspace>" 'sanityinc/kill-back-to-indentation)

(defun whitespace-cleanup-all ()
  (interactive)
  (setq indent-tab-mode nil)
  (whitespace-cleanup))

;;copy buffer into clipboard
(defun my/copy-buffer ()
	"Copy buffer contents to kill ring."
	(interactive)
	(kill-new (buffer-substring-no-properties (point-min) (point-max))))


;; setup for duplicate region taken from https://github.com/magnars/.emacs.d
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated."
  (interactive "p")
  (if (region-active-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (duplicate-region arg beg end)
        (one-shot-keybinding "d" (λ (duplicate-region 1 beg end))))
    (duplicate-current-line arg)
    (one-shot-keybinding "d" 'duplicate-current-line)))

(defun one-shot-keybinding (key command)
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd key) command)
     map) t))

(defun replace-region-by (fn)
  (let* ((beg (region-beginning))
         (end (region-end))
         (contents (buffer-substring beg end)))
    (delete-region beg end)
    (insert (funcall fn contents))))

(defun duplicate-region (&optional num start end)
  "Duplicates the region bounded by START and END NUM times.
If no START and END is provided, the current region-beginning and
region-end is used."
  (interactive "p")
  (save-excursion
    (let* ((start (or start (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring start end)))
      (goto-char end)
      (dotimes (i num)
        (insert region)))))

(defun paredit-duplicate-current-line ()
  (back-to-indentation)
  (let (kill-ring kill-ring-yank-pointer)
    (paredit-kill)
    (yank)
    (newline-and-indent)
    (yank)))

(defun duplicate-current-line (&optional num)
  "Duplicate the current line NUM times."
  (interactive "p")
  (if (bound-and-true-p paredit-mode)
      (paredit-duplicate-current-line)
    (save-excursion
      (when (eq (point-at-eol) (point-max))
        (goto-char (point-max))
        (newline)
        (forward-char -1))
      (duplicate-region num (point-at-bol) (1+ (point-at-eol))))))

;; special os specific functions
(when (eq system-type 'gnu/linux)
  (setq dired-listing-switches "-lFaGh1v --group-directories-first")
  (defun yank-to-x-clipboard ()
    (interactive)
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))

  (global-set-key (kbd "C-M-w") 'yank-to-x-clipboard)
  ;; suspend-frame isn't working on Linux?
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen nil)
  ;; brew install coreutils
  (if (executable-find "gls")
      (progn
        (setq insert-directory-program "gls")
        (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
    (setq dired-listing-switches "-ahlF"))
  (defun copy-from-osx ()
    "Handle copy/paste intelligently on osx."
    (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
      (if (and (eq system-type 'darwin)
               (file-exists-p pbpaste))
          (let ((tramp-mode nil)
                (default-directory "~"))
            (shell-command-to-string pbpaste)))))

  (defun paste-to-osx (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "/usr/bin/pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-osx
        interprogram-paste-function 'copy-from-osx)

  (defun move-file-to-trash (file)
    "Use `trash' to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash\"."
    (call-process (executable-find "trash")
                  nil 0 nil
                  file))

  ;; Trackpad scrolling
  (global-set-key [wheel-up] 'previous-line)
  (global-set-key [wheel-down] 'next-line))

(provide 'init-functions)
