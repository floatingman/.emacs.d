(icomplete-mode 1)


(setq sentence-end-double-space nil)


(global-set-key (kbd "RET") 'newline-and-indent)

;; Searching based on the current word
(defun sacha/search-word-backward ()
  "Find the previous occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-backward "w_")
    (goto-char
     (if (re-search-backward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))

(defun sacha/search-word-forward ()
  "Find the next occurrence of the current word."
  (interactive)
  (let ((cur (point)))
    (skip-syntax-forward "w_")
    (goto-char
     (if (re-search-forward (concat "\\_<" (current-word) "\\_>") nil t)
         (match-beginning 0)
       cur))))
(global-set-key '[M-up] 'sacha/search-word-backward)
(global-set-key '[M-down] 'sacha/search-word-forward)
(defadvice search-for-keyword (around sacha activate)
  "Match in a case-insensitive way."
  (let ((case-fold-search t))
    ad-do-it))

;;Ido-mode: Show recent files
(ido-mode 1)
(setq ido-default-buffer-method 'selected-window)
(add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
(add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
                      (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
                  (if (= (nth 0 ta) (nth 0 tb))
                      (> (nth 1 ta) (nth 1 tb))
                    (> (nth 0 ta) (nth 0 tb)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (if (string-equal (substring x 0 1) ".") x))
              ido-temp-list))))

;;kill ring mangement
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-quit-action 'save-and-restore)


;; better undo handling
(require 'undo-tree)
(global-undo-tree-mode)

;;count frequently used keys
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;avoid weasel words
(require 'artbollocks-mode)
;; Avoid these phrases
(setq weasel-words-regex
      (concat "\\b" (regexp-opt
                     '("one of the"
                       "should"
                       "just"
                       "sort of"
                       "a lot"
                       "probably"
                       "maybe"
                       "perhaps"
                       "I think"
                       "really"
                       "pretty"
                       "maybe"
                       "nice"
                       "action"
                       "utilize"
                       "leverage") t) "\\b"))
;; Fix a bug in the regular expression to catch repeated words
(setq lexical-illusions-regex "\\b\\(\\w+\\)\\W+\\(\\1\\)\\b")
;; Don't show the art critic words, or at least until I figure
;; out my own jargon
(setq artbollocks nil)
;; Make sure keywords are case-insensitive
(defadvice search-for-keyword (around sacha activate)
  "Match in a case-insensitive way."
  (let ((case-fold-search t))
    ad-do-it))

(add-hook 'org-capture-mode-hook 'artbollocks-mode)
