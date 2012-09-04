(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
(global-set-key (kbd "M-o") 'other-window) ; was facemenu-keymap
(global-set-key (kbd "M-k") 'kill-this-buffer)
;;(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-f") 'insert-file-name)
(global-set-key (kbd "M-Y") 'yank-pop-backwards)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-M-}") 'forward-page)
(global-set-key (kbd "C-M-{") 'backward-page)

;Make page up and page down a whole lot nicer
(global-set-key "\C-v" 'pager-page-down)
(global-set-key [next] 'pager-page-down)
(global-set-key "\ev" 'pager-page-up)
(global-set-key [prior] 'pager-page-up)
(global-set-key '[M-up] 'pager-row-up)
(global-set-key '[M-kp-8] 'pager-row-up)
(global-set-key '[M-down] 'pager-row-down)
(global-set-key '[M-kp-2] 'pager-row-down)

;; some shortcuts
(global-set-key  [f4]  'goto-line)

;; moving between compilation error
(global-set-key [f2] 'previous-error)
(global-set-key [f3] 'next-error)

;; Get a buffer menu with the right mouse button.
(global-set-key [mouse-3] 'mouse-buffer-menu)

;; ease killing of innocent buffers
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)
(global-set-key (kbd "C-x K") 'kill-other-buffer)
(global-set-key (kbd "C-x C-S-K") 'kill-other-buffer-and-window)

;; make (un)commenting easy ;)
(global-set-key (kbd "C-#") 'comment-region)
(global-set-key (kbd "C-\'") 'uncomment-region)
;; toggle line numer display
(global-set-key (kbd "C-c n") 'global-linum-mode)

;; browse kill ring bindings
;(browse-kill-ring-default-keybindings)

;; iedit keybindings
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)

;; python keybindings
;(eval-after-load 'python
;  '(progn
;     (define-key python-mode-map [f2] 'flymake-goto-prev-error)
;     (define-key python-mode-map [f3] 'flymake-goto-next-error)
;     (define-key python-mode-map (kbd ".") 'ac-self-insert-and-complete)))

;; buffer-move
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;; Copy-Cut-Paste from clipboard with Super-C Super-X Super-V
(global-set-key (kbd "s-x") 'clipboard-kill-region) ;;cut
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ;;copy
(global-set-key (kbd "s-v") 'clipboard-yank) ;;paste

;; calc-mode more comfortable
(global-set-key (kbd "M-c") 'calc-dispatch)

;;insert date in standard format
(global-set-key (kbd "C-c d") 'insert-date)

;; duplicate current line
(global-set-key (kbd "C-c d") 'duplicate-line)

;;move current region or current line up and down
(global-set-key [(M C up)] 'move-text-up)
(global-set-key [(M C down)] 'move-text-down)

;;Mark whole line
(global-set-key (kbd "C-c l") 'mark-line)

;; duplicate a line
(global-set-key (kbd "C-c y") 'djcb-duplicate-line)

;; duplicate a line and comment the first
(global-set-key (kbd "C-c c")(lambda()(interactive)(djcb-duplicate-line t)))

;; increase or increase font-size
;;(global-set-key (kbd "C-+") 'ryan/increase-font-size)
;;(global-set-key (kbd "C--") 'ryan/decrease-font-size)

;; emacs-starter-kit-bindings
;; You know, like Readline.
(global-set-key (kbd "C-M-h") 'backward-kill-word)
;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)
;; Completion that uses many different methods to find options.
(global-set-key (kbd "M-/") 'hippie-expand)
;; Turn on the menu bar for exploring new modes
(global-set-key [f1] 'menu-bar-mode)
;; Font Size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
;; Use regex searches by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
;; File Finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
;; Window switching. (C-x o goes to the next window)
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two
;; Indentation help
(global-set-key (kbd "C-x ^") 'join-line)
;; M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)
;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
    (lambda () (interactive)
      (let ((case-fold-search isearch-case-fold-search))
        (occur (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))))
;;Rgrep
;;Rgrep is infinitely useful in multi-file projects.
(define-key global-map "\C-x\C-r" 'rgrep)
