(global-set-key (kbd "M-3") 'split-window-horizontally) ; was digit-argument
(global-set-key (kbd "M-2") 'split-window-vertically) ; was digit-argument
(global-set-key (kbd "M-1") 'delete-other-windows) ; was digit-argument
(global-set-key (kbd "M-0") 'delete-window) ; was digit-argument
(global-set-key (kbd "M-o") 'other-window) ; was facemenu-keymap
(global-set-key (kbd "M-k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c C-f") 'insert-file-name)
(global-set-key (kbd "M-Y") 'yank-pop-backwards)
(global-set-key (kbd "C-z") 'repeat)
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

;; align selected code on specific regex
(global-set-key (kbd "C-x a r") 'align-regexp)

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
(browse-kill-ring-default-keybindings)

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
