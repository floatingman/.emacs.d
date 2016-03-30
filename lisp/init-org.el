(defun my/org-mode-hook ()
  (interactive)
  (turn-on-auto-fill)
  (turn-on-flyspell)
  (when (fboundp 'yas-minor-mode)
    (yas-minor-mode 1))
  (when (fboundp 'my/enable-abbrev-mode)
    (my/enable-abbrev-mode))

  ;; fix some bindings that org-mode overwrites
  (define-key org-mode-map [C-tab] 'other-window)
  (define-key org-mode-map [C-S-tab]
    (lambda ()
      (interactive)
      (other-window -1)))
  (define-key org-mode-map (kbd "C-'")
    'eyebrowse-next-window-config)
  (define-key org-mode-map (kbd "C-c C-x C-f") 'org-refile)
  (when (boundp 'org-agenda-mode-map)
    (define-key org-agenda-mode-map (kbd "C-c C-x C-f") 'org-agenda-refile)))

(use-package org
  :ensure org
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         ("C-c M-p" . org-babel-previous-src-block)
         ("C-c M-n" . org-babel-next-src-block)
         ("C-c S" . org-babel-previous-src-block)
         ("C-c s" . org-babel-next-src-block))
  :defer 30
  :config
  (progn
    (use-package org-install)
    (use-package ob-core)
    ;; org-export
    (use-package ox)
    ;; Enable archiving things
    (use-package org-archive)
    (add-hook 'org-mode-hook #'hl-line-mode)
    (add-hook 'org-mode-hook #'my/org-mode-hook)
    ;; enabled export backends
    (custom-set-variables '(org-export-backends '(ascii html latex md rss)))
    (setq org-directory (file-truename "~/personal/org")
          ;; follow links by pressing ENTER on them
          org-return-follows-link t
          ;; allow changing between todo stats directly by hotkey
          org-use-fast-todo-selection t
          ;; syntax highlight code in source blocks
          org-src-fontify-natively t
          ;; for the leuven theme, fontify the whole heading line
          org-fontify-whole-heading-line t
          ;; force UTF-8
          org-export-coding-system 'utf-8
          ;; don't use ido completion (I use helm)
          org-completion-use-ido nil
          ;; start up org files with indentation (same as #+STARTUP: indent)
          org-startup-indented t
          ;; don't indent source code
          org-edit-src-content-indentation 0
          ;; don't adapt indentation
          org-adapt-indentation nil
          ;; preserve the indentation inside of source blocks
          org-src-preserve-indentation t
          ;; Imenu should use 3 depth instead of 2
          org-imenu-depth 3
          ;; put state change log messages into a drawer
          org-log-into-drawer t
          ;; special begin/end of line to skip tags and stars
          org-special-ctrl-a/e t
          ;; special keys for killing a headline
          org-special-ctrl-k t
          ;; don't adjust subtrees that I copy
          org-yank-adjusted-subtrees nil
          ;; try to be smart when editing hidden things
          org-catch-invisible-edits 'smart
          ;; blank lines are removed when exiting the code edit buffer
          org-src-strip-leading-and-trailing-blank-lines t
          ;; how org-src windows are set up when hitting C-c '
          org-src-window-setup 'current-window
          ;; Overwrite the current window with the agenda
          org-agenda-window-setup 'current-window
          ;; Use 100 chars for the agenda width
          org-agenda-tags-column -100
          ;; Use full outline paths for refile targets - we file directly with IDO
          org-refile-use-outline-path t
          ;; Targets complete directly with IDO
          org-outline-path-complete-in-steps nil
          ;; Allow refile to create parent tasks with confirmation
          org-refile-allow-creating-parent-nodes 'confirm
          ;; never leave empty lines in collapsed view
          org-cycle-separator-lines 0
          ;; Use cider as the clojure backend
          org-babel-clojure-backend 'cider
          ;; don't run stuff automatically on export
          org-export-babel-evaluate nil
          ;; export tables as CSV instead of tab-delineated
          org-table-export-default-format "orgtbl-to-csv"
          ;; start up showing images
          org-startup-with-inline-images t
          ;; always enable noweb, results as code and exporting both
          org-babel-default-header-args
          (cons '(:noweb . "yes")
                (assq-delete-all :noweb org-babel-default-header-args))
          org-babel-default-header-args
          (cons '(:exports . "both")
                (assq-delete-all :exports org-babel-default-header-args))
          ;; I don't want to be prompted on every code block evaluation
          org-confirm-babel-evaluate nil
          ;; Mark entries as done when archiving
          org-archive-mark-done t
          ;; Where to put headlines when archiving them
          org-archive-location "%s_archive::* Archived Tasks"
          ;; Sorting order for tasks on the agenda
          org-agenda-sorting-strategy
          '((agenda habit-down
                    time-up
                    priority-down
                    user-defined-up
                    effort-up
                    category-keep)
            (todo priority-down category-up effort-up)
            (tags priority-down category-up effort-up)
            (search priority-down category-up))
          ;; Enable display of the time grid so we can see the marker for the
          ;; current time
          org-agenda-time-grid
          '((daily today remove-match)
            #("----------------" 0 16 (org-heading t))
            (0900 1100 1300 1500 1700))
          ;; keep the agenda filter until manually removed
          org-agenda-persistent-filter t
          ;; show all occurrences of repeating tasks
          org-agenda-repeating-timestamp-show-all t
          ;; always start the agenda on today
          org-agenda-start-on-weekday nil
          ;; Use sticky agenda's so they persist
          org-agenda-sticky t
          ;; show 4 agenda days
          org-agenda-span 4
          ;; Do not dim blocked tasks
          org-agenda-dim-blocked-tasks nil
          ;; Compact the block agenda view
          org-agenda-compact-blocks t
          ;; Show all agenda dates - even if they are empty
          org-agenda-show-all-dates t
          ;; Agenda org-mode files
          org-agenda-files `(,(file-truename "~/personal/org/")
                             ,(file-truename "~/personal/org/danielnewmandesign/")
                             ,(file-truename "~/personal/org/clients/")))

    ;; Org todo keywords
    (setq org-todo-keywords
          '((sequence "TODO(t)" "|" "DONE(d)")
            (sequence "TODO(t)"
                      "SOMEDAY(s)"
                      "INPROGRESS(i)"
                      "HOLD(h)"
                      "WAITING(w@/!)"
                      "NEEDSREVIEW(n@/!)"
                      "|" "DONE(d)")
            (sequence "TODO(t)" "INPROGRESS(i)" "|" "CANCELLED(c@/!)")))
    ;; Org faces
    (setq org-todo-keyword-faces
          '(("TODO" :foreground "red" :weight bold)
            ("INPROGRESS" :foreground "deep sky blue" :weight bold)
            ("SOMEDAY" :foreground "purple" :weight bold)
            ("NEEDSREVIEW" :foreground "#edd400" :weight bold)
            ("DONE" :foreground "forest green" :weight bold)
            ("WAITING" :foreground "orange" :weight bold)
            ("HOLD" :foreground "magenta" :weight bold)
            ("CANCELLED" :foreground "forest green" :weight bold)))
    ;; add or remove tags on state change
    (setq org-todo-state-tags-triggers
          '(("CANCELLED" ("CANCELLED" . t))
            ("WAITING" ("WAITING" . t))
            ("HOLD" ("WAITING") ("HOLD" . t))
            (done ("WAITING") ("HOLD"))
            ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
            ("INPROGRESS" ("WAITING") ("CANCELLED") ("HOLD"))
            ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
    ;; refile targets all level 1 and 2 headers in current file and agenda files
    (setq org-refile-targets '((nil :maxlevel . 2)
                               (org-agenda-files :maxlevel . 2)))
    ;; quick access to common tags
    (setq org-tag-alist
          '(("oss" . ?o)
            ("home" . ?h)
            ("work" . ?w)
            ("xplugins" . ?x)
            ("book" . ?b)
            ("support" . ?s)
            ("docs" . ?d)
            ("emacs" . ?e)
            ("noexport" . ?n)
            ("recurring" . ?r)))
    ;; capture templates
    (setq org-capture-templates
          '(("t" "Todo" entry (file "~/personal/org/refile.org")
             "* TODO %?\n%U\n")
            ("m" "Email" entry (file "~/personal/org/refile.org")
             "* TODO [#B] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")
            ("n" "Notes" entry (file+headline "~/personal/org/notes.org" "Notes")
             "* %? :NOTE:\n%U\n")
            ("e" "Emacs note" entry
             (file+headline "~/personal/org/notes.org" "Emacs Links")
             "* %? :NOTE:\n%U\n")
            ("j" "Journal" entry (file+datetree "~/personal/org/journal.org")
             "* %?\n%U\n")
            ("b" "Book/Bibliography" entry
             (file+headline "~/personal/org/bibliography.org" "Refile")
             "* %?%^{TITLE}p%^{AUTHOR}p%^{TYPE}p")))
    ;; Custom agenda command definitions
    (setq org-agenda-custom-commands
          '(("N" "Notes" tags "NOTE"
             ((org-agenda-overriding-header "Notes")
              (org-tags-match-list-sublevels t)))
            (" " "Agenda"
             ((agenda "" nil)
              ;; All items with the "REFILE" tag, everything in refile.org
              ;; automatically gets that applied
              (tags "REFILE"
                    ((org-agenda-overriding-header "Tasks to Refile")
                     (org-tags-match-list-sublevels nil)))
              ;; All "INPROGRESS" todo items
              (todo "INPROGRESS"
                    ((org-agenda-overriding-header "Current work")))
              ;; All headings with the "support" tag
              (tags "support/!"
                    ((org-agenda-overriding-header "Support cases")))
              ;; All "NEESREVIEW" todo items
              (todo "NEEDSREVIEW"
                    ((org-agenda-overriding-header "Waiting on reviews")))
              ;; All "WAITING" items without a "support" tag
              (tags "WAITING-support"
                    ((org-agenda-overriding-header "Waiting for something")))
              ;; All TODO items
              (todo "TODO"
                    ((org-agenda-overriding-header "Task list")
                     (org-agenda-sorting-strategy
                      '(time-up priority-down category-keep))))
              ;; Everything on hold
              (todo "HOLD"
                    ((org-agenda-overriding-header "On-hold")))
              ;; All headings with the "recurring" tag
              (tags "recurring/!"
                    ((org-agenda-overriding-header "Recurring"))))
             nil)))

    ;; Exclude DONE state tasks from refile targets
    (defun my/verify-refile-target ()
      "Exclude todo keywords with a done state from refile targets"
      (not (member (nth 2 (org-heading-components)) org-done-keywords)))
    (setq org-refile-target-verify-function 'my/verify-refile-target)

    ;; org-mode bindings
    (define-key org-mode-map (kbd "C-M-<return>") 'org-insert-todo-heading)
    (define-key org-mode-map (kbd "C-c t") 'org-todo)
    (define-key org-mode-map (kbd "M-G") 'org-plot/gnuplot)
    (define-key org-mode-map (kbd "RET") 'org-return-indent)
    ;; swap C-RET and M-RET
    (define-key org-mode-map (kbd "C-<return>") 'org-insert-heading)
    (define-key org-mode-map (kbd "M-<return>")
      'org-insert-heading-after-current)

    (local-unset-key (kbd "M-S-<return>"))

    ;; org-babel stuff
    (require 'ob-clojure)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (clojure . t)
       (dot . t)
       (sh . t)
       (js . t)
       (haskell . t)
       (ruby . t)
       (python . t)
       (gnuplot . t)
       (plantuml . t)
       (latex . t)))

    ;; this is where Fedora installs it, YMMV
    (setq org-plantuml-jar-path "/usr/share/java/plantuml.jar")

    ;; Use org.css from the :wq website for export document stylesheets
    (setq org-html-head-extra
          "<link rel=\"stylesheet\" href=\"http://dakrone.github.io/org.css\" type=\"text/css\" />"
          org-html-head-include-default-style nil)

    ;; ensure this variable is defined
    (unless (boundp 'org-babel-default-header-args:sh)
      (setq org-babel-default-header-args:sh '()))

    ;; add a default shebang header argument shell scripts
    (add-to-list 'org-babel-default-header-args:sh
                 '(:shebang . "#!/usr/bin/env bash"))

    ;; add a default shebang header argument for python
    (add-to-list 'org-babel-default-header-args:python
                 '(:shebang . "#!/usr/bin/env python"))

    ;; Clojure-specific org-babel stuff
    (defvar org-babel-default-header-args:clojure
      '((:results . "silent")))

    (defun org-babel-execute:clojure (body params)
      "Execute a block of Clojure code with Babel."
      (let ((result-plist
             (nrepl-send-string-sync
              (org-babel-expand-body:clojure body params) nrepl-buffer-ns))
            (result-type  (cdr (assoc :result-type params))))
        (org-babel-script-escape
         (cond ((eq result-type 'value) (plist-get result-plist :value))
               ((eq result-type 'output) (plist-get result-plist :value))
               (t (message "Unknown :results type!"))))))

    ;; Function declarations
    (defun my/skip-non-archivable-tasks ()
      "Skip trees that are not available for archiving"
      (save-restriction
        (widen)
        ;; Consider only tasks with done todo headings as archivable candidates
        (let ((next-headline (save-excursion
                               (or (outline-next-heading) (point-max))))
              (subtree-end (save-excursion (org-end-of-subtree t))))
          (if (member (org-get-todo-state) org-todo-keywords-1)
              (if (member (org-get-todo-state) org-done-keywords)
                  (let* ((daynr (string-to-int
                                 (format-time-string "%d" (current-time))))
                         (a-month-ago (* 60 60 24 (+ daynr 1)))
                         (this-month
                          (format-time-string "%Y-%m-" (current-time)))
                         (subtree-is-current
                          (save-excursion
                            (forward-line 1)
                            (and (< (point) subtree-end)
                                 (re-search-forward this-month
                                                    subtree-end t)))))
                    (if subtree-is-current
                        subtree-end     ; Has a date in this month, skip it
                      nil))             ; available to archive
                (or subtree-end (point-max)))
            next-headline))))

    (defun my/save-all-agenda-buffers ()
      "Function used to save all agenda buffers that are
   currently open, based on `org-agenda-files'."
      (interactive)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (member (buffer-file-name)
                        (mapcar 'expand-file-name (org-agenda-files t)))
            (save-buffer)))))

    ;; save all the agenda files after each capture
    (add-hook 'org-capture-after-finalize-hook 'my/save-all-agenda-buffers)

    (use-package org-id
      :config
      (progn
        (setq org-id-link-to-org-use-id t)

        (defun my/org-custom-id-get (&optional pom create prefix)
          "Get the CUSTOM_ID property of the entry at point-or-marker POM.
   If POM is nil, refer to the entry at point. If the entry does
   not have an CUSTOM_ID, the function returns nil. However, when
   CREATE is non nil, create a CUSTOM_ID if none is present
   already. PREFIX will be passed through to `org-id-new'. In any
   case, the CUSTOM_ID of the entry is returned."
          (interactive)
          (org-with-point-at pom
            (let ((id (org-entry-get nil "CUSTOM_ID")))
              (cond
               ((and id (stringp id) (string-match "\\S-" id))
                id)
               (create
                (setq id (org-id-new prefix))
                (org-entry-put pom "CUSTOM_ID" id)
                (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
                id)))))

        (defun my/org-add-ids-to-headlines-in-file ()
          "Add CUSTOM_ID properties to all headlines in the
   current file which do not already have one."
          (interactive)
          (org-map-entries (lambda () (my/org-custom-id-get (point) 'create))))

        ;; automatically add ids to captured headlines
        (add-hook 'org-capture-prepare-finalize-hook
                  (lambda () (my/org-custom-id-get (point) 'create)))))

    (defun my/org-inline-css-hook (exporter)
      "Insert custom inline css to automatically set the
   background of code to whatever theme I'm using's background"
      (when (eq exporter 'html)
        (let* ((my-pre-bg (face-background 'default))
               (my-pre-fg (face-foreground 'default)))
          ;;(setq org-html-head-include-default-style nil)
          (setq
           org-html-head-extra
           (concat
            org-html-head-extra
            (format
             "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
             my-pre-bg my-pre-fg))))))

    (add-hook 'org-export-before-processing-hook #'my/org-inline-css-hook)))

(defun my/org-clock-in ()
  (interactive)
  (org-clock-in '(4)))

(global-set-key (kbd "<f11>") #'my/org-clock-in)
(global-set-key (kbd "<f12>") 'org-clock-out)

(use-package org
  :bind (("C-c C-x C-i" . my/org-clock-in)
         ("C-c C-x C-o" . org-clock-out))
  :config
  (progn
    ;; Insinuate it everywhere
    (org-clock-persistence-insinuate)
    ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
    (setq org-clock-history-length 23
          ;; Resume clocking task on clock-in if the clock is open
          org-clock-in-resume t
          ;; Separate drawers for clocking and logs
          org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "RESULTS" "HIDDEN")
          ;; Save clock data and state changes and notes in the LOGBOOK drawer
          org-clock-into-drawer t
          ;; Sometimes I change tasks I'm clocking quickly -
          ;; this removes clocked tasks with 0:00 duration
          org-clock-out-remove-zero-time-clocks t
          ;; Clock out when moving task to a done state
          org-clock-out-when-done t
          ;; Save the running clock and all clock history when exiting Emacs, load it on startup
          org-clock-persist t
          ;; Prompt to resume an active clock
          org-clock-persist-query-resume t
          ;; Enable auto clock resolution for finding open clocks
          org-clock-auto-clock-resolution #'when-no-clock-is-running
          ;; Include current clocking task in clock reports
          org-clock-report-include-clocking-task t
          ;; don't use pretty things for the clocktable
          org-pretty-entities nil
          ;; some default parameters for the clock report
          org-agenda-clockreport-parameter-plist
          '(:maxlevel 10 :fileskip0 t :score agenda :block thismonth :compact t :narrow 60))))

(use-package org
  :config
  (require 'ox-rss)
  (require 'ox-icalendar)
  (setq org-publish-project-alist
        `(;; Main website at http://writequit.org
          ("writequit-org"
           :base-directory ,(file-truename "~/personal/org/danielnewmandesign/")
           :base-extension "org"
           :publishing-directory "/ssh:writequit.org:~/www/"
           :publishing-function org-html-publish-to-html
           :with-toc nil
           :html-preamble t
           :html-head-extra
           "<link rel=\"alternate\" type=\"application/rss+xml\"
                href=\"http://writequit.org/posts.xml\"
                title=\"RSS feed for writequit.org\">")
          ("writequit-rss"
           :base-directory ,(file-truename  "~/org/writequit")
           :base-extension "org"
           :publishing-directory "/ssh:writequit.org:~/www/"
           :publishing-function org-rss-publish-to-rss
           :html-link-home "http://writequit.org/"
           :exclude ".*"
           :include ("posts.org")
           :html-link-use-abs-url t)
          ("writequit-images"
           :base-directory ,(file-truename  "~/org/writequit/images")
           :base-extension "png\\|jpg\\|gif"
           :publishing-directory "/ssh:writequit.org:~/www/images"
           :publishing-function org-publish-attachment)
          ("writequit-files"
           :base-directory ,(file-truename  "~/org/writequit/files")
           :base-extension "*"
           :publishing-directory "/ssh:writequit.org:~/www/files/"
           :publishing-function org-publish-attachment)
          ("writequit" :components ("writequit-org"
                                    "writequit-images"
                                    "writequit-files"
                                    "writequit-rss"))

          ;; Denver emacs site
          ("denver-emacs"
           :base-directory ,(file-truename "~/org/denver-emacs-meetup/")
           :base-extension "org\\|html"
           :publishing-directory
           "/ssh:writequit.org:~/www/denver-emacs"
           :publishing-function org-html-publish-to-html
           :with-toc nil
           :html-preamble t)

          ;; Org-mode files for ~/.emacs.d/settings.org
          ("dotfiles"
           :base-directory ,(file-truename "~/.emacs.d/../")
           :base-extension "org\\|html"
           :publishing-directory
           "/ssh:writequit.org:~/www/org/"
           :publishing-function org-html-publish-to-html
           :with-toc t
           :html-preamble t)

          ;; Org-mode files for ~/org files
          ("org-org"
           :base-directory ,(file-truename "~/org/")
           :base-extension "org\\|html"
           :publishing-directory
           "/ssh:writequit.org:~/www/org/"
           :publishing-function org-html-publish-to-html
           :with-toc t
           :html-preamble t)
          ("org-images"
           :base-directory ,(file-truename "~/org/images")
           :base-extension "png\\|jpg"
           :publishing-directory
           "/ssh:writequit.org:~/www/org/images"
           :publishing-function org-publish-attachment)
          ("org" :components ("org-org" "org-images"))

          ;; Org-mode for the ~/org/es files
          ("org-es-org"
           :base-directory ,(file-truename "~/org/es/")
           :base-extension "org\\|html"
           :publishing-directory
           "/ssh:writequit.org:~/www/org/es"
           :publishing-function org-html-publish-to-html
           :with-toc t
           :html-preamble t)
          ("org-es-files"
           :base-directory ,(file-truename "~/org/es/")
           :base-extension "css\\|pdf\\|sh\\|es\\|zsh\\|py\\|org"
           :publishing-directory
           "/ssh:writequit.org:~/www/org/es"
           :publishing-function org-publish-attachment)
          ("org-es-images"
           :base-directory ,(file-truename "~/org/es/images")
           :base-extension "png\\|jpg"
           :publishing-directory
           "/ssh:writequit.org:~/www/org/es/images"
           :publishing-function org-publish-attachment)
          ("org-es"
           :components ("org-es-org" "org-es-files" "org-es-images"))

          ;; Org-mode for the ~/org/es/design files
          ("org-es-design-org"
           :base-directory ,(file-truename "~/org/es/design")
           :base-extension "org\\|html"
           :publishing-directory
           "/ssh:writequit.org:~/www/org/es/design"
           :publishing-function org-html-publish-to-html
           :with-toc t
           :html-preamble t)
          ("org-es-design-files"
           :base-directory ,(file-truename "~/org/es/design")
           :base-extension "css\\|pdf\\|sh\\|es\\|zsh\\|py\\|org"
           :publishing-directory
           "/ssh:writequit.org:~/www/org/es/design"
           :publishing-function org-publish-attachment)
          ("org-es-designs-images"
           :base-directory ,(file-truename "~/org/es/design/images")
           :base-extension "png\\|jpg"
           :publishing-directory
           "/ssh:writequit.org:~/www/org/es/design/images"
           :publishing-function org-publish-attachment)
          ("org-es-design"
           :components ("org-es-design-org"
                        "org-es-design-files"
                        "org-es-design-images")))))

(use-package org-alert
  :disabled t
  :init (org-alert-enable))

(use-package org-present
  :defer 20
  :init
  (add-hook 'org-present-mode-hook
            (lambda ()
              (org-present-big)
              (org-display-inline-images)
              (org-present-hide-cursor)
              (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
            (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

(use-package alert
  :config
  (when (eq system-type 'darwin)
    (setq alert-default-style 'notifier))
  (when (eq system-type 'gnu/linux)
    (setq alert-default-style 'notifications))

  (defun my/compilation-finish (buffer msg)
    (interactive)
    (alert (format "[%s]: %s" buffer msg)))
  (add-to-list 'compilation-finish-functions #'my/compilation-finish)

  (defun finish ()
    (interactive)
    (alert (concat "Finished shell command in " (buffer-name))
           :severity 'high
           :category 'eshell
           :title (buffer-name)
           :persistent t)))

(provide 'init-org)
