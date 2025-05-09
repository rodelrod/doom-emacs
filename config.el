;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Rodrigo Eliseu"
      user-mail-address "rodrigo@eliseu.me")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Source Code Pro" :size 14)
      doom-variable-pitch-font (font-spec :family "Source Sans 3"))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-opera)
;; (setq doom-theme 'doom-nord-light)


;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.



;;
;;; Global Settings


(setq delete-by-moving-to-trash t
      auto-save-default t)

;; System locale to use for formatting time values.  Make sure that the weekdays
;; in the time stamps of your Org mode files and in the agenda appear in
;; English.
(setq system-time-locale "C")

;;
;;; Utils


(defun rodelrod/regex-replace-at-point (source-regex replacement)
  "Convert string matching source-regex under or behind cursor.
Only looks in the current line and replaces the closest match."
  (save-excursion
    (while (and
            (>= (point) (line-beginning-position))
            (not (looking-at source-regex)))
      (backward-word))
    (replace-match replacement t)))

(defun rodelrod/convert-evernote-datetime-at-point-to-org-timestamp ()
  "Convert Evernote datetime under or behind cursor with an org inactive timestamp.
Only looks in the current line and replaces the closest match."
  (interactive)
  (let ((evernote-datetime-regex
         "\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{4\\}\\) \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\( --\\)?")
        (org-mode-datetime-replacement
         "[\\3-\\2-\\1 \\4]"))
    (rodelrod/regex-replace-at-point evernote-datetime-regex org-mode-datetime-replacement)))

(defun rodelrod/convert-evernote-date-at-point-to-org-timestamp ()
  "Convert Evernote date under or behind cursor with an org inactive timestamp.
Only looks in the current line and replaces the closest match."
  (interactive)
  (let ((evernote-date-regex
         "\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{4\\}\\) \\( --\\)?")
        (org-mode-date-replacement
         "[\\3-\\2-\\1]"))
    (rodelrod/regex-replace-at-point evernote-date-regex org-mode-date-replacement)))

(defun rodelrod/regex-replace-all (source-regex replacement)
  "Convert string matching source-regex in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward source-regex nil t)
      (replace-match replacement t))))

(defun rodelrod/convert-all-evernote-dates-to-org-timestamp ()
  "Convert all Evernote formatted dates and datetimes in the current buffer
 to org-mode inactive timestamps"
  (interactive)
  (let ((evernote-datetime-regex
         "\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{4\\}\\) \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\( --\\)?")
        (org-mode-datetime-replacement
         "[\\3-\\2-\\1 \\4]")
        (evernote-date-regex
         "\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{4\\}\\) \\( --\\)?")
        (org-mode-date-replacement
         "[\\3-\\2-\\1]"))
    (rodelrod/regex-replace-all evernote-datetime-regex org-mode-datetime-replacement)
    (rodelrod/regex-replace-all evernote-date-regex org-mode-date-replacement)))

(defun rodelrod/json-timestamp-to-iso ()
  "Echo the ISO data time version of the json timestamp under the cursor
or selected.
Assumes millisecond timestamps."
  (interactive)
  (let (timestamp)
    (setq timestamp (string-to-number (doom-thing-at-point-or-region 'word)))
    (message
     (format-time-string "%Y-%m-%d %H:%M:%S"
                         (seconds-to-time (/ timestamp 1000))))))

(defun rodelrod/convert-all-dbg-jira-to-link ()
  "Convert all strings that look like DBG JIRA tickets to links"
  (interactive)
  (let ((jira-projects '("PID-2000" "SBD-" "CFCCON-"))
        (dbg-jira-replacement
         "\\1[[https://jiradbg.deutsche-boerse.de/browse/\\2][\\2]]"))
    (dolist (jira-project jira-projects)
      (let ((dbg-jira-regex
             (rx
              (group (or (not (any "[" "/")) bol))
              (group (regexp jira-project) (repeat 3 6 digit)))))
        (rodelrod/regex-replace-all dbg-jira-regex dbg-jira-replacement)))))

;; From https://ivanaf.com/emacs_drag-drop_pdfs_paste_html_custom_templates.html
(defun html2org-clipboard ()
  "Convert clipboard contents from HTML to Org and then paste (yank)."
  (interactive)
  (let* (
         (text_html (gui-backend-get-selection 'PRIMARY 'text/html))
         (text_raw (gui-get-selection))
         (text_html (when text_html
                      (decode-coding-string text_html 'unix)))
         (text_html (when text_html
                      (shell-command-to-string (concat "echo "  (shell-quote-argument text_html) "|timeout 2  pandoc -f html-native_divs-native_spans -t org"))))
         (text (or text_html
                   text_raw))
         )
    (progn  (kill-new text) (yank))))



;;
;;; Global Keybindings


(defun rodelrod/insert-timestamp ()
  "Insert current date time formatted like an org inactive timestamp."
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a %H:%M]")))
(map! :desc "Insert inactive timestamp" :nvie "C-'" 'rodelrod/insert-timestamp)

(map! :leader
      :prefix "t"
      :desc "Highlight current line" "h" #'hl-line-mode)

(map! :desc "Use extra mouse button to go back" [mouse-8] #'previous-buffer
      :desc "Use extra mouse button to go forward" [mouse-9] #'next-buffer)

(map! :desc "Avy jump in this window" "C-;" #'evil-avy-goto-char-timer)
(setq avy-single-candidate-jump t)

;; Mimic my neotree keybindings on vim for the sake of muscle memory
(map! :map evil-window-map
      :desc "Toggle treemacs" "e" #'+treemacs/toggle
      :desc "Toggle treemacs" "C-e" #'+treemacs/toggle)

;; Move workspaces around with keybindings mirroring what I use in tmux
(map! :leader
      :prefix "TAB"
      :desc "Swap workspace to the left" "H" #'+workspace/swap-left
      :desc "Swap workspace to the right" "L" #'+workspace/swap-right)



;;
;;; Package Configuration


(after! blacken
  (setq blacken-skip-string-normalization t))


(use-package! org
  :mode "\\.org_archive\\'"
  :init
  (setq org-directory (file-truename "~/Org/notes/"))
  (add-hook! 'org-mode-hook #'doom-disable-line-numbers-h)
  (add-hook! 'org-mode-hook #'+org-pretty-mode)
  :config
  (setq org-archive-location "ARCHIVE/%s_archive::"
        org-download-image-org-width 400
        org-ellipsis " ▶"
        org-enforce-todo-dependencies t
        org-list-indent-offset 1                      ; indent plain lists with 3 spaces
        org-log-done t                                ; Add CLOSED timestamp when todo is done
        org-log-into-drawer t                         ; Put log notes (C-c C-z) and state changes in LOGBOOK drawer.
        org-modern-star 'replace
        org-plantuml-jar-path  "/usr/share/plantuml/plantuml.jar"
        org-reverse-note-order t
        org-startup-folded 'content
        org-tag-alist '(("TASKS")("PROJECT")("paused"))         ; Can't use single-key shortcuts because broken in Doom AFAICT
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c@)"))
        tab-width 8      ; 8 is insane but necessary since Org 9.7 (see https://lists.gnu.org/archive/html/emacs-orgmode/2023-12/msg00284.html)
        )

  ;; Use the fold prefix `z' for outline navigation
  ;; and recover `gk' and `gj' for visual line up and down
  (map! :map org-mode-map

        :prefix "z"
        :n "j" #'org-forward-heading-same-level
        :n "k" #'org-backward-heading-same-level
        :n "h" #'org-up-element

        :prefix "g"
        :n "j" #'evil-next-visual-line
        :n "k" #'evil-previous-visual-line)

  ;; Link to Trello card using the hash (e.g. [[trello:pkJCXtRX]])
  (add-to-list 'org-link-abbrev-alist '("trello" . "https://trello.com/c/"))

  ;; Emacs wants to open everything in emacs, but many times I just want
  ;; the system default.
  (setq org-file-apps
        '((auto-mode . emacs)
          (directory . emacs)
          ;; `setsid' is needed to keep the child process alive when xdg-open
          ;; hands over to the default application that actually opens the file
          (system . "setsid -w xdg-open %s")
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . default)
          ("\\.png\\'" . system)
          ("\\.jpg\\'" . system)
          ("\\.svg\\'" . system)
          ("\\.xslx\\'" . system)
          ))

  ;; Export to an `./exports' directory to prevent cluttering the main file and
  ;; allow to easily exclude from git.
  ;; Adapted from https://stackoverflow.com/a/47850858/20989
  (defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
    (unless pub-dir
      (setq pub-dir "exports")
      (unless (file-directory-p pub-dir)
        (make-directory pub-dir)))
    (apply orig-fun extension subtreep pub-dir nil))
  (advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)

  ;; Move a sub-tree to the top or bottom of its parent
  ;; Copied from a John Kitchin's SO answer: https://emacs.stackexchange.com/a/43662
  (defun JK-org-move-to-extreme (up)
    "Move current org subtree to the end of its parent.
     With prefix arg move subtree to the start of its parent."
    (interactive "P")
    (condition-case err
        (while t
          (funcall (if up
                       'org-move-subtree-up
                     'org-move-subtree-down)))
      (user-error
       (let ((err-msg (cadr err)))
         (unless (string-match "Cannot move past superior level or buffer limit" err-msg)
           (signal 'user-error (list err-msg)))))))

  (defun JK-org-move-to-top ()
    "Move sub-tree to top of parent"
    (interactive)
    (setq current-prefix-arg 4) ; C-u
    (call-interactively 'JK-org-move-to-extreme))
  (defun JK-org-move-to-bottom ()
    "Move sub-tree to bottom of parent"
    (interactive)
    (call-interactively 'JK-org-move-to-extreme))
  ;; Bind org move to top/bottom keys
  (map! :map org-mode-map
        :prefix "z"
        :n "J" #'JK-org-move-to-bottom
        :n "K" #'JK-org-move-to-top)

  (defun rodelrod/org-open-link-split ()
    "Open link in an horizontal split"
    (interactive)
    (+evil/window-split-and-follow)
    (org-open-at-point))
  (defun rodelrod/org-open-link-vsplit ()
    "Open link in a vertical split"
    (interactive)
    (+evil/window-vsplit-and-follow)
    (org-open-at-point))
  (map! :map org-mode-map
        :leader
        :desc "Open link in split" :n "o s" #'rodelrod/org-open-link-split
        :desc "Open link in vertical split" :n "o v" #'rodelrod/org-open-link-vsplit)

  ;;
  ;; Weekly Reviews
  ;; --------------
  ;; Create a weekly review entry in a week datetree based on a template.
  (require 'f)
  (require 'org-datetree)
  (defun rodelrod/org-datetree-find-iso-week-create (d &optional template-file)
    "Find or create an ISO week entry for date D.
Compared to `org-datetree-find-iso-week-create' this function does not create a
day entry under the week. Only headers for the year and the week are created.
A TEMPLATE-FILE can be read to insert text after the week heading when
and only when it is first created."
    (goto-char (point-min))
    (require 'cal-iso)
    (let* ((year (calendar-extract-year d))
           (month (calendar-extract-month d))
           (day (calendar-extract-day d))
           (time (encode-time 0 0 0 day month year))
           (iso-date (calendar-iso-from-absolute
                      (calendar-absolute-from-gregorian d)))
           (weekyear (nth 2 iso-date))
           (week (nth 0 iso-date)))
      ;; ISO 8601 week format is %G-W%V(-%u)
      (org-datetree--find-create
       "^\\*+[ \t]+\\([12][0-9]\\{3\\}\\)\\(\\s-*?\
\\([ \t]:[[:alnum:]:_@#%%]+:\\)?\\s-*$\\)"
       weekyear nil nil
       (format-time-string "%G" time))
      (org-datetree--find-create
       "^\\*+[ \t]+%d-W\\([0-5][0-9]\\)$"
       weekyear week nil
       (concat
        (format-time-string "%G-W%V" time)
        "\n"
        (f-read-text template-file)))))

  (defun rodelrod/go-to-weekly-review ()
    "Visit weekly review file in headline for current week.  Headline is created
if does not exist, inserting the contents of the template file"
    (interactive)
    (let ((reviews-file  "~/Org/notes/fleeting/weekly_reviews.org")
          (template-file  "~/Org/templates/weekly_review.org"))
      (find-file reviews-file)
      (rodelrod/org-datetree-find-iso-week-create (calendar-current-date) template-file)))

  (map! :map org-mode-map
        :leader
        :prefix "n"
        :desc "Go to weekly review" "w" #'rodelrod/go-to-weekly-review)

  ;; Stuck project definition
  (add-to-list 'org-tags-exclude-from-inheritance "PROJECT")
  (setq org-stuck-projects
        '("+PROJECT-paused/-SOMEDAY-DONE-CANCELLED" ("NEXT") nil "SCHEDULED:")))


(after! org-agenda
  (setq org-agenda-files (mapcar 'file-truename
                                 '("~/Org/notes/inbox"
                                   "~/Org/notes/area"
                                   "~/Org/notes/project"
                                   "~/Org/notes/client/dbg/area"
                                   "~/Org/notes/client/dbg/project"
                                   "~/Org/notes/client/dbg/meeting"
                                   "~/Org/notes/client/magicfil/area"
                                   "~/Org/notes/client/magicfil/project"
                                   )))
  (setq org-agenda-custom-commands
        '(("n" "Agenda, NEXT, and WAITING"
           ((agenda "" nil)
            (org-ql-block '(and (todo "NEXT")
                                (tags "dbg")
                                (not (scheduled)))
                          ((org-ql-block-header "DBG next unscheduled tasks")))
            (org-ql-block '(and (todo "NEXT")
                                (not (tags "dbg"))
                                (not (scheduled)))
                          ((org-ql-block-header "Other next unscheduled tasks")))
            (org-ql-block '(todo "WAITING")
                          ((org-ql-block-header "Waiting")))))
          ("o" "Other TODOs: TODO and SOMEDAY"
           ((agenda "" nil)
            (org-ql-block '(and (todo "TODO")
                                (tags "dbg")
                                (not (scheduled)))
                          ((org-ql-block-header "DBG unscheduled TODOs")))
            (org-ql-block '(and (todo "SOMEDAY")
                                (tags "dbg")
                                (not (scheduled)))
                          ((org-ql-block-header "DBG unscheduled SOMEDAYs")))
            (org-ql-block '(and (todo "TODO")
                                (not (tags "dbg"))
                                (not (scheduled)))
                          ((org-ql-block-header "Other unscheduled TODOs")))
            (org-ql-block '(and (todo "SOMEDAY")
                                (not (tags "dbg"))
                                (not (scheduled)))
                          ((org-ql-block-header "Other unscheduled SOMEDAYs")))))
          ;; Last week entries sorted roughly by from latest to earliest (it's
          ;; hard to sort by creation date, which is what I wanted).
          ("l"  "Entries created last week"
           tags "+TIMESTAMP_IA>\"<-1w>\""
           ((org-agenda-sorting-strategy '(tsia-down timestamp-down))))

          ;; Allow me to archive old items for area notes only. I want to
          ;; archive project notes in one go instead of having the tasks
          ;; scattered in the Archive datetree.
          ("r" "Area tasks ready to archive"
           tags "TASKS+CLOSED<\"<-3y>\""
           ;; Only archive TODOs from the area files, not project etc.
           ((org-agenda-files (mapcar 'file-truename
                                      '("~/Org/notes/area"
                                        "~/Org/notes/client/dbg/area"
                                        "~/Org/notes/client/magicfil/area"
                                        )))))))
  ;; HACK fix evil keybindings in org-ql
  ;;      (see https://github.com/alphapapa/org-super-agenda/issues/50)
  (setq org-super-agenda-header-map (make-sparse-keymap))

  ;; HACK show category in org-ql-block
  ;;      (see https://github.com/alphapapa/org-ql/issues/23#issuecomment-1063345875)
  (defun rodelrod/org-ql-view--format-element (orig-fun &rest args)
    "This function will intercept the original function and
   add the category to the result.

   ARGS is `element' in `org-ql-view--format-element'"
    (if (not args)
        ""
      (let* ((element args)
             (properties (cadar element))
             (result (apply orig-fun element))
             (category (org-entry-get (plist-get properties :org-marker) "CATEGORY")))
        (org-add-props
            (format "   %-10s %s" (concat category ":") result)
            (text-properties-at 0 result)))))
  (advice-add 'org-ql-view--format-element :around #'rodelrod/org-ql-view--format-element)

  ;; Function used to launch agenda on emacs client startup
  (defun org-agenda-show-n (&optional arg)
    "Launch agenda in its own buffer"
    ;; I don't really know how this works, most of the code comes
    ;; from +org-capture/open-frame
    (interactive "P")
    (let* ((frame-title-format "")
           (frame (selected-frame)))
      (with-selected-frame frame
        (condition-case ex
            (letf! ((#'pop-to-buffer #'switch-to-buffer))
              (switch-to-buffer (doom-fallback-buffer))
              (org-agenda arg "n"))
          ('error
           (message "org-agenda: %s" (error-message-string ex))
           (delete-frame frame)))))))


(after! org-capture
  (setq org-default-notes-file "inbox/inbox.org")
  ;; Get into insert state immediately after entering Capture
  ;;(add-hook 'org-capture-mode-hook 'evil-insert-state)
  (setq org-capture-templates
        ;; Looking for the meaning of the '%' placeholders?
        ;; Check https://orgmode.org/manual/Template-expansion.html#Template-expansion
        '(("t" "todo" entry
           (file "inbox/inbox.org")
           "* TODO %?\n%U\n")
          ("T" "todo Panzer" entry
           (file "inbox/inbox-panzer.org")
           "* TODO %?\n%U\n")
          ("l" "todo with link" entry
           (file "inbox/inbox.org")
           "* TODO %?\n%U\n%i\n%a\n")
          ("n" "note wih link" entry
           (file "inbox/inbox.org")
           "* %?\n%U\n%i\n%a\n")
          ("p" "Protocol Snippet" entry
           (file "inbox/inbox.org")
           "* %a\n%U\n\n#+begin_quote\n%i\n#+end_quote\n\n%?"
           :empty-lines 1
           :immediate-finish t)
          ("L" "Protocol Link" entry
           (file "inbox/inbox.org")
           "* %a\n%U"
           :empty-lines 1
           :immediate-finish t))))


(after! (org-agenda org-capture)
  ;; Auto-save all org files on some org-agenda commands (feel free to add)
  ;; based on https://emacs.stackexchange.com/a/7840 and https://emacs.stackexchange.com/a/489
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
  (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers)
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (add-hook 'org-capture-after-finalize-hook 'org-save-all-org-buffers))


(use-package! org-roam
  :after org

  :init
  (setq org-roam-directory (file-truename "~/Org/notes")
        org-roam-file-exclude-regexp "weekly_reviews.org"
        org-roam-db-location "~/.local/cache/org-roam/org-roam.db"
        org-roam-graph-exclude-matcher '("fleeting/" "inbox/")
        org-roam-graph-link-hidden-types '("file" "http" "https" "attachment" "fuzzy")
        +org-roam-open-buffer-on-find-file nil)

  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags)))))

  :config
  (setq org-roam-node-display-template "${title:*} ${tags:32}")

  ;; HACK Doom introduced an optimization to use fd instead of find.
  ;;      However fd will exclude files in .gitignore by default.
  ;;      We can try to fix the calls to fd by adding the -I option.
  ;;      Or we can change the order to use find instead.
  ;;      This is only necessary because I have a dbg directory that
  ;;      I do not want to track in the main git repo.
  (setq org-roam-list-files-commands '(find fd fdfind rg))

  (defun rodelrod/org-roam-db-clear-and-sync ()
    "Clear and rebuild org-roam database"
    (interactive)
    (org-roam-db-clear-all)
    (org-roam-db-sync))

  (map! :leader
        :prefix "n"
        :desc "Insert org-roam link" "i" #'org-roam-node-insert
        :desc "Find in org-roam notes" "f" #'org-roam-node-find
        :desc "Show org-roam Graph" "g" #'org-roam-graph
        :desc "Add roam files to org-id to fix export" "I" #'org-roam-update-org-id-locations
        :desc "Clear and rebuild org-roam DB" "R" #'rodelrod/org-roam-db-clear-and-sync)

  (setq org-roam-capture-templates
        '(("t" "topic" plain "%?" :if-new
           (file+head "topic/${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :topic:\n")
           :unnarrowed t)
          ("l" "literature" plain "%?" :if-new
           (file+head "literature/%<%Y%m>-${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :literature:\n")
           :unnarrowed t)
          ("p" "DBG project" plain "%?" :if-new
           (file+head "client/dbg/project/%<%Y%m>-${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :project:dbg:\n")
           :unnarrowed t)
          ("P" "project" plain "%?" :if-new
           (file+head "project/%<%Y%m>-${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :project:\n")
           :unnarrowed t)
          ("a" "DBG area" plain "%?" :if-new
           (file+head "client/dbg/area/${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :area:dbg:\n")
           :unnarrowed t)
          ("A" "area" plain "%?" :if-new
           (file+head "area/${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :area:\n")
           :unnarrowed t)
          ("m" "DBG recurring meeting" plain "%?" :if-new
           (file+head "client/dbg/meeting/%<%Y%m>-${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :meeting:dbg:\n#+startup: overview\n")
           :unnarrowed t)
          ("M" "recurring meeting" plain "%?" :if-new
           (file+head "meeting/%<%Y%m>-${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :meeting:\n#+startup: overview\n")
           :unnarrowed t)
          ("w" "who" plain "%?" :if-new
           (file+head "who/${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :who:\n")
           :unnarrowed t)
          ))

  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer)))

  (use-package! websocket
    :after org-roam)

  (use-package! org-roam-ui
    :after org-roam ;; or :after org
    ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
    ;;         a hookable mode anymore, you're advised to pick something yourself
    ;;         if you don't care about startup time, use
    ;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

  ;; ARCHIVING
  ;; ------------------------------------
  ;; Archiving/unarchiving org-roam files

  (defun rodelrod/archive-org-roam-file ()
    "Move an org-roam file to an ARCHIVE sub-directory and add ARCHIVE tag."
    (if (not buffer-file-name)
        (message "Buffer '%s' is not visiting a file!" (buffer-name))
      (let ((roam-directory (f-dirname buffer-file-name))
            (roam-basename  (f-filename buffer-file-name)))

        (when (not (member "ARCHIVE" (f-split roam-directory)))
          (org-roam-tag-add '("ARCHIVE"))
          (save-buffer)
          (let ((archive-directory (f-expand (file-name-as-directory "ARCHIVE") roam-directory)))
            (f-mkdir archive-directory)
            (let ((archive-filename (f-expand roam-basename archive-directory)))
              (f-move buffer-file-name archive-filename)
              (kill-buffer)
              (find-file archive-filename)))))))

  (defun rodelrod/unarchive-org-roam-file ()
    "Move an org-roam file out of the ARCHIVE sub-directory and remove ARCHIVE tag."
    (if (not buffer-file-name)
        (message "Buffer '%s' is not visiting a file!" (buffer-name))
      (let ((archive-directory (f-dirname buffer-file-name))
            (archive-basename  (f-filename buffer-file-name)))

        (when (member "ARCHIVE" (f-split archive-directory))
          (org-roam-tag-remove '("ARCHIVE"))
          (save-buffer)
          (let ((roam-directory (f-parent archive-directory)))
            (let ((roam-filename (f-expand archive-basename roam-directory)))
              (f-move buffer-file-name roam-filename)
              (kill-buffer)
              (find-file roam-filename)))))))

  (defun rodelrod/toggle-archive-org-roam-file ()
    "Archive and unarchive org-roam file according to current state."
    (interactive)
    (if (not buffer-file-name)
        (message "Buffer '%s' is not visiting a file!" (buffer-name))
      (let ((current-file-directory (f-dirname buffer-file-name)))
        (if (member "ARCHIVE" (f-split current-file-directory))
            (rodelrod/unarchive-org-roam-file)
          (rodelrod/archive-org-roam-file)))))

  (map! :leader
        :prefix "n"
        :desc "Toggle archive org-roam file" "A" #'rodelrod/toggle-archive-org-roam-file))


(use-package! org-who
  :after org)


;; (after! ivy-bibtex
;;   (setq bibtex-completion-bibliography "~/Org/resources/Zotero.bib"
;;         bibtex-completion-library-path "~/Dropbox/Zotero"
;;         bibtex-completion-pdf-field "File"
;;         bibtex-completion-notes-path "~/Org/notes/literature"
;;         bibtex-completion-notes-template-multiple-files
;;         (concat
;;          "#+TITLE: ${title}\n"
;;          "#+ROAM_KEY: cite:${=key=}\n"
;;          "* TODO Notes\n"
;;          ":PROPERTIES:\n"
;;          ":Custom_ID: ${=key=}\n"
;;          ":AUTHOR: ${author-abbrev}\n"
;;          ":JOURNAL: ${journaltitle}\n"
;;          ":DATE: ${date}\n"
;;          ":YEAR: ${year}\n"
;;          ":DOI: ${doi}\n"
;;          ":URL: ${url}\n"
;;          ":END:\n\n"
;;          )
;;         ))



(after! persp-mode
  ;; Do not create new workspace when opening emacsclient
  (setq persp-emacsclient-init-frame-behaviour-override "main"))


;; (after! rustic
;;   (setq rustic-flycheck-clippy-params "--message-format=json"))


(after! python
  (map! :localleader
        (:prefix ("o" . "Open")
         :desc "Python REPL" "r" #'+python/open-repl
         :desc "iPython" "i" #'+python/open-ipython-repl
         :desc "Jupyter" "j" #'+python/open-jupyter-repl)
        (:prefix ("s" . "Send to REPL")
         :desc "Send buffer to REPL" "b" #'python-shell-send-buffer
         :desc "Send file to REPL" "f" #'python-shell-send-file
         :desc "Send region to REPL" "r" #'python-shell-send-region)))


(after! tramp
  ;; Prevent tramp from prompting passphrase for authinfo.gpg, since I don't use
  ;; that for ssh connections.
  (setq tramp-completion-use-auth-sources nil))

(after! treemacs
  (defun treemacs-custom-filter (filename _)
    (or (s-equals? "__pycache__" filename)
        (s-ends-with? ".pyc" filename)))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-custom-filter))


(after! whitespace

  (defun rodelrod/toggle-show-all-whitespace ()
    "Toggle showing all whitespace as visible characters.
Toggles between Doom's very bare default style and a more complete style
showing almost everything."
    (interactive)
    (let* ((doom-style
            '(face tabs tab-mark))
           (full-style
            '(face indentation tabs tab-mark spaces space-mark newline newline-mark trailing lines-tail))
           (current-style-is-full?
            (not (cl-set-exclusive-or (symbol-value 'whitespace-style) full-style))))
      (if current-style-is-full?
          (progn (setq whitespace-style doom-style)
                 (message "Whitespace mode set to Doom's default"))
        (progn (setq whitespace-style full-style)
               (message "Whitespace mode set to show everything"))))
    (whitespace-mode 0)
    (whitespace-mode 1))

  (map! :leader
        :prefix "t"
        :desc "Show all whitespace" "s" #'rodelrod/toggle-show-all-whitespace))


(use-package! writeroom-mode
  :config
  (setq +zen-text-scale 1)  ; default 2 was a bit too big
  (defun rodelrod/switch-off-hl-line ()
    "Deactivate highlighting current line entering writeroom mode
and reactivate upon exit"
    (hl-line-mode (if writeroom-mode -1 +1)))
  (add-hook 'writeroom-mode-hook #'rodelrod/switch-off-hl-line))
