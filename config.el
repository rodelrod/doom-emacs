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

;; Bigger headings for org-mode, markdown and any other outliney things
(custom-set-faces!
  '(org-document-title :height 1.35)
  '(outline-1 :weight bold :height 1.30)
  '(outline-2 :weight bold :height 1.25)
  '(outline-3 :weight bold :height 1.20)
  '(outline-4 :weight semi-bold :height 1.15)
  '(outline-5 :weight semi-bold :height 1.12)
  '(outline-6 :weight semi-bold :height 1.09)
  '(outline-8 :weight semi-bold :height 1.06)
  '(outline-9 :weight semi-bold)
  `(org-drawer :foreground ,(doom-blend 'comments 'bg 0.7))
  `(org-date :foreground ,(doom-blend 'yellow 'bg 0.8))
  ;; '(org-ellipsis :distant-foreground "dim gray")
  )

;; HACK Emacs cannot distinguish C-i from TAB, which is disturbing. Instead,
;;      let's at least make GUI Emacs aware of this distinction:
;; (This code was committed (2020-05-11) then reverted (2020-05-13) from
;;  Doom Emacs. I need it to be able to jump back in Org files.)
(define-key key-translation-map [?\C-i]
  (λ! (if (and (not (cl-position 'tab    (this-single-command-raw-keys)))
               (not (cl-position 'kp-tab (this-single-command-raw-keys)))
               (display-graphic-p))
          [C-i] [?\C-i])))
(map! :g [C-i] #'evil-jump-forward)



;;
;;; Utils


(defun rodelrod/evernote-date-to-org-timestamp ()
  "Convert Evernote date under or behind cursor with an org inactive timestamp.
Only looks in the current line and replaces the closest match."
  (interactive)
  (let ((evernote-data-regex
         "\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{4\\}\\) \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\( --\\)?")
        (org-mode-replacement
         "[\\3-\\2-\\1 \\4]"))
    (save-excursion
      (while (and
              (>= (point) (line-beginning-position))
              (not (looking-at evernote-data-regex)))
        (backward-word))
      (replace-match org-mode-replacement))))

(defun rodelrod/json-timestamp-to-iso ()
  "Echo the ISO data time version of the json timestamp under the cursor or selected.
Assumes millisecond timestamps."
  (interactive)
  (let (timestamp)
    (setq timestamp (string-to-number (doom-thing-at-point-or-region 'word)))
    (message
     (format-time-string "%Y-%m-%d %H:%M:%S"
                         (seconds-to-time (/ timestamp 1000))))))

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
  (setq org-directory (file-truename "~/org/notes/"))
  (add-hook! 'org-mode-hook #'(+org-pretty-mode
                               doom-disable-line-numbers-h))
  :config
  (setq org-archive-location "ARCHIVE/%s_archive::datetree/"
        org-download-image-org-width 400
        org-ellipsis " ▶"
        org-enforce-todo-dependencies t
        org-id-link-to-org-use-id 'create-if-interactive
        org-list-indent-offset 1                      ; indent plain lists with 3 spaces
        org-log-done t                                ; Add CLOSED timestamp when todo is done
        org-log-into-drawer t                         ; Put log notes (C-c C-z) and state changes in LOGBOOK drawer.
        org-plantuml-jar-path  "/usr/share/plantuml/plantuml.jar"
        org-reverse-note-order t
        org-startup-folded 'content
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c@)"))
        tab-width 3
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

  (defun rodelrod/convert-evernote-dates-to-org-timestamp ()
    "Convert all Evernote formatted dates in the current buffer to org-mode inactive timestamps"
    (interactive)
    (let ((evernote_data_regex
           "\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{4\\}\\) \\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\( --\\)?")
          (org_mode_replacement
           "[\\3-\\2-\\1 \\4]"))
      (goto-char (point-min))
      (while (re-search-forward evernote_data_regex nil t)
        (replace-match org_mode_replacement))))


  ;;
  ;; ProjectState Property
  ;; ---
  ;; The ProjectState property is set automatically on every heading that has a
  ;; statistics cookie.

  ;; Set ACTIVE and MUTED as allowed values for ProjectState
  (defun org-property-set-allowed-project-states (property)
    "Set allowed valued for the ProjectState property."
    (when (equal property "ProjectState") '("ACTIVE" "MUTED")))
  (add-hook 'org-property-allowed-value-functions 'org-property-set-allowed-project-states)

  ;;  Set ProjectState automatically to:
  ;;  - ACTIVE: if the heading contains at least one subtask to be done
  ;;  - MUTED: if there's no subtask to be done
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to ~n-done~ when all subentries are done, to ~n-not-done~ otherwise."
    ;; HACK: We want to shadow a couple of variables to turn off logging below,
    ;; but since lexical binding is on in this file, we need to force dynamic
    ;; binding using defvar.
    (defvar org-log-done)
    (defvar org-log-states)
    (let (org-log-done org-log-states)   ; turn off logging
      (org-set-property "ProjectState" (if (= n-not-done 0) "MUTED" "ACTIVE")))
    (ignore n-done))   ; reassure the bytecomp that we know we're not using the variable
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; List as stuck project if ProjectState property is ACTIVE but it has no
  ;; sub-task marked as NEXT; except if project is marked as a SOMEDAY, DONE
  ;; or CANCELLED todo item.
  (setq org-stuck-projects
        '("+ProjectState=\"ACTIVE\"/-SOMEDAY-DONE-CANCELLED" ("NEXT") nil "")))


(after! org-agenda
  (setq org-agenda-files (mapcar 'file-truename
                                 '("~/org/notes/tasks" "~/org/notes/project" "~/org/notes/area")))
  (setq org-agenda-custom-commands
        '(("n" "Agenda, NEXT, and other TODOs"
           ((agenda "" nil)
            (todo "NEXT"
                  ((org-agenda-overriding-header "Unscheduled NEXT items:")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
            (todo "WAITING" nil)
            )
           nil)

          ;; Last week entries sorted roughly by from latest to earliest (it's
          ;; hard to sort by creation date, which is what I wanted).
          ("l"  "Entries created last week"
           tags "+TIMESTAMP_IA>\"<-1w>\""
           ((org-agenda-sorting-strategy '(tsia-down timestamp-down))))

          ;; Allow me to archive old items for area notes only. I want to
          ;; archive project notes in one go instead of having the tasks
          ;; scattered in the Archive datetree.
          ("r" "Area tasks ready to archive"
           tags "TASKS+CLOSED<\"<-2m>\""
           ;; Only archive TODOs from the area files, not project etc.
           ((org-agenda-files (list (file-truename "~/org/notes/area")))))))

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
           (delete-frame frame))))))
  (setq org-refile-targets
        ;; Refile only to dedicated tasks lists and not to any random heading (which can contain notes)
        `(
          (org-agenda-files :tag . "TASKS")
          )))


(after! org-capture
  (setq org-default-notes-file "tasks/inbox.org")
  ;; Get into insert state immediately after entering Capture
  ;;(add-hook 'org-capture-mode-hook 'evil-insert-state)
  (setq org-capture-templates
        ;; Looking for the meaning of the '%' placeholders?
        ;; Check https://orgmode.org/manual/Template-expansion.html#Template-expansion
        '(("t" "todo" entry
            (file "tasks/inbox.org")
            "* TODO %?\n%U\n")
          ("l" "todo with link" entry
            (file "tasks/inbox.org")
            "* TODO %?\n%U\n%i\n%a\n")
          ("n" "note wih link" entry
            (file "tasks/inbox.org")
            "* %?\n%U\n%i\n%a\n")
          ("r" "weekly org review" entry
            (file "tasks/weekly_reviews.org")
            (file "templates/weekly_review.org") :prepend t)
          ("p" "Protocol Snippet" entry
            (file "tasks/inbox.org")
            "* %a\n%U\n\n#+begin_quote\n%i\n#+end_quote\n\n%?"
            :empty-lines 1
            :immediate-finish t)
          ("L" "Protocol Link" entry
            (file "tasks/inbox.org")
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
  (setq org-roam-directory (file-truename "~/org/notes")
        org-roam-file-exclude-regexp "weekly_reviews.org"
        org-roam-db-location "~/.local/cache/org-roam/org-roam.db"
        org-roam-graph-exclude-matcher '("daily/" "tasks/")
        org-roam-graph-link-hidden-types '("file" "http" "https" "attachment" "fuzzy")
        +org-roam-open-buffer-on-find-file nil)

  (setq org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags)))))

  :config
  (setq org-roam-node-display-template "${title:*} ${tags:32}")

  (map! :leader
        :prefix "n"
        :desc "Insert org-roam link" "i" #'org-roam-node-insert
        :desc "Find in org-roam notes" "f" #'org-roam-node-find
        :desc "Show org-roam Graph" "g" #'org-roam-graph
        :desc "Toggle org-roam Buffer" "r" #'org-roam-buffer-toggle)

  (map! :leader
        :desc "Capture to daily" "D" #'org-roam-dailies-capture-today
        :prefix ("d" . "dailies")
        :desc "Find daily for today" "t" #'org-roam-dailies-find-today
        :desc "Find daily in calendar" "d" #'org-roam-dailies-find-date
        :desc "Find daily for yesterday" "y" #'org-roam-dailies-find-yesterday
        :desc "Find next daily" "n" #'org-roam-dailies-find-next-note
        :desc "Find previous daily" "p" #'org-roam-dailies-find-previous-note)

  (setq org-roam-capture-templates
        '(("t" "topic" plain "%?" :if-new
           (file+head "topic/${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :topic:\n")
           :unnarrowed t)
          ("l" "literature" plain "%?" :if-new
           (file+head "literature/%<%Y%m>-${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :literature:\n")
           :unnarrowed t)
          ("p" "project" plain "%?" :if-new
           (file+head "project/%<%Y%m>-${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :project:\n")
           :unnarrowed t)
          ("a" "area" plain "%?" :if-new
           (file+head "area/%<%Y%m>-${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :area:\n")
           :unnarrowed t)
          ("m" "recurring meeting" plain "%?" :if-new
           (file+head "meeting/%<%Y%m>-${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :meeting:\n#+startup: overview\n")
           :unnarrowed t)
          ("w" "who" plain "%?" :if-new
           (file+head "who/${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :who:\n")
           :unnarrowed t)
          ))

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?\n%U" :if-new
           (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d, %a W%V>\n#+filetags: :daily:\n"))))


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


;; (after! ivy-bibtex
;;   (setq bibtex-completion-bibliography "~/org/resources/Zotero.bib"
;;         bibtex-completion-library-path "~/Dropbox/Zotero"
;;         bibtex-completion-pdf-field "File"
;;         bibtex-completion-notes-path "~/org/notes/literature"
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


;; (after! rustic
;;   (setq rustic-flycheck-clippy-params "--message-format=json"))


(after! treemacs
  (defun treemacs-custom-filter (filename _)
    (or (s-equals? "__pycache__" filename)
        (s-ends-with? ".pyc" filename)))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-custom-filter)
  )


(after! whitespace

  (defun rodelrod/toggle-show-all-whitespace ()
    "Toggle showing all whitespace as visible characters.
Toggles between Doom's very bare default style and a more complete style showing almost everything."
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
    "Deactivate highlighting current line entering writeroom mode and reactivate upon exit"
     (hl-line-mode (if writeroom-mode -1 +1)))
  (add-hook 'writeroom-mode-hook #'rodelrod/switch-off-hl-line))
