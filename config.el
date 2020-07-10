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
      doom-variable-pitch-font (font-spec :family "Source Sans Pro" :size 16))

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


;; System locale to use for formatting time values.
(setq system-time-locale "C")         ; Make sure that the weekdays in the
                                      ; time stamps of your Org mode files and
                                      ; in the agenda appear in English.
(setq delete-by-moving-to-trash t)


(setq doom-localleader-key ","
      doom-localleader-alt-key "M-,")

(defun rodelrod/insert-timestamp ()
  "Insert current date time formatted like an org inactive timestamp."
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a %H:%M]")))
(map! :nvie "C-;" 'rodelrod/insert-timestamp)

(map! :leader
      :prefix "t"
      :desc "Highlight current line" "h" #'hl-line-mode)

;; Bigger headings for org-mode, markdown and any other outliney things
(custom-set-faces!
  '(org-document-title :weight bold :height 1.35)
  '(outline-1 :weight bold :height 1.30)
  '(outline-2 :weight bold :height 1.25)
  '(outline-3 :weight bold :height 1.20 )
  ;; HACK: setting width for this font does not change width but it's enough to
  ;; trick emacs into not giving me the outsized Noto's Black Florette for this
  ;; bullet and use Deja Vu's instead
  '(outline-4 :weight semi-bold :height 1.15 :width semi-condensed)
  '(outline-5 :weight semi-bold :height 1.12)
  '(outline-6 :weight semi-bold :height 1.09)
  '(outline-8 :weight semi-bold :height 1.06)
  '(outline-9 :weight semi-bold))

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
;;; Package Configuration


(use-package! org
  :mode "\\.org_archive\\'"
  :init
  (setq org-directory "~/org/")
  (add-hook! 'org-mode-hook #'(+org-pretty-mode
                               doom-disable-line-numbers-h))
  :config
  (setq org-archive-location "%s_archive::datetree/"
        org-startup-folded 'content
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c@)"))
        org-enforce-todo-dependencies t
        org-log-done t                                ; Add CLOSED timestamp when todo is done
        org-log-into-drawer t                         ; Put log notes (C-c C-z) and state changes in LOGBOOK drawer.
        org-indirect-buffer-display 'new-frame
        org-list-indent-offset 1                      ; indent plain lists with 3 spaces
        tab-width 3)

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
    ;; HACK: We want to shadow a couple of variables to turn off loggin below,
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
        '("+ProjectState=\"ACTIVE\"/-SOMEDAY-DONE-CANCELLED" ("NEXT") nil ""))
  )


(after! org-capture
  (setq org-default-notes-file "tasks/inbox.org")
  ;; Get into insert state immediately after entering Capture
  ;; (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (setq org-capture-templates
        '(("t" "todo" entry
            (file "tasks/inbox.org")
            "* TODO %?\n%U\n")
          ("l" "todo with link" entry
            (file "tasks/inbox.org")
            "* TODO %?\n%U\n%i\n%a\n")
          ("n" "note wih link" entry
            (file "tasks/inbox.org")
            "* %?\n%U\n%i\n%a\n")
          ("p" "org-protocol" entry
            (file "tasks/inbox.org")
            "* %:annotation\n%U\n\n%i\n"
            :empty-lines 1
            :immediate-finish t)
          ("r" "weekly org review" entry
            (file "tasks/weekly_reviews.org")
            (file "templates/weekly_review.org") :prepend t))))


(after! org-agenda
  (setq org-agenda-files (nconc
                          (directory-files-recursively "~/org/tasks" "\.org$")
                          (directory-files-recursively "~/org/notes/project" "\.org$")))
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

          ;; Allow me to archive old items. I prefer archiving only level-2
          ;; headings: Level-1 are Areas and Level-2 can be projects or odd
          ;; tasks. I want to archive project trees in one go instead of
          ;; having the tasks scattered in the Archive datetree.
          ("r" "Tasks/Projects ready to archive (Level-2 items closed more than 2 months ago)"
           tags "+CLOSED<\"<-2m>\"+LEVEL=2")))

  ;; Function used to launch agenda on emacs client startup
  (defun org-agenda-show-n (&optional arg)
    (interactive "P")
    (org-agenda arg "n")))


(after! (org-agenda org-capture)
  ;; Auto-save all org files on some org-agenda commands (feel free to add)
  ;; based on https://emacs.stackexchange.com/a/7840 and https://emacs.stackexchange.com/a/489
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
  (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers)
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (add-hook 'org-capture-after-finalize-hook 'org-save-all-org-buffers)
  )


(use-package! org-roam
  :init
  (map! :leader
        :prefix "n"
        :desc "Insert org-roam link" "i" #'org-roam-insert
        :desc "Insert org-roam link immediately" "I" #'org-roam-insert-immediate
        :desc "Switch to org-roam buffer" "b" #'org-roam-switch-to-buffer
        :desc "Find in org-roam notes" "f" #'org-roam-find-file
        :desc "Find in all notes" "F" #'+default/find-in-notes
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph)
  (setq org-roam-directory "/data/Dropbox/Dropbox/Org/notes"
        org-roam-db-location "/data/Dropbox/Dropbox/Org/notes/db/org-roam.db"
        org-roam-tag-sources '(prop all-directories))
  :config
  (setq org-roam-capture-templates
        '(("t" "topic" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+title: ${title}\n#+created: %U\n"
           :head "#+title: ${title}\n"
           :unnarrowed t)
          ("l" "literature" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "literature/%<%Y%m>-${slug}"
           :head "#+title: ${title}\n#+created: %U\n"
           :unnarrowed t)
          ("p" "project" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "project/%<%Y%m>-${slug}"
           :head "#+title: ${title}\n#+created: %U\n"
           :unnarrowed t)
          ("m" "recurring meeting" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "meeting/%<%Y%m>-${slug}"
           :head "#+title: ${title}\n#+created: %U\n#+startup: overview\n"
           :unnarrowed t)
          ("w" "who" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "who/${slug}"
           :head "#+title: ${title}\n#+created: %U\n"
           :unnarrowed t)
          )))


(use-package! org-journal
  :init
  (map! :leader
        :desc "Today's file"
        "n j t" #'org-journal-open-current-journal-file)
  :config
  (setq org-journal-dir "~/org/notes/journal"
        org-journal-date-prefix "#+TITLE: "
        org-journal-date-format "%A W%V, %d %B %Y"
        org-journal-time-format "[%Y-%m-%d %H:%M] "   ; make it easier to refile preserving data
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-carryover-items nil)
  (add-hook! 'org-journal-after-entry-create-hook #'evil-insert-state)
  (add-to-list '+word-wrap-visual-modes 'org-journal-mode))


(use-package! helm-org-rifle
  :init
  (map! :leader
        "o r" 'helm-org-rifle-agenda-files)
  :config
  ;; show path to header in search results
  (setq helm-org-rifle-show-path t)
  (set-popup-rule! "^\\*helm" :vslot -100 :size 0.30 :ttl nil))
