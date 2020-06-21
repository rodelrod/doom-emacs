;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
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
(setq doom-font (font-spec :family "Source Code Pro" :size 14))
(setq doom-variable-pitch-font (font-spec :family "Ubuntu" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

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


;; System locale to use for formatting time values.
(setq system-time-locale "C")         ; Make sure that the weekdays in the
                                      ; time stamps of your Org mode files and
                                      ; in the agenda appear in English.

;; Custom key bindings
;; ===================

;; Use C-; to insert current datetime like in every other app
(defun rodelrod/insert-timestamp ()
  "Insert current date time formatted like an org inactive timestamp."
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %a %H:%M]")))
(map! :nvie "C-;" 'rodelrod/insert-timestamp)

;; Org settings
;; ============

;; Font Display
;; ------------

;; Bigger headings for org-mode, markdown and any other outliney things
(custom-set-faces!
  '(org-document-title :weight bold :height 1.35)
  '(outline-1 :weight bold :height 1.25)
  '(outline-2 :weight bold :height 1.20)
  '(outline-3 :weight bold :height 1.15)
  '(outline-4 :weight semi-bold :height 1.12)
  '(outline-5 :weight semi-bold :height 1.09)
  '(outline-6 :weight semi-bold :height 1.06)
  '(outline-8 :weight semi-bold :height 1.03)
  '(outline-9 :weight semi-bold))

;; +org-pretty-mode: Hide the ~tildes~ and =equals= of the world, as well as org entities
(add-hook! 'org-mode-hook #'+org-pretty-mode)
(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "WAITING(w@)" "SOMEDAY(s)" "|" "DONE(d)" "CANCELLED(c@)")))
  (setq org-enforce-todo-dependencies t)
  ;; Add CLOSED timestamp when todo is done
  (setq org-log-done t)
  ;; Put log notes (C-c C-z) and state changes in LOGBOOK drawer.
  (setq org-log-into-drawer t)
  ;; Open narrowed indirect buffer in a new frame instead of re-using another window.
  ;; This means I have to delete the buffers myself, or they'll just accumulate.
  (setq org-indirect-buffer-display 'new-frame)

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

  ;; Org-Agenda
  ;; ----------
  (setq org-agenda-files '("~/org/"))
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
    (org-agenda arg "n"))

  ;; Auto-save all org files on some org-agenda commands (feel free to add)
  ;; based on https://emacs.stackexchange.com/a/7840 and https://emacs.stackexchange.com/a/489
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)
  (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers)
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (add-hook 'org-capture-after-finalize-hook 'org-save-all-org-buffers)

  ;; Org-Archive
  ;; -----------
  (setq org-archive-location "%s_archive::datetree/")

  ;; Org-Roam
  ;; --------
  (setq org-roam-tag-sources '(prop all-directories))
  (setq org-roam-directory "/data/Dropbox/Dropbox/Databases/Org/notes")


  ;; Org: Personal Project Setup
  ;; ---------------------------

  ;; Org-Stuck-Projects
  ;; List as stuck project if ProjectState property is ACTIVE but it has no
  ;; sub-task marked as NEXT; except if project is marked as a SOMEDAY, DONE
  ;; or CANCELLED todo item. The ProjectState property is set automatically on
  ;; every heading that has a statistics cookie.
  (setq org-stuck-projects
        '("+ProjectState=\"ACTIVE\"/-SOMEDAY-DONE-CANCELLED" ("NEXT") nil ""))

  ;; Toggle ProjectState in headings with a statistic cookie between
  ;; ACTIVE (if it contains at least one subtask to be done) and
  ;; MUTED (if there's no subtask to be done).
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    ;; We want to shadow a couple of variables to turn off loggin below, but
    ;; since lexical binding is on in this file, we need to force dynamic
    ;; binding using defvar.
    (defvar org-log-done)
    (defvar org-log-states)
    (let (org-log-done org-log-states)   ; turn off logging
      (org-set-property "ProjectState" (if (= n-not-done 0) "MUTED" "ACTIVE")))
    (ignore n-done))   ; reassure the bytecomp that we know we're not using the variable
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;; Set ACTIVE and MUTED as allowed values for ProjectState
  (defun org-property-set-allowed-project-states (property)
    "Set allowed valued for the ProjectState property."
    (when (equal property "ProjectState") '("ACTIVE" "MUTED")))
  (add-hook 'org-property-allowed-value-functions 'org-property-set-allowed-project-states)

  )

(add-hook! 'org-journal-mode-hook #'org-indent-mode)
(after! org-journal
  (setq org-journal-date-prefix "#+TITLE: "
        org-journal-date-format "%A W%V, %d/%m/%Y"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-carryover-items nil)
  (add-to-list '+word-wrap-visual-modes 'org-journal-mode))
