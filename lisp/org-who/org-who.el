;;; org-who.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Rodrigo Eliseu
;;
;; Author: Rodrigo Eliseu <rodrigo@eliseu.me>
;; Maintainer: Rodrigo Eliseu <rodrigo@eliseu.me>
;; Created: August 02, 2023
;; Modified: August 02, 2023
;; Version: 0.0.1
;; Keywords: convenience org-mode
;; Homepage: https://github.com/rodelrod/org-who
;; Package-Requires: ((emacs "27.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'cl-lib)
(require 'org-element)
(require 'rx)
(require 's)


;; Composable Regexps
;; ----------------
(rx-define org-who--bullet-regexp (seq bol (or "- " "+ ")))
(rx-define org-who--who-line-regexp (seq bol "- who :: "))
(rx-define org-who--key-regexp (seq "@" (group (+ (any word ?_)))))
(rx-define org-who--value-strict-regexp (opt (+ blank) (group (>= 2 (+ upper (* (any word ?_))) (opt (or blank "-"))))))
(rx-define org-who--value-non-strict-regexp (opt (+ blank) (group (* nonl))))

;; Hoist
;; -----
(defun org-who-hoist ()
  (interactive)
  (let* ((main-who-list-elt (org-who--get-main-who-list-element))
         (main-who-list (org-who--get-list-from-element main-who-list-elt))
         (who-list-items (org-who--gather-from-who-list))
         (who-line-items (org-who--gather-from-who-line))
         (body-items (org-who--gather-from-body)))
    (setq main-who-list (org-who--merge-who-items who-list-items main-who-list))
    (setq main-who-list (org-who--merge-who-items who-line-items main-who-list))
    (setq main-who-list (org-who--merge-who-items body-items main-who-list 'PROTECT-INTO))
    (setq main-who-list (org-who--sort-alist-by-key main-who-list))
    (org-who--replace-element main-who-list-elt (org-who--format-main-who-list main-who-list))))

(defun org-who--merge-who-items (from into &optional protect-into)
  (let* ((new-list '())
         (keys-with-duplicates (mapcar #'car (append from into)))
         (keys (cl-remove-duplicates keys-with-duplicates :test #'equal)))
    (dolist (key keys new-list)
      ;; get values for key in from and into
      (let* ((value-from (cdr (assoc key from)))
             (value-into (cdr (assoc key into)))
             (value-to-insert
              (if (and value-into protect-into)
                  value-into
                (org-who--longest-value-string (list value-from value-into)))))
        (push (cons key value-to-insert) new-list)))
    new-list))

(defun org-who--longest-value-string (values)
  (car (sort values (lambda (a b) (> (length a) (length b))))))

(defun org-who--sort-alist-by-key (my-alist)
  (sort my-alist #'(lambda (a b) (string< (car a) (car b)))))

(defun org-who--replace-element (org-elt replacement)
  (save-excursion
    (let ((start (org-element-property :begin org-elt))
          (end (org-element-property :end org-elt)))
      (goto-char start)
      (delete-region start end)
      (insert replacement))))

(defun org-who--format-main-who-list (main-who-list)
  (string-join
   (mapcar (lambda (item)
             (if (cdr item)
                 (format "- @%s %s\n" (car item) (cdr item))
               (format "- @%s\n" (car item))))
           main-who-list)))


;; Main Who List
;; -----------------
(defun org-who--get-main-who-list-element ()
  (let*
      ((tree (org-element-parse-buffer))
       (headlines (org-element-map tree 'headline
                    (lambda (headline)
                      (when (and
                             (string= (org-element-property :raw-value headline) "Who")
                             (eq (org-element-property :level headline) 1))    ; main Who is at top-level
                        headline))))
       (who-headline (car headlines))
       (main-who-lists (org-element-map who-headline 'plain-list #'identity))
       (main-who-list-elt (car main-who-lists)))
    (unless headlines (error "\"* Who\" headline not found in this buffer"))
    main-who-list-elt))

(defun org-who--get-list-from-element (main-who-list-elt)
  (org-who--gather-from-list-in-region
   (org-element-property :begin main-who-list-elt)
   (org-element-property :end main-who-list-elt)))

(defun org-who--gather-from-list-in-region (start end &optional is-strict)
  "Gather Who's from a list within the region defined by START and END.

Assumes that the region contains a proper who list and grabs all top-level
list items to turn them into org-who-items.

All the text after the org-who-key is considered part of the org-who-value,
until the end of line, unless IS-STRICT is non-nil.

If IS-STRICT is non-nil, the org-who-value is restricted to the same rules
used when gathering from the body: this function will try to match only
<Firstname [Middlename] Last-Name>.
"
  (let ((case-fold-search nil)
        (items-from-list '())
        (who-item-regexp
         (if is-strict
             (rx org-who--bullet-regexp org-who--key-regexp org-who--value-strict-regexp)
           (rx org-who--bullet-regexp org-who--key-regexp org-who--value-non-strict-regexp))))
    (save-excursion
      (goto-char start)
      (save-restriction
        (narrow-to-region start end)
        (while
            (re-search-forward who-item-regexp nil t)
          (push (cons (match-string 1) (match-string 2)) items-from-list)))
      items-from-list)))

;; Gather Who's from Who-Line
;; --------------------------
(defun org-who--gather-from-who-line ()
  "Gather who-items from lines starting with `- who ::'"
  (interactive)
  (goto-char (point-min))
  (let ((items-from-who-line '()))
    (while (re-search-forward
            (rx
             org-who--who-line-regexp
             (group (* not-newline) eol)           ; string containing org-who-items to be split later
             ) nil t)
      (let ((items-string (match-string 1)))
        (dolist (item-string (split-string items-string "@" t (rx (* (any blank "," ";")))))
          (push (org-who--split-string-into-two item-string) items-from-who-line))))
    items-from-who-line))

(defun org-who--split-string-into-two (input-string)
  (let ((words (split-string input-string "\\s-+")))
    (if words
        (cons (car words) (string-join (cdr words) " "))
      (cons "" ""))))


;; Gather Who's from Who-List
;; --------------------------
(defun org-who--get-who-list-elements ()
  (let*
      ((tree (org-element-parse-buffer))
       (who-headlines (org-element-map tree 'headline
                        (lambda (headline)
                          (when (and
                                 (string= (org-element-property :raw-value headline) "Who")
                                 (not (eq (org-element-property :level headline) 1)))    ;; we don't want the main one
                            headline)))))
    (org-element-map who-headlines 'plain-list #'identity)))

(defun org-who--gather-from-who-list ()
  "Gather who-items from lines top-level unordered lists"
  (interactive)
  (let ((who-list-elements (org-who--get-who-list-elements))
        (who-list-items '()))
    (dolist
        (who-list-elt who-list-elements)
      (let ((next-list-items
             (org-who--gather-from-list-in-region
              (org-element-property :begin who-list-elt)
              (org-element-property :end who-list-elt))))
        (setq who-list-items (org-who--merge-who-items next-list-items who-list-items))))
    who-list-items))

;; Gather Who's from Body
;; ----------------------
(defun org-who--gather-from-body ()
  "Gather who-items from anywhere except who lines and lists"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (items-from-body '()))
      (while (re-search-forward
              (rx org-who--key-regexp org-who--value-strict-regexp)
              nil t)
        (unless (org-who--current-line-matches-regexp-p
                 (rx org-who--who-line-regexp))    ; Make sure we don't match the who line
          (push (cons (match-string 1) (match-string 2)) items-from-body)))
      items-from-body)))

(defun org-who--current-line-matches-regexp-p (line-regexp)
  "Check if line where there is a match matches a certain regexp"
  (save-match-data
    (save-excursion
      (forward-line 0)
      (re-search-forward line-regexp (line-end-position) t))))



(provide 'org-who)
;;; org-who.el ends here
