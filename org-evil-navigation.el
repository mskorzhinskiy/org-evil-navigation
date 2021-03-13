;;; org-evil-navigation.el --- navigation functions for org-mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Mikhail Skorzhisnkii
;;
;; Author: Mikhail Skorzhisnkii <https://github.com/rasmi>
;; Maintainer: Mikhail Skorzhisnkii <mskorzhinskiy@eml.cc>
;; Created: January 31, 2021
;; Modified: January 31, 2021
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/rasmi/org-evil-navigation
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'org)

(defcustom org-evil-navigation-reveal-context 'minimal
  "Defines which context should be used when revealing is done.

Configuration value is absolutely similar to values defined at
`org-show-context-detail'.

Allowed visibility spans are

  minimal        show current headline; if point is not on headline,
                 also show entry

  local          show current headline, entry and next headline

  ancestors      show current headline and its direct ancestors; if
                 point is not on headline, also show entry

  lineage        show current headline, its direct ancestors and all
                 their children; if point is not on headline, also show
                 entry and first child

  tree           show current headline, its direct ancestors and all
                 their children; if point is not on headline, also show
                 entry and all children

  canonical      show current headline, its direct ancestors along with
                 their entries and children; if point is not located on
                 the headline, also show current entry and all children"
  :group 'org
  :type '(choice :tag "Detail level"
                 (const minimal)
                 (const local)
                 (const ancestors)
                 (const lineage)
                 (const tree)
                 (const canonical)))

(defcustom org-evil-navigation-collpase-on-move nil
  "If set to true, collpase previous heading, uncollapse next heading."
  :group 'org
  :type '(choice :tag "Revealing type"
                 (const entry)
                 (const subtree)
                 (const children)
                 (const children-and-entry)
                 nil))

(defun org-evil-navigation-hide ()
  "Collapse parent headline if there is parent headline."
  (save-excursion
    (while (org-up-heading-safe)
      (outline-hide-subtree))))

;;;###autoload
(defun org-evil-navigation-collapse-set ()
  "Interactive function to control `org-evil-navigation-collpase-on-move' setting."
  (interactive)
  (if (not org-evil-navigation-collpase-on-move)
    (let ((type (completing-read
                 "Choose style:"
                 (list 'entry
                       'subtree
                       'children
                       'children-and-entry)
                 nil t)))
      (setq org-evil-navigation-collpase-on-move type)
      (message (format "Collapse %s on navigation" type)))
    (setq org-evil-navigation-collpase-on-move nil)
    (message "Collapse on navigation disabled!")))

(defun org-evil-navigation-reveal (narrow-to-subtree)
  "Reveal tree using `org-show-context' and narrow buffer to parent subtree.

Narrowing will be done only if NARROW-TO-SUBTREE is set to true.
Configure `org-evil-navigation-reveal-context' to which level of
details to reveal."
  (org-show-context org-evil-navigation-reveal-context)
  (save-excursion
    (org-up-heading-safe)
    (when narrow-to-subtree
      (org-narrow-to-subtree)))
  (cond ((string= org-evil-navigation-collpase-on-move 'entry)
         (org-show-entry))
        ((string= org-evil-navigation-collpase-on-move 'subtree)
         (org-show-subtree))
        ((string= org-evil-navigation-collpase-on-move 'children)
         (org-show-children))
        ((string= org-evil-navigation-collpase-on-move 'children-and-entry)
         (org-show-entry)
         (org-show-children))))

(setq org-evil-navigation-collpase-on-move t)

;;;###autoload
(defun org-evil-navigation-up ()
  "Go /up/ to headline and possibly reveal subtree.

Narrow to subtree of the parent headline if it is non-visible.
For subtrees without parents, i.e. level 0 headlines, just widen
the buffer."
  (interactive)
  ;; If the parent is visible, do the re-reveal. Otherwise just jump straight ahead.
  ;; If the parent is level 0 heading, widen the buffer.
  (let ((should-reveal (not (save-excursion (org-up-heading-safe)))))
    (when should-reveal
      (org-evil-navigation-hide)
      (widen))
    (org-up-element)
    (when (not (= 1 (org-outline-level)))
      (when should-reveal
        (org-evil-navigation-reveal 'narrow)))))

(defmacro org-evil-navigation-exre (&rest body)
  "Execute BODY saving both excursion and restriction."
  (declare (debug (body)))
  `(save-excursion
     (save-restriction
       ,@body)))

;;;###autoload
(defun org-evil-navigation-down ()
  "Go /down/ to the first visible child.

If no children are visible, go down just to the first child. If
there're no children, then hide all details and leavy only direct
hiearhy to be shown."
  (interactive)
  (if (not (save-excursion (org-goto-first-child)))
      ;; Hide details if there're no children
      (org-evil-navigation-hide)
    ;; Try to jump to the first one
    (let ((new-point
           (org-evil-navigation-exre
            (org-narrow-to-subtree)
            (org-goto-first-child)
            (let ((last-point -1))
              (while (and (org-invisible-p)
                          (not (= (point) last-point)))
                (setq last-point (point))
                (org-forward-heading-same-level 1 t)))
            ;; Return new point only we're at the visible portion of buffer
            (unless (org-invisible-p)
              (point)))))
      (if new-point
          (goto-char new-point)
        (org-goto-first-child))))
  (org-evil-navigation-reveal nil)
  (org-show-siblings))

(defun org-evil-navigation-reveal-jump-to (what-func &rest args)
  "Call WHAT-FUNC with ARGS, get new location and reveal subtree.

Revealing will be done only if object is unvisible."
  ;; NOTE (implementation)
  ;;
  ;; There are basically two distinct concept in org-mode/emacs: invisibility
  ;; and narrowing. This function tries to mitigate narrowing, however not all
  ;; org-mode and outline-mode functions will ignore invisible parts of the
  ;; buffer.
  (let* ((old-point (point))
         (next-headline-visible
          (save-excursion
            (apply what-func args)
            (= old-point (point))))
         (next-heading-point
          (org-with-wide-buffer
           (apply what-func args)
           (point)))
         (should-reveal
          (and next-headline-visible
               (not (= old-point next-heading-point)))))
    (when org-evil-navigation-collpase-on-move
      (outline-hide-subtree))
    (when should-reveal
        (widen))
    (goto-char next-heading-point)
    (org-evil-navigation-reveal
     (when should-reveal
       'narrow))))

;;;###autoload
(defun org-evil-navigation-next ()
  "Go to next thing: heading, item or paragraph.

For headlines function also narrow to parent subtree. If there is
no parent headline, narrow current subtree."
  (interactive)
  (cond ((org-at-heading-p)
         (org-evil-navigation-reveal-jump-to
          #'org-forward-heading-same-level 1 'invisible-ok))
        ((org-in-item-p)
         (org-next-item))
        (t
         (save-restriction
           (org-narrow-to-element)
           (forward-paragraph 1)))))

;;;###autoload
(defun org-evil-navigation-prev ()
  "Go to previous thing: heading, item or paragraph.

For headlines function also narrow to parent subtree. If there is
no parent headline, narrow current subtree."
  (interactive)
  (cond ((org-at-heading-p)
         (org-evil-navigation-reveal-jump-to
          #'org-backward-heading-same-level 1 'invisible-ok))
        ((org-in-item-p)
         (org-previous-item))
        (t
         (save-restriction
           (org-narrow-to-element)
           (backward-paragraph 1)))))

(provide 'org-evil-navigation)
;;; org-evil-navigation.el ends here
