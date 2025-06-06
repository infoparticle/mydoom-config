(defmacro with-system (type &rest body)
  "Evaluate BODY if `system-type' equals TYPE.
   usage: (with-system windows-nt  <code> )
        : (with-system gnu/linux  <code> ) "
  (declare (indent defun))
  `(when (eq system-type ',type)
     ,@body))

;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Gopinath Sadasivam"
      user-mail-address "noemail@gopi")

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
;; NEVER CHANGE THIS AGAIN, TRIED ALL AND JETBRAINSMONOMEDIUM IS THE BOSS!
(setq myfont "JetBrainsMonoMedium NF"  myfontsize 20)
;;(setq myfont "Fira Code Medium"  myfontsize 17)
;;(setq myfont "Iosevka"  myfontsize 22)
(setq doom-font (font-spec :family myfont :size myfontsize :weight 'medium)
      doom-variable-pitch-font (font-spec :family "sans" :size myfontsize)
      doom-unicode-font (font-spec :family "symbola" :size myfontsize))
;;(setq doom-font (font-spec :family "Fira Code Medium" :size 17 :weight 'medium)
;;      doom-variable-pitch-font (font-spec :family "sans" :size 13))


;;(setq doom-theme 'doom-one-light)
;;(setq doom-theme 'doom-opera-light)

(setq tao-theme-use-height t
      tao-theme-use-sepia nil
      tao-theme-use-boxes nil)
                                        ;(setq doom-theme 'doom-zenburn)
(setq doom-theme 'doom-badger)

(defun my/set-default-font ()
  (interactive)
  (let* ((font (completing-read "Select font: " (font-family-list)))
         (size (read-number "Font size: " 20)))
    (setq doom-font (font-spec :family font :size size))
    (doom/reload-font)))

;;  (setq hl-sexp-foreground-color nil
;;        hl-sexp-background-color "#00253c") ;;dark blue
;;         hl-sexp-foreground-color "#00253c" ;;light yellow

(with-system windows-nt
  (set-selection-coding-system 'utf-16-le)
  (set-default-coding-systems 'utf-8)
  (set-language-environment "UTF-8"))

(setq initial-major-mode 'org-mode)  ; *scratch* will be in org-mode!
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/orgagenda")
;;(setq org-directory "c:/my/org-roam")
(setq org-agenda-root-dir "~/org/orgagenda")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(global-superword-mode 1)

;;(show-paren-mode 1)
;;(setq show-paren-style 'expression)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;(setq org-superstar-headline-bullets-list '("◉" "◎" "⚫" "○" "►" "◇"))

;; from https://www.emacswiki.org/emacs/CalendarWeekNumbers
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

(copy-face 'default 'calendar-iso-week-header-face)
(setq calendar-intermonth-header
      (propertize "Wk"                  ; or e.g. "KW" in Germany
                  'font-lock-face 'calendar-iso-week-header-face))

(set-face-attribute 'calendar-iso-week-face nil
                    :height 1.0 :foreground "salmon")

(use-package! anki-editor
  :after org
  :bind (:map org-mode-map
              ("<f10>" . anki-editor-cloze-region-auto-incr)
              ("<f9>" . anki-editor-cloze-region-dont-incr)
              ("<f8>" . anki-editor-reset-cloze-number)
              ("<f7>"  . anki-editor-push-tree))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
  :config
  (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
        anki-editor-org-tags-as-anki-tags t)

  (defun anki-editor-cloze-region-auto-incr (&optional arg)
    "Cloze region without hint and increase card number."
    (interactive)
    (anki-editor-cloze-region my-anki-editor-cloze-number "")
    (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
    (forward-sexp))
  (defun anki-editor-cloze-region-dont-incr (&optional arg)
    "Cloze region without hint using the previous card number."
    (interactive)
    (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
    (forward-sexp))
  (defun anki-editor-reset-cloze-number (&optional arg)
    "Reset cloze number to ARG or 1"
    (interactive)
    (setq my-anki-editor-cloze-number (or arg 1)))
  (defun anki-editor-push-tree ()
    "Push all notes under a tree."
    (interactive)
    (anki-editor-push-notes '(4))
    (anki-editor-reset-cloze-number))
  ;; Initialize
  (anki-editor-reset-cloze-number)
  )

(setq
time-stamp-active t          ; do enable time-stamps
time-stamp-pattern "34/\\(\\(L\\|l\\)ast\\( \\|-\\)\\(\\(S\\|s\\)aved\\|\\(M\\|m\\)odified\\|\\(U\\|u\\)pdated\\)\\|Time-stamp\\) *: [\"]%b %02d, %Y %a[\"]")


;; can also add this to source code: // (set-variable time-stamp-format "%04y-%:b-%02d %02H:%02M:%02S")

(add-hook 'before-save-hook 'time-stamp)  ; update time stamps when saving

;; https://emacs.stackexchange.com/questions/62720/open-org-link-in-the-same-window
;; https://emacs.stackexchange.com/questions/16652/change-the-behavior-of-org-mode-auto-expand-relative-path-in-link
(after! org
  (setq
   org-adapt-indentation t
   org-cycle-separator-lines -1
   org-ellipsis "  \u2935"
   org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-indent-indentation-per-level 2
   org-link-file-path-type 'relative ;; insert relative links in org-insert-link
   ;;org-odd-levels-only t
   org-src-fontify-natively t
   org-src-preserve-indentation t
   org-src-window-setup 'current-window
   org-startup-indented t
   org-tags-column 50)
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file-other-window))

(add-hook 'org-babel-pre-tangle-hook (lambda () (setq coding-system-for-write 'utf-8-unix)))

 (defun my/org/org-reformat-buffer ()
    (interactive)
    (when (y-or-n-p "Really format current buffer? ")
      (let ((document (org-element-interpret-data (org-element-parse-buffer))))
        (erase-buffer)
        (insert document)
        (goto-char (point-min)))))

(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :custom
  (org-modern-prefix t))

(org-babel-do-load-languages
'org-babel-load-languages
'((python . t)
(ipython . t)
(shell . t)
(eshell . t)
(ledger . t)
(plantuml . t)
;(napkin . t)
(lisp . t)
(gnuplot . t)
(haskell . t)
(java . t)
(dot . t)
(restclient . t)
(mermaid . t)
(clojure . t)
(powershell . t)
(sql . t)))

(setq org-plantuml-jar-path (expand-file-name "~/emacstools/.local/jars/plantuml.jar"))
(setq ob-mermaid-cli-path "C:/Users/gopinat/AppData/Roaming/npm/mmdc.cmd")

;; avoid tangling into dos eol in linux files edited using tramp
(add-hook 'org-babel-pre-tangle-hook (lambda () (setq coding-system-for-write 'utf-8-unix)))

(defun org-babel-copy-block ()
  "Copy the current org-babel source block content to the kill ring."
  (interactive)
  (when (org-babel-get-src-block-info)
    (let ((content (nth 1 (org-babel-get-src-block-info))))
      (kill-new content)
      (message "Source block copied to clipboard."))))


(map! :leader
      :map org-mode-map
      :desc "screenshot for org-roam"
      "m y b" #'org-babel-copy-block)

(use-package! org-auto-tangle
  :defer 3
  :hook (org-mode . org-auto-tangle-mode))

;; Add keybinding for executing code blocks in normal mode
(map! :map org-mode-map
      :n "M-RET" #'org-babel-execute-src-block)

(global-set-key (kbd "C-c a") 'org-agenda-list)
(global-set-key (kbd "M-,") 'execute-extended-command)

(setq org-agenda-inhibit-startup t) ;; ~50x speedup
(setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
(setq org-agenda-use-time-grid t
      org-agenda-timegrid-use-ampm t)

(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t%-6e% s")
                                (todo . " %i %-12:c %-6e")
                                (tags . " %i %-12:c")
                                (search . " %i %-12:c")))

;; Define TODO keyword sets for Tasks and Projects
(setq org-todo-keywords
      '(;; Task Workflow: BACKLOG -> TODO -> DOING -> DONE (with BLOCKED state)
        (type "TASK" ; Name of the type
              "BACKLOG(b)" ; Starting state (b is fast access key C-c C-t b)
              "TODO(t)"    ; Groomed state (t is fast access key C-c C-t t)
              "DOING(i)"   ; In progress state (i is fast access key C-c C-t i)
              "BLOCKED(l)" ; Blocked state (l is fast access key C-c C-t l)
              "|"          ; Separator for done states
              "DONE(d)")   ; Final done state (d is fast access key C-c C-t d)

        ;; Project Workflow: BACKLOG -> ACTIVE -> HOLD -> DONE -> ARCHIVED
        (type "PROJ" ; Name of the type
              "BACKLOG(B)" ; Project backlog state (B is fast access key C-c C-t B)
              "ACTIVE(A)"  ; Project is active (A is fast access key C-c C-t A)
              "HOLD(H)"    ; Project is on hold (H is fast access key C-c C-t H)
              "|"          ; Separator for done states
              "DONE(D)"    ; Project completed (D is fast access key C-c C-t D)
              "ARCHIVED(R)") ; Project archived (R is fast access key C-c C-t R)
        ))

;; Define faces (colors/styles) for TODO keywords according to user preference
;; note, using org-todo-keyword-faces won't work, org-modern overrides them!
(setq org-modern-todo-faces
      '(
        ("ACTIVE"   . (:background "dark blue" :foreground "white" :weight bold))
        ("BLOCKED"  . (:background "dark red" :foreground "white" :weight bold))
        ("HOLD"     . (:background "dark red" :foreground "white" :weight bold))
        ("TODO"     . (:background "purple" :foreground "white" :weight bold))
        ("DOING"    . (:background "yellow" :foreground "black" :weight bold))
        ("DONE"     . (:background "dark green" :foreground "white" :weight bold))
        ("BACKLOG"  . (:foreground "gray" :weight bold))
        ("ARCHIVED" . (:foreground "dim gray" :weight normal))
       ))
;; Ensure Org agenda uses the new keywords correctly
;; (This usually works automatically, but explicit setting can help)
;; (setq org-agenda-todo-ignore-scheduled 'future)
;; (setq org-agenda-todo-ignore-deadlines 'future)
;; (setq org-agenda-todo-ignore-with-date t) ; Don't show done items with dates unless specified

(setq org-todo-state-tags-triggers
      '(
        ;; --- TASK State Triggers ---

        ;; When a task becomes BLOCKED, add a 'needs_attention' tag.
        ("BLOCKED" . (("needs_attention" . t)))

        ;; When a task leaves the BLOCKED state (goes to TODO, DOING, or DONE),
        ;; remove the 'needs_attention' tag.
        ((not "BLOCKED") . (("needs_attention" . nil)))

        ;; When starting work on a task (moving to DOING), add an 'active' tag.
        ("DOING" . (("active" . t)))

        ;; When a task is no longer being actively worked on (moving out of DOING),
        ;; remove the 'active' tag. This covers moving to BLOCKED, DONE, or back to TODO.
        ((not "DOING") . (("active" . nil)))

        ;; When a task is marked DONE, add a 'completed' tag and remove 'active'/'needs_attention'
        ;; (The removal might be redundant due to the (not ...) triggers above,
        ;; but explicit removal on DONE can be clearer).
        ("DONE" . (("completed" . t) ("active" . nil) ("needs_attention" . nil)))


        ;; --- PROJ State Triggers ---

        ;; When a project becomes ACTIVE, add a 'current' tag.
        ("ACTIVE" . (("current" . t)))

        ;; When a project is no longer ACTIVE (moves to HOLD, DONE, ARCHIVED),
        ;; remove the 'current' tag.
        ((not "ACTIVE") . (("current" . nil)))

        ;; When a project is put on HOLD, add a 'paused' tag.
        ("HOLD" . (("paused" . t)))

        ;; When a project leaves the HOLD state, remove the 'paused' tag.
        ((not "HOLD") . (("paused" . nil)))

        ;; When a project is ARCHIVED, add an 'archived_project' tag and remove others.
        ("ARCHIVED" . (("archived_project" . t) ("current" . nil) ("paused" . nil)))

       ))

(setq
 cfw:display-calendar-holidays nil ;don't process holidays.el and clutter the agenda
 cfw:render-line-breaker 'cfw:render-line-breaker-wordwrap)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)

(use-package! org-super-agenda
  :defer 3
  :commands org-super-agenda-mode)

(after! org-agenda
  (org-super-agenda-mode))

(setq org-agenda-sorting-strategy '((agenda time-up deadline-down scheduled-down habit-down priority-down category-keep)
 (todo priority-down category-keep)
 (tags priority-down category-keep)))

(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-tags-column 100 ;; from testing this seems to be a good value
      org-agenda-compact-blocks t)

(setq org-agenda-custom-commands
      '(("o" "Overview"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :todo "TODAY"
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '((:name "Next to do"
                           :todo "NEXT"
                           :order 1)
                          (:name "Important"
                           :tag "Important"
                           :priority "A"
                           :order 6)
                          (:name "Due Today"
                           :deadline today
                           :order 2)
                          (:name "Due Soon"
                           :deadline future
                           :order 8)
                          (:name "Overdue"
                           :deadline past
                           :face error
                           :order 7)
                          (:name "Assignments"
                           :tag "Assignment"
                           :order 10)
                          (:name "Issues"
                           :tag "Issue"
                           :order 12)
                          (:name "Emacs"
                           :tag "Emacs"
                           :order 13)
                          (:name "Projects"
                           :tag "Project"
                           :order 14)
                          (:name "Research"
                           :tag "Research"
                           :order 15)
                          (:name "To read"
                           :tag "Read"
                           :order 30)
                          (:name "Waiting"
                           :todo "WAITING"
                           :order 20)
                          (:name "University"
                           :tag "uni"
                           :order 32)
                          (:name "Trivial"
                           :priority<= "E"
                           :tag ("Trivial" "Unimportant")
                           :todo ("SOMEDAY" )
                           :order 90)
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))

(use-package! atomic-chrome
  :defer 3
 )

(use-package! org-id
  :defer 3
  )
(use-package! org-super-links
  :defer 3
    :bind (("C-c s s" . org-super-links-link)
         ("C-c s l" . org-super-links-store-link)
         ("C-c s C-l" . org-super-links-insert-link)
         ("C-c s d" . org-super-links-quick-insert-drawer-link)
         ("C-c s i" . org-super-links-quick-insert-inline-link)
         ("C-c s C-d" . org-super-links-delete-link))

  :config
  (defun org-super-links-backlink-prefix-timestamp ()
  "Return the default prefix string for a backlink.
Inactive timestamp formatted according to `org-time-stamp-formats' and
a separator ' <- '."
  (let* ((time-format (substring (cdr org-time-stamp-formats) 1 -1))
         (time-stamp (format-time-string time-format (current-time))))
    (format " - [%s] <- "
            time-stamp)))

(defun org-super-links-link-prefix-timestamp ()
  "Return the default prefix string for a backlink.
Inactive timestamp formatted according to `org-time-stamp-formats' and
a separator ' -> '."
  (let* ((time-format (substring (cdr org-time-stamp-formats) 1 -1))
         (time-stamp (format-time-string time-format (current-time))))
    (format " - [%s] -> "
            time-stamp)))


(defun org-super-links-quick-insert-inline-link ()
  (interactive)
  ;; how to handle prefix here?
  (let ((org-super-links-related-into-drawer nil)
        ;(org-super-links-link-prefix 'org-super-links-backlink-prefix-timestamp)
        ;(org-super-links-link-prefix "- ")
        )
    (org-super-links-link)))

(defun org-super-links-quick-insert-drawer-link ()
  (interactive)
  ;; how to handle prefix here?
  (let ((org-super-links-related-into-drawer (or org-super-links-related-into-drawer t))
        ;(org-super-links-link-prefix 'org-super-links-link-prefix-timestamp)
        ;(org-super-links-link-prefix "- ")
        )
    (org-super-links-link)))

(setq org-id-link-to-org-use-id
        'create-if-interactive-and-no-custom-id)

  (setq org-super-links-related-into-drawer t
        org-super-links-backlink-into-drawer t
        org-super-links-link-prefix "- ")
  )

(use-package! org-appear
  :defer 3

  :hook (org-mode . org-appear-mode)
  :config
  (setq  org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

(use-package! org-sidebar
  :defer 3
                )

(require 'url-util) ;needed for encoding spaces to %20

(defun my/clean-spaces-from-path (string)
  (replace-regexp-in-string  "-org$" ""
                             (replace-regexp-in-string "-+" "-"
                                                       (replace-regexp-in-string "[^[:alnum:]]" "-" string))))
(defun my/create-rich-doc()
  (interactive)
  ;; (setq parent-dir (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name))))
  (setq file-name-without-full-path (my/clean-spaces-from-path (file-name-nondirectory buffer-file-name)))
  (make-directory (concat default-directory ".imgs/") :parents)
  (setq myvar/img-folder-path (concat default-directory ".imgs/" file-name-without-full-path)))

(defun my/img-maker ()
  (my/create-rich-doc)
  (setq myvar/img-name (concat (format-time-string "%Y-%m-%d-%H%M%S") ".png"))
  (setq myvar/img-Abs-Path (replace-regexp-in-string "/" "\\" (concat myvar/img-folder-path "/" myvar/img-name) t t)) ;Relative to workspace.
  (setq file-name-without-full-path (my/clean-spaces-from-path (file-name-nondirectory buffer-file-name)))
  (setq myvar/relative-filename (concat "./.imgs/" file-name-without-full-path "/" myvar/img-name))
  (org-insert-heading)
  (insert (concat (read-string (format"Enter Image Header (%s): " myvar/img-name) nil nil  (concat (format-time-string "%Y-%m-%d %H:%M:%S"))) "\n"))
  ;;(insert "\n[[file:" (url-encode-url myvar/relative-filename) "]]" "\n")
  (insert "#+ATTR_ORG: :width 900\n[[file:"  myvar/relative-filename "]]" "\n"))

(defun my/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
 sub-directory (%filenameIMG) as the org-buffer and insert a link to this file."
  (interactive)
                                        ;(evil-insert)
  (save-buffer)
  (my/img-maker)
                                        ;(make-frame-invisible)
                                        ;(lower-frame)
  (message myvar/img-Abs-Path)
  (call-process "c:\\opt\\irfanview\\i_view32.exe" nil nil nil (concat "/clippaste /convert="  myvar/img-Abs-Path))
                                        ;(raise-frame)
                                        ;(make-frame-visible)
  (org-display-inline-images))


(global-set-key (kbd "C-<f5>") 'my/org-screenshot)

(after! org-roam

  (setq org-roam-farm-path "c:/my/org-farm/")
  (setq org-roam-directory "c:/my/org-farm/work.ord")

  (setq org-attach-dir-relative 't)

  (defun my-org-attach-dir (&optional create)
    "Return custom attachment dir based on properties and base dir."
    (let* ((base-dir org-attach-id-dir)
           (dir-prop (org-entry-get nil "DIR"))
           (full-path (when dir-prop
                        (expand-file-name dir-prop base-dir))))
      (when (and create full-path)
        (make-directory full-path t))
      full-path))

  (advice-add 'org-attach-dir :override #'my-org-attach-dir)

  (defun my/org-roam-switch-repo-by-path(repo-path)
    (setq org-roam-directory (expand-file-name repo-path org-roam-farm-path))
    (setq org-attach-id-dir (expand-file-name ".attach" org-roam-directory))
    (unless (file-exists-p org-attach-id-dir)
      (make-directory org-attach-id-dir t)
                                        ;(org-roam-db-sync)
      (message "Switched to Org Roam Repo: %s" repo-path)))

  ;; Function to Switch Org Roam Repository
  (defun my/org-roam-switch-repo ()
    "Prompt to switch between Org Roam repositories in the farm."
    (interactive)
    (let* ((repos (directory-files org-roam-farm-path nil "^[^.]"))
           (selected-repo (completing-read "Select Org Roam Repo: " repos)))
      (setq org-roam-directory (expand-file-name selected-repo org-roam-farm-path))
      (setq org-attach-id-dir (expand-file-name ".attach" org-roam-directory))
      (unless (file-exists-p org-attach-id-dir)
        (make-directory org-attach-id-dir t))
      (org-roam-db-sync)
      (message "Switched to Org Roam Repo: %s" selected-repo)))

  (defun my/org-roam-switch-repo-and-find-node ()
    "Switch to org-roam repo and find node"
    (interactive)
    (let* ((repos (directory-files org-roam-farm-path nil "^[^.]"))
           (selected-repo (completing-read "Select Org Roam Repo: " repos)))
      (setq org-roam-directory (expand-file-name selected-repo org-roam-farm-path))
      (setq org-attach-id-dir (expand-file-name ".attach" org-roam-directory))
      (unless (file-exists-p org-attach-id-dir)
        (make-directory org-attach-id-dir t))
      ;;(org-roam-db-sync)
      (message "Switched to Org Roam Repo: %s" selected-repo))
    (org-roam-node-find))
  (map! "<f1>" #'my/org-roam-switch-repo-and-find-node)

  (map! :leader
        :desc "screenshot for org-roam"
        "z s r" #'my/org-roam-switch-repo)

  (defun my/org-screenshot-for-roam ()
    "Take a screenshot and save it as an org attachment with user-selected subdirectory"
    (interactive)

    ;; Ensure we have org-attach-id-dir set
    (unless (and (boundp 'org-attach-id-dir) org-attach-id-dir)
      (error "Please set org-attach-id-dir first"))

    ;; Get subdirectory relative to org-attach-id-dir
    (setq attach-subdir (file-relative-name
                         (read-directory-name "Select directory to save the screenshot: " org-attach-id-dir)
                         org-attach-id-dir))

    ;; Create full attachment directory path
    (setq full-attach-dir (expand-file-name attach-subdir org-attach-id-dir))
    (unless (file-exists-p full-attach-dir)
      (make-directory full-attach-dir t))

    ;; Create image filename and path
    (setq img-name
          (read-string
           "Image name: "
           (concat (format-time-string "%Y-%m-%d-%H%M%S") ".png")))
    (setq img-full-path (replace-regexp-in-string
                         "/" "\\"
                         (expand-file-name img-name full-attach-dir)
                         t t))

    ;; Insert org heading with properties
    (org-insert-heading)
    (setq sub-heading-name
          (read-string
           "Heading content: "
           (concat (format-time-string "%Y-%m-%d - "))))
    (insert  (concat sub-heading-name "         :ATTACH:\n"))
    (org-entry-put nil "DIR" attach-subdir)

    ;; Insert the attachment link
    (insert (format "[[attachment:%s]]\n" img-name))

    ;; Save clipboard to image using IrfanView
    (call-process "c:\\opt\\irfanview\\i_view32.exe" nil nil nil
                  (concat "/clippaste /convert=" img-full-path))

    ;; Display the image
    (org-display-inline-images))

  (map! :leader
        :desc "screenshot for org-roam"
        "z s s" #'my/org-screenshot-for-roam))

;; ~/.doom.d/config.el
(use-package! org-transclusion
  :after org
  :init
  (map!
   :map global-map "<f12>" #'org-transclusion-add
   :leader
   :prefix "n"
   :desc "Org Transclusion Mode" "t" #'org-transclusion-mode))

(use-package! ox-reveal)

(defvar bigger-org-headlines-cookies nil)

(make-variable-buffer-local 'bigger-org-headlines-cookies)

(define-minor-mode bigger-org-headlines
  "Make Org headlines bigger."
  :lighter " Big-Org-Headlines"
  (if bigger-org-headlines
      (mapc (lambda (face)
              (push (face-remap-add-relative face :height 2.5)
                    bigger-org-headlines-cookies))
            (cons 'org-link org-level-faces))
    (mapc #'face-remap-remove-relative bigger-org-headlines-cookies)
    (setq bigger-org-headlines-cookies nil))
  (force-window-update (current-buffer)))


;; Configure fill width
(setq visual-fill-column-width 110
      visual-fill-column-center-text t)

(setq org-present-text-scale 3)

(defun my/org-present-start ()
  ;; Center the presentation and wrap lines
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (flyspell-mode 0)
  (org-present-read-only)
  (bigger-org-headlines 1)
  (org-present-hide-cursor)
  ;(org-present-big)
  (hide-mode-line-mode 1)
  (visual-fill-column-mode 1)
  (visual-line-mode 1)
  (tab-bar-mode 0)
  (hl-line-mode 0)
  ;(text-scale-increase org-present-text-scale)
  )

(defun my/org-present-end ()
  ;; Stop centering the document
  (visual-fill-column-mode 0)
  (visual-line-mode 0)
                                        ;(setq-local face-remapping-alist '((default variable-pitch default)))
  (org-present-read-write)
  (org-present-small)
  (org-present-show-cursor)
  (bigger-org-headlines 0)
  (hide-mode-line-mode 0)
  (tab-bar-mode 1)
  (hl-line-mode 1)
  )

;; Register hooks with org-present
(add-hook 'org-present-mode-hook 'my/org-present-start)
(add-hook 'org-present-mode-quit-hook 'my/org-present-end)

(defun my/org-present-prepare-slide (buffer-name heading)
  ;; Show only top-level headlines
  (org-overview)

  ;; Unfold the current entry
  (org-show-entry)

  ;; Show only direct subheadings of the slide but don't expand them
  (org-show-children))

(add-hook 'org-present-after-navigate-functions 'my/org-present-prepare-slide)

(with-system windows-nt
  (setq-default ispell-program-name "C:/opt/hunspell/bin/hunspell.exe")
  (setq ispell-hunspell-dict-paths-alist
        '(("en_US" "C:/opt/hunspell/dict/en_US.aff"))))

(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

;;enable when needed
;;(setq text-mode-hook '(lambda() (flyspell-mode t)))

(defun modi/org-in-any-block-p ()
  "Return non-nil if the point is in any Org block.
The Org block can be *any*: src, example, verse, etc., even any
Org Special block.
This function is heavily adapted from `org-between-regexps-p'."
  (save-match-data
    (let ((pos (point))
          (case-fold-search t)
          (block-begin-re "^[[:blank:]]*#\\+begin_\\(?1:.+?\\)\\(?: .*\\)*$")
          (limit-up (save-excursion (outline-previous-heading)))
          (limit-down (save-excursion (outline-next-heading)))
          beg end)
      (save-excursion
        ;; Point is on a block when on BLOCK-BEGIN-RE or if
        ;; BLOCK-BEGIN-RE can be found before it...
        (and (or (org-in-regexp block-begin-re)
                 (re-search-backward block-begin-re limit-up :noerror))
             (setq beg (match-beginning 0))
             ;; ... and BLOCK-END-RE after it...
             (let ((block-end-re (concat "^[[:blank:]]*#\\+end_"
                                         (match-string-no-properties 1)
                                         "\\( .*\\)*$")))
               (goto-char (match-end 0))
               (re-search-forward block-end-re limit-down :noerror))
             (> (setq end (match-end 0)) pos)
             ;; ... without another BLOCK-BEGIN-RE in-between.
             (goto-char (match-beginning 0))
             (not (re-search-backward block-begin-re (1+ beg) :noerror))
             ;; Return value.
             (cons beg end))))))

(defun modi/org-split-block ()
  "Sensibly split the current Org block at point. "
  (interactive)
  (if (modi/org-in-any-block-p)
      (save-match-data
        (save-restriction
          (widen)
          (let ((case-fold-search t)
                (at-bol (bolp))
                block-start
                block-end)
            (save-excursion
              (re-search-backward "^\\(?1:[[:blank:]]*#\\+begin_.+?\\)\\(?: .*\\)*$" nil nil 1)
              (setq block-start (match-string-no-properties 0))
              (setq block-end (replace-regexp-in-string
                               "begin_" "end_" ;Replaces "begin_" with "end_", "BEGIN_" with "END_"
                               (match-string-no-properties 1))))
            ;; Go to the end of current line, if not at the BOL
            (unless at-bol
              (end-of-line 1))
            (insert (concat (if at-bol "" "\n")
                            block-end
                            "\n\n"
                            block-start
                            (if at-bol "\n" "")))
            ;; Go to the line before the inserted "#+begin_ .." line
            (beginning-of-line (if at-bol -1 0)))))
    (message "Point is not in an Org block")))

(defun modi/org-meta-return (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-heading', `org-insert-item',
`org-table-wrap-region', or `modi/org-split-block' depending on
context.  When called with an argument, unconditionally call
`org-insert-heading'."
  (interactive "P")
  (org-check-before-invisible-edit 'insert)
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively (cond (arg #'org-insert-heading)
                                ((org-at-table-p) #'org-table-wrap-region)
                                ((org-in-item-p) #'org-insert-item)
                                ((modi/org-in-any-block-p) #'modi/org-split-block)
                                (t #'org-insert-heading)))))
(advice-add 'org-meta-return :override #'modi/org-meta-return)

(after! perspective
 (setq persp-mode-prefix-key (kbd "C-c p"))
 ;; Because we're not savages who lose their workspaces
 (setq persp-state-default-file (expand-file-name "perspective-state" doom-cache-dir)))

(use-package! eyebrowse
  :init
  (eyebrowse-mode t)
  :config
  (setq eyebrowse-new-workspace t)
  (setq eyebrowse-wrap-around t)
  (map! :leader
        (:prefix ("w" . "windows/workspaces")
         :desc "Switch to config 1" "1" #'eyebrowse-switch-to-window-config-1
         :desc "Switch to config 2" "2" #'eyebrowse-switch-to-window-config-2
         :desc "Switch to config 3" "3" #'eyebrowse-switch-to-window-config-3
         :desc "Switch to config 4" "4" #'eyebrowse-switch-to-window-config-4
         :desc "Create workspace" "c" #'eyebrowse-create-window-config
         :desc "Next workspace" "n" #'eyebrowse-next-window-config
         :desc "Prev workspace" "p" #'eyebrowse-prev-window-config
         :desc "Close workspace" "k" #'eyebrowse-close-window-config)))

;; in your personal keybinding kingdom
(map! :leader
     (:prefix ("p" . "project/perspective") ; SPC p for the win
      :desc "Switch perspective" "s" #'persp-switch
      :desc "Switch buffer" "b" #'persp-switch-buffer
      :desc "Kill perspective" "k" #'persp-kill
      :desc "Rename perspective" "r" #'persp-rename
      :desc "Save perspectives" "S" #'persp-state-save
      :desc "Load perspectives" "L" #'persp-state-load))

(require 'key-chord)

(key-chord-define-global "BB" 'iswitchb)
(key-chord-define-global "FF" 'find-file)
(key-chord-define-global "jk" 'mode-line-other-buffer)
(key-chord-define-global "HH" 'previous-buffer)
(key-chord-define-global "LL" 'next-buffer)

(key-chord-mode +1)

(global-set-key (kbd "<f2>")  (lambda()(interactive)(find-file "c:/my/emacs/start-page.org")))

(with-system windows-nt
  (require 'epa-file)
  (epa-file-enable)
  (setq-local epa-file-encrypt-to '("emacsuser@localhost"))
  (custom-set-variables '(epg-gpg-program  "C:/Program Files (x86)/GnuPG/bin/gpg.exe"))
  (custom-set-variables '(epg-gpgconf-program  "C:/Program Files (x86)/GnuPG/bin/gpgconf.exe"))
  (custom-set-variables '(epg-gpg-home-directory  "c:/Users/gopinat/AppData/Roaming/gnupg"))
  (defadvice epg--start (around advice-epg-disable-agent disable)
    "Make epg--start not able to find a gpg-agent."
    (let ((agent (getenv "GPG_AGENT_INFO")))
      (setenv "GPG_AGENT_INFO" nil)
      ad-do-it
      (setenv "GPG_AGENT_INFO" agent)))

  (defun epg-disable-agent ()
    "Make EasyPG bypass any gpg-agent."
    (interactive)
    (ad-enable-advice 'epg--start 'around 'advice-epg-disable-agent)
    (ad-activate 'epg--start)
    (message "EasyPG gpg-agent bypassed"))

  (defun epg-enable-agent ()
    "Make EasyPG use a gpg-agent after having been disabled with epg-disable-agent."
    (interactive)
    (ad-disable-advice 'epg--start 'around 'advice-epg-disable-agent)
    (ad-activate 'epg--start)
    (message "EasyPG gpg-agent re-enabled")))

(defun my/insert-gpg-header ()
  "Insert a header for .gpg files if it's not already present."
  (when (and (buffer-file-name)
             (string-match-p "\\.gpg\\'" (buffer-file-name)))
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "-\\*- epa-file-encrypt-to:" nil t)
        (goto-char (point-min))
        (insert "; -*- epa-file-encrypt-to: (\"DailyKey@localhost\") -*-\n\n\n")
        (normal-mode)))))

(add-hook 'find-file-hook 'my/insert-gpg-header)

(use-package! popper
  :bind (("C-\\"   . popper-toggle-latest)
         ("M-\\"   . popper-cycle)
         ("M-~"   . popper-kill-latest-popup)
         ("C-M-\\" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package! pulsar
  :config
  (setq pulsar-pulse-functions
        ;; NOTE 2022-04-09: The commented out functions are from before
        ;; the introduction of `pulsar-pulse-on-window-change'.  Try that
        ;; instead.
        '(recenter-top-bottom
          move-to-window-line-top-bottom
          reposition-window
          bookmark-jump
          other-window
          delete-window
          delete-other-windows
          forward-page
          backward-page
          scroll-up-command
          scroll-down-command
          ;; windmove-right
          ;; windmo     ve-left
          ;; windmove-up
          ;; windmove-down
          ;; windmove-swap-states-right
          ;; windmove-swap-states-left
          ;; windmove-swap-states-up
          ;; windmove-swap-states-down
          tab-new
          tab-close
          tab-next
          org-next-visible-heading
          org-previous-visible-heading
          org-forward-heading-same-level
          org-backward-heading-same-level
          outline-backward-same-level
          outline-forward-same-level
          outline-next-visible-heading
          outline-previous-visible-heading
          outline-up-heading))

  (setq pulsar-pulse-on-window-change t)
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-green)
  (setq pulsar-highlight-face 'pulsar-yellow)

  (pulsar-global-mode 1)

  ;; OR use the local mode for select mode hooks

  (dolist (hook '(org-mode-hook emacs-lisp-mode-hook))
    (add-hook hook #'pulsar-mode))

  ;; pulsar does not define any key bindings.  This is just a sample that
  ;; respects the key binding conventions.  Evaluate:
  ;;
  ;;     (info "(elisp) Key Binding Conventions")
  ;;
  ;; The author uses C-x l for `pulsar-pulse-line' and C-x L for
  ;; `pulsar-highlight-line'.
  ;;
  ;; You can replace `pulsar-highlight-line' with the command
  ;; `pulsar-highlight-dwim'.
  (let ((map global-map))
    (define-key map (kbd "C-c h p") #'pulsar-pulse-line)
    (define-key map (kbd "C-c h h") #'pulsar-highlight-line))
  )

(use-package! super-save
  :config
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil)
  (setq super-save-remote-files nil)
  (setq super-save-exclude '(".gpg"))
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (super-save-mode +1))

(setq dired-dwim-target t)
(setq dired-listing-switches "-hal")
(setq dired-recursive-copies (quote always)) ;no asking
(setq dired-recursive-deletes 'top) ; ask once
(setq ls-lisp-dirs-first t)
(setq ls-lisp-sort-reverse t)

(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(with-eval-after-load 'dired
  (defun xah-dired-mode-setup ()
    "to be run as hook for `dired-mode'."
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'xah-dired-mode-setup)

  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(use-package! dired-sidebar
  :defer 3
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "  ")
  (setq dired-sidebar-theme 'ascii)
  ;(setq dired-sidebar-theme 'icons) ;not working
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(setq
 m/sidebar-file "~/orgdir/emacs/sidebar.org"
 m/sidebar-private-file "~/orgdir/emacs/sidebar-private.org")

(defun m/showindex ()
  "Show the index of current projects"
  (interactive)
  (let ((buffer (get-file-buffer m/sidebar)))
    (progn
      (display-buffer-in-side-window buffer '((side . left) (window-width . 0.25)))
      (set-window-dedicated-p (get-buffer-window buffer) t)
      (select-window (get-buffer-window buffer))
          ;; (m/index-faces)
      )))

(defun m/hideindex ()
  "Hide the index of current projects"
  (interactive)
  (let ((buffer (get-file-buffer m/sidebar)))
    (progn
      (delete-window (get-buffer-window buffer)))))

(defun m/toggleindex ()
  "Toggle the index of current projects"
  (interactive)
  (let* ((buffer (get-file-buffer m/sidebar))
         (window (get-buffer-window buffer)))
    (if (and buffer window)
        (m/hideindex)
      (progn
        (find-file-noselect m/sidebar)
        (m/showindex)))))

(defun m/toggleindex-public ()
  "Set the sidebar-file file and toggle it"
  (interactive)
  (setq m/sidebar m/sidebar-file)
  (m/toggleindex))


(defun m/toggleindex-private ()
  "Set the sidebar file and toggle it"
  (interactive)
  (setq m/sidebar m/sidebar-private-file)
  (m/toggleindex))

(global-set-key (kbd "C-<f1>") 'm/toggleindex-public)
(global-set-key (kbd "C-<f2>") 'm/toggleindex-private)

;(use-package! speed-type)

(use-package! evil-mc)

(when (file-exists-p "c:/opt/putty/plink.exe")
  (setq exec-path (append '("C:/opt/putty") exec-path)))

;(doom-themes-neotree-config)
;(setq doom-themes-neotree-file-icons t)

(use-package! all-the-icons
  :if (display-graphic-p))

(with-eval-after-load 'projectile
  (with-system windows-nt
    (projectile-register-project-type 'maven '("pom.xml")
                                      :project-file "pom.xml"
                                      :compile "mvn.cmd -B clean install"
                                      :test "mvn.cmd -B test"
                                      :run "mvn.cmd exec:java"
                                      :test-suffix "Tests"))
  (with-system gnu/linux
    (projectile-register-project-type 'maven '("pom.xml")
                                      :project-file "pom.xml"
                                      :compile "mvn -B clean install"
                                      :test "mvn -B test"
                                      :run "mvn exec:java"
                                      :test-suffix "Tests"))

  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'hybrid)
  (setq projectile-globally-ignored-directories
        '("*dist"
          "target"
          ".imgs"
          "*node_modules"
          ".idea"
          ".vscode"
          ".ensime_cache"
          ".eunit"
          ".git"
          ".hg"
          ".fslckout"
          "_FOSSIL_"
          ".bzr"
          "_darcs"
          ".tox"
          ".svn"
          ".stack-work"
          ".ccls-cache"
          ".cache"
          ".clangd")))

(global-set-key (kbd "C-M-i") 'iedit-mode)
(add-hook! 'prog-mode-hook
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?_ "w"))

(defun my/git-diff-to-buffer ()
  "Run `git diff` and display results in `diff-mode`, read-only."
  (interactive)
  (let ((buf (get-buffer-create "*Git Diff*")))
    (with-current-buffer buf
      (erase-buffer)
      (call-process "git" nil buf nil "diff")
      (diff-mode)
      (read-only-mode 1))  ;; Make buffer read-only
    (switch-to-buffer buf)))

(defun my/git-quick-commit-and-push ()
  "Quickly commit all changes and push, allowing the user to edit the default commit message."
  (interactive)
  (let* ((default-msg "wip - updates")
         (commit-msg (completing-read "Commit message: " nil nil nil default-msg)))
    (when (yes-or-no-p (format "Commit and push with message: \"%s\"? " commit-msg))
      (shell-command (format "git add . && git commit -m \"%s\" && git push" commit-msg))
      (message "Git commit & push done!"))))

 (use-package! highlight-symbol
        :defer 10
        :bind (("M-n" . highlight-symbol-next)
               ("M-p" . highlight-symbol-prev))
        :init
        (setq highlight-symbol-idle-delay 0.3)
        (add-hook 'prog-mode-hook 'highlight-symbol-mode)
        (highlight-symbol-nav-mode))

(setq infodir-root "~/emacstools/my-info-references/info-files/")

;; https://github.com/frap/doom-termux/blob/3cd61486bab2c534da1f464881ac99b385eff5fc/%2Bpopup.el
(set-popup-rule! "^\\*info.*" :size 82 :side 'right :ttl t :select t :quit t)

(defun info-mode ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (kill-buffer (current-buffer))
    (info file-name)))
(add-to-list 'auto-mode-alist '("\\.info\\'" . info-mode))


(defun my/pick-infodir-name-action-list-candidates (str pred _)
  (setq infodir-list  (cl-delete-if (lambda (k) (string-match-p "^\\." k))
                                 (directory-files infodir-root))))
(defun my/pick-infodir-name-action (x)
  (info  (concat infodir-root x)))

(defun my/pick-infodir-name ()
  "pick a wiki from dropbox folder."
  (interactive)
  (ivy-read "List of info files: "  #'my/pick-infodir-name-action-list-candidates
            :preselect (ivy-thing-at-point)
            :require-match t
            :action #'my/pick-infodir-name-action
            :caller 'my/pick-infodir-name))

(map! :leader
      :desc "Pick an info file"
      "o i" #'my/pick-infodir-name)

(load "~/emacstools/bb-codegen/emacs-bb.el")

(setq yas-snippet-dirs
      '("~/emacstools/snippets")) ;; personal snippets

(map! :map org-mode-map
     "C-c <tab>" #'yas-next-field
     "C-c y" #'yas-expand
     "C-c C-y" #'yas-insert-snippet)

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq evil-shift-width 4)))

(after! Cider
 (map! :map cider-mode-map
      :n "M-RET" #'cider-eval-defun-at-point))

(with-system windows-nt
  (setq JAVA_BASE "C:/Users/gopinat/.jabba/jdk"))

(with-system gnu/linux
  (setq JAVA_BASE "/usr/lib/jvm"))
;;
;; This function returns the list of installed
;;
(defun switch-java--versions ()
  "Return the list of installed JDK."
  (seq-remove
   (lambda (a) (or (equal a ".") (equal a "..")))
   (directory-files JAVA_BASE)))


(defun switch-java--save-env ()
  "Store original PATH and JAVA_HOME."
  (when (not (boundp 'SW_JAVA_PATH))
    (setq SW_JAVA_PATH (getenv "PATH")))
  (when (not (boundp 'SW_JAVA_HOME))
    (setq SW_JAVA_HOME (getenv "JAVA_HOME")))
  (when (not (boundp 'SW_EXEC_PATH))
    (setq SW_EXEC_PATH exec-path))
  )


(defun switch-java ()
  "List the installed JDKs and enable to switch the JDK in use."
  (interactive)
  ;; store original PATH and JAVA_HOME
  (switch-java--save-env)

  (let ((ver (completing-read
              "Which Java: "
              (seq-map-indexed
               (lambda (e i) (list e i)) (switch-java--versions))
              nil t "")))
    ;; switch java version
    (setenv "JAVA_HOME" (concat JAVA_BASE "/" ver ))
    (setenv "PATH" (concat (concat (getenv "JAVA_HOME") "/bin")
                           ";" SW_JAVA_PATH))
    (setq exec-path (append (list (concat JAVA_BASE "/" ver "/bin" )) SW_EXEC_PATH)))
  ;; show version
  (switch-java-which-version?))


(defun switch-java-default ()
  "Restore the default Java version."
  (interactive)
  ;; store original PATH and JAVA_HOME
  (switch-java--save-env)

  ;; switch java version
  (setenv "JAVA_HOME" SW_JAVA_HOME)
  (setenv "PATH" SW_JAVA_PATH)
  (setq exec-path SW_EXEC_PATH)
  ;; show version
  (switch-java-which-version?))


(defun switch-java-which-version? ()
  "Display the current version selected Java version."
  (interactive)
  ;; displays current java version
  (message (concat "JAVA_HOME : " (getenv "JAVA_HOME"))))


(defun powershell (&optional buffer)
  "Launches a powershell in buffer *powershell* and switches to it."
  (interactive)
  (let ((buffer (or buffer "*powershell*"))
        (powershell-prog "c:\\windows\\system32\\WindowsPowerShell\\v1.0\\powershell.exe"))
    (make-comint-in-buffer "shell" "*powershell*" powershell-prog)
    (switch-to-buffer buffer)))

(defun show-alert (output-message background-color foreground-color)
  (when (posframe-workable-p)
    (posframe-show "*make-output*"
                   :poshandler #'posframe-poshandler-frame-bottom-right-corner
                                        ;:string (concat (format-time-string "\n[%Y-%m-%d %H:%M:%S]\n\n") output-message "\n")
                   :string (concat output-message)
                   :timeout 20
                   :right-fringe 10
                   :left-fringe 10
                   :border-width 1
                   :border-color foreground-color
                   :background-color background-color
                   :foreground-color foreground-color)))

;; (defun process-exit-code-and-output (program &rest args)
;;   "Run PROGRAM with ARGS and return the exit code and output in a list."
;;   (with-temp-buffer
;;     (list (apply 'call-process program nil (current-buffer) nil args)
;;           (buffer-string))))

(defun process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (let* ((buffer "*Make Output Tmp*")
         (process (apply 'start-process "procname" buffer program args)))
    (set-process-sentinel
     process
     (lambda (process event)
       (when (eq (process-status process) 'exit)
         (let* ((exit-code (process-exit-status process))
                (output (with-current-buffer "*Make Output Tmp*" (buffer-string)))
                (alert-color (if (zerop exit-code) "green" "yellow")))
           (kill-buffer "*Make Output Tmp*")
           (show-alert output nil alert-color)))))))

(defun my/run-make-on-save()
  (interactive)
  (show-alert "running..." nil "pink")
  (process-exit-code-and-output "make" "run" "--quiet"))

(defun my/delete-all-posframes ()
  (interactive)
  (posframe-delete-all))
(define-minor-mode  run-make-on-save-mode
  "Minor mode to automatically call `make run' whenever the
current buffer is saved."
  :lighter " MoS"
  (if run-make-on-save-mode
      (progn
        (setq super-save-mode nil)
        (save-buffer)
        (global-set-key (kbd "C-c m") 'my/run-make-on-save)
        (global-set-key (kbd "C-c l") 'my/delete-all-posframes)
        (add-hook 'after-save-hook 'my/run-make-on-save nil t))
    (remove-hook 'after-save-hook 'my/run-make-on-save nil t)))

(after! dumb-jump
  (setq dumb-jump-read-tags t)
  (setq dumb-jump-disable-obsolete-warnings t)

  ;; Aggressive caching
  (setq dumb-jump-max-find-time 5)
  (setq dumb-jump-prefer-searcher 'rg)  ; Use ripgrep - it's faster!
  (setq dumb-jump-aggressive t))         ; More aggressive searching



(defun proj-build-ctags ()
  "Build ctags for the current project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (if default-directory
        (progn
          (message "Building ctags in %s" default-directory)
          (shell-command "universal-ctags -R --languages=Java --exclude=.git --exclude=target"))
      (message "Not in a project!"))))

(defun proj-update-ctags ()
  "Update ctags for the current project."
  (interactive)
  (proj-build-ctags))  ; Simply re-run the build for simplicity

;; If you want to be extra sure, explicitly set the path
(setq tags-file-name (expand-file-name "tags" (projectile-project-root)))
(setq tags-table-list `(,(projectile-project-root)))

(use-package! citre
  :defer t
  :init
  ;; Configure basic settings
  (setq citre-readtags-program "readtags.exe"  ; The star of our show
        citre-ctags-program "universal-ctags.exe"        ; Its trusty sidekick
        citre-use-project-root-when-creating-tags t
        citre-prompt-language-for-ctags-command t)

  :config
  ;; The good stuff - default keybindings
  (map! :after evil
        :n "gd" #'citre-jump
        :n "gb" #'citre-jump-back))

(add-to-list 'exec-path "C:/tools/ghc-9.2.3/bin")

(after! rustic-flycheck
  (customize-set-variable 'rustic-flycheck-clippy-params-stable
                          (concat rustic-flycheck-clippy-params-stable " --target x86_64-unknown-linux-gnu"))
  (add-to-list 'flycheck-checkers 'rustic-clippy)
  (delete 'rust-clippy flycheck-checkers)
  (delete 'rust-cargo flycheck-checkers)
  (delete 'rust flycheck-checkers))

(after! lsp-rust
  (setq lsp-rust-analyzer-cargo-watch-command "check"))

(map! :after rust-mode
      :map rust-mode-map
      :nvi "<f5>" #'my/rust-compile-on-save
      )
(after! rustic
  (map! :map rustic-mode-map
        "M-j" #'lsp-ui-imenu
        "M-?" #'lsp-find-references
        "C-c C-c C-c" #'rustic-compile
        "C-c C-c l" #'flycheck-list-errors
        "C-c C-c a" #'lsp-execute-code-action
        "C-c C-c r" #'lsp-rename
        "C-c C-c q" #'lsp-workspace-restart
        "C-c C-c Q" #'lsp-workspace-shutdown
        "C-c C-c s" #'lsp-rust-analyzer-status)
  (setq lsp-enable-symbol-highlighting nil)
  (setq rustic-format-trigger nil)
  (add-hook 'rustic-mode-hook 'my/rustic-mode-hook)
  (setq lsp-rust-analyzer-server-display-inlay-hints nil)
  (customize-set-variable 'lsp-ui-doc-enable nil)
  (add-hook 'lsp-ui-mode-hook #'(lambda () (lsp-ui-sideline-enable nil))))


(defun my/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(remove-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(remove-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

(use-package! aggressive-indent
  :defer 3
  :config
  (add-hook 'lisp-mode-hook 'aggressive-indent-mode))

(use-package! sly
  :init
  (with-system windows-nt
    (add-to-list 'exec-path "c:/opt/lisp/sbcl/")
    (setq inferior-lisp-program "sbcl"))
  ;;(add-hook 'lisp-mode-hook 'sly-mode) ;;invoke sly on demand
  )
(use-package! sly-quicklisp)

(after! paren
  (setq show-paren-style nil)  ;; Match the entire expression (optional)
  (set-face-attribute 'show-paren-match nil
                      :underline t       ;; Add underline
                      :foreground "red"    ;; No foreground color
                      :background nil))  ;; No background color

;; https://discourse.doomemacs.org/t/tip-heres-how-to-replace-rainbow-delimiters/3307
(fset 'rainbow-delimiters-mode #'ignore)
(use-package! paren-face
  :config
  (add-hook 'lisp-mode-hook 'paren-face-mode)
  (add-hook 'emacs-lisp-mode-hook 'paren-face-mode))


(use-package! lispy
  :hook ((emacs-lisp-mode
          lisp-mode
          lisp-data-mode
          clojure-mode
          scheme-mode
          racket-mode) . lispy-mode)
  :config
  (setq lispy-no-space t))  ;; Example: prevent spaces after parentheses

;; Enable lispyville for enhanced editing
(use-package! lispyville
  :hook (lispy-mode . lispyville-mode)
  :config
  ;; Configure Lispyville key themes
  (lispyville-set-key-theme
   '(operators          ; Enable d/y/c as operators
     c-w                ; Add better killing behavior
     additional         ; Extra lispy functionality
     slurp/barf-cp      ; Structural editing for slurping/barfing
     commentary         ; Commenting functionality
     movement)))        ; Vim-style movement with h/j/k/l

(use-package! rustic
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save

  (map! :leader
        :desc "exec cargo run"
        "r" #'rustic-cargo-run)
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(setq  leet-daily-dir "c:/dev/leet/daily/")
(setq  leet-source-dir "c:/dev/leet/source/")

(defun leet/start-daily-practise()
  (interactive)
  (let* ((date-string (format-time-string "%Y-%m-%d"))
         (title (completing-read "problem name : " nil nil nil "leetcode-practise"))
         (file-name (concat leet-daily-dir date-string "-" title ".org")))
    (delete-other-windows)
    (find-file file-name)

    ;; Insert template content
    (insert (format "#+TITLE: %s\n" title))
    (insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M")))

    (split-window-right)
    (find-file (concat leet-source-dir "start-here.org"))))

(with-eval-after-load 'counsel
  (when (eq system-type 'windows-nt)
    (setq counsel-locate-cmd 'counsel-locate-cmd-es)
    (defun counsel-locate-cmd-es (input)
      "Return a shell command based on INPUT."
      (format "c:/opt/localbin/es.exe  -n 30 -p -r %s"
              (counsel--elisp-to-pcre
               (ivy--regex input t)))))
  ;;https://github.com/abo-abo/swiper/issues/1218
  (setq ivy-dynamic-exhibit-delay-ms 1000)
  (map! :leader
        :desc "voidtools everything search"
        "s f" #'counsel-locate))

(map! :leader
      :desc "New journal entry"
      "o W" #'my/pick-wiki-name)

(setq wiki-root "C:\\Users\\gopinat\\Dropbox\\emacs-apps\\wikis")

(defun my/pick-wiki-name-action-list-candidates (str pred _)
  (setq wiki-list  (cl-delete-if (lambda (k) (string-match-p "^\\." k))
                                 (directory-files wiki-root))))

(defun my/open-wiki (wiki-root-arg wiki-name)
  (if(file-directory-p wiki-root-arg)
      (progn
                                        ;(persp-mode t)
                                        ;(persp-frame-switch wiki-name)
        (delete-other-windows)
        (find-file  (concat wiki-root-arg "/" wiki-name "/contents/index.org"))
        (split-window-right 30)
        (find-file-other-window (concat wiki-root-arg "/" wiki-name "/tmp/" wiki-name "-" "inbox.org"))
        (when (file-exists-p  (concat wiki-root-arg "/" ".config.el"))
          (load-file  (concat wiki-root-arg "/" ".config.el"))))
    (message "Wiki not found %s" wiki-root-arg)))

(defun my/pick-wiki-name-action (x)
  (my/open-wiki  wiki-root x))

(defun my/pick-wiki-name ()
  "pick a wiki from dropbox folder."
  (interactive)
  (ivy-read "List of wikis: "  #'my/pick-wiki-name-action-list-candidates
            :preselect (ivy-thing-at-point)
            :require-match t
            :action #'my/pick-wiki-name-action
            :caller 'my/pick-wiki-name))

(setq my-work-proj-dir "c:/my/work/work.projects/")
(defun my/pick-work-projects-name-action-list-candidates (str pred _)
  (setq wiki-list  (cl-delete-if (lambda (k) (string-match-p "^\\." k))
                                 (directory-files my-work-proj-dir)))
  (sort wiki-list 'string>))

(defun my/open-work-project (work-proj-root-arg work-proj-name)
  (if(file-directory-p work-proj-root-arg)
      (progn
        (delete-other-windows)
        (find-file  (concat work-proj-root-arg "/" work-proj-name "/" work-proj-name "-index.org")))
    (message "project not found %s" work-proj-root-arg)))

(defun my/pick-work-projects-name-action (x)
  (my/open-work-project my-work-proj-dir x))

(defun my/pick-work-projects-name ()
  "pick a wiki from dropbox folder."
  (interactive)
  (ivy-read "List of projects: "  #'my/pick-work-projects-name-action-list-candidates
            :preselect (ivy-thing-at-point)
            :require-match t
            :action #'my/pick-work-projects-name-action
            :caller 'my/pick-work-projects-name))

(defun my/create-an-entry-in-org-roam (project-name file-path)
  "Insert an entry for PROJECT-NAME with FILE-PATH into active-projects.org, using Org properties."
  (let ((active-projects-path "c:/my/org-farm/work.ord/projects/active-projects.org")
        (status "In Progress")
        (created-date (format-time-string "[%Y-%m-%d %a]")))
    (with-temp-buffer
      ;; Read the existing file
      (insert-file-contents active-projects-path)
      ;; Move to end of file
      (goto-char (point-max))
      ;; Insert new entry with Org properties
      (insert (format "\n* [[file:%s][%s]]\n" file-path project-name))
      (insert ":PROPERTIES:\n")
      (insert (format ":STATUS: %s\n" status))
      (insert (format ":CREATED: %s\n" created-date))
      (insert ":END:\n")
      ;; Write back to file
      (write-region (point-min) (point-max) active-projects-path))))

(defun my/create-projects (root-dir)
  "Create a file with date stamp and title as its name under the project directory and open it for editing."
  (interactive "DDirectory: ")
  (let* ((project (read-from-minibuffer "Project name: "))
         (project_extension (completing-read "Directory Extension: " '("project" "task" "tmp" "thoughts")))
         (date-stamp (format-time-string "%Y-%m-%d"))
         (project-dir (concat (file-name-as-directory root-dir) date-stamp "-"
                              (replace-regexp-in-string "[^[:alnum:]]" "-"
                                                        (downcase project))
                              "." project_extension))
         (file-name (concat date-stamp "-" (replace-regexp-in-string "[^[:alnum:]]" "-" (downcase project)) "." project_extension "-index.org"))
         (file-path (concat (file-name-as-directory project-dir) file-name)))
    (unless (file-directory-p project-dir)
      (make-directory project-dir))
    (my/create-an-entry-in-org-roam project file-path)
    (find-file file-path)
    (save-buffer)))

(defun my/create-projects-wrapper()
  (interactive)
  (my/create-projects my-work-proj-dir))

(map! :leader
      :desc "create or open projects"
      "<f2> c" #'my/create-projects-wrapper
      "<f2> o" #'my/pick-work-projects-name)

(defun path-to-windows (path)
  (replace-regexp-in-string "/" "\\" path t t))

(defun my/open-path-in-explorer ()
  "Open the current buffer's file in Explorer."
  (interactive)
  "Open current file/directory in explorer."
  (interactive)
  (if-let ((path (or buffer-file-name default-directory)))
      (start-process "explorer" nil "explorer"
                     (path-to-windows (file-name-directory path)))
    (error "No file/directory found")))

(defun my/open-project-in-explorer ()
  "Open project root in explorer."
  (interactive)
  (if-let ((root (doom-project-root)))
      (start-process "explorer" nil "explorer"
                     (path-to-windows root))
    (error "Not in a project")))

;; IntelliJ integration
(defvar idea-path "C:/Program Files/JetBrains/IntelliJ IDEA Community Edition 2023.2.4/bin/idea64.exe")

(defun my/open-in-idea ()
  "Open current file in IntelliJ."
  (interactive)
  (if-let ((path buffer-file-name))
      (start-process "idea" nil idea-path (path-to-windows path))
    (error "No file found")))

(defun my/open-project-in-idea ()
  "Open project in IntelliJ."
  (interactive)
  (if-let ((root (doom-project-root)))
      (start-process "idea" nil idea-path (path-to-windows root))
    (error "Not in a project")))

(setq web-bookmarks-file "~/org/bookmarks.org")
(defun my/add-web-bookmark ()
  "Add URL with description to bookmarks.org"
  (interactive)
  (let ((url (read-string "URL: "))
        (desc (read-string "Description: ")))
    (with-current-buffer (find-file-noselect web-bookmarks-file)
      (goto-char (point-max))
      (insert (format "- [[%s][%s]]\n" url desc))
      (save-buffer))))

(map!
 "C-c o e" #'my/open-path-in-explorer
 "C-c o E" #'my/open-project-in-explorer
 "C-c o i" #'my/open-in-idea
 "C-c o I" #'my/open-project-in-idea
 "C-c o b" #'my/add-web-bookmark)

(setq trading-wiki-root "c:/Users/gopinat/Dropbox/emacs-apps/wikis/trading-wiki/")
(defun my/chartgallery/add-entry-to-index(it)
  (save-excursion
    (find-file (concat trading-wiki-root "contents/chart-gallery/chart-gallery-index.org"))
    (end-of-buffer)
    (org-insert-heading)
    (insert (read-string "Enter comments for the screenshot :"
                         (concat (format-time-string "%Y-%m-%d-%a-"))))
    (insert "\n#+ATTR_ORG: :width 400\n[[file:"  it "]]" "\n")
    (org-display-inline-images)))


(defun my/trading/save-screenshot-to-chart-gallery()
  (interactive)
  (save-excursion
    (setq screenshot-file-name
          (concat (my/clean-spaces-from-path
                   (read-string "Enter file name :"
                                (concat (format-time-string "%Y-%m-%d-%a-"))))
                  ".png"))
    (setq chart-gallery-path
          (concat trading-wiki-root "contents/chart-gallery/" (format-time-string "%Y/%Y-%m-%b/")))
    (make-directory chart-gallery-path :parents)
    (setq myvar/img-Abs-Path (replace-regexp-in-string "/" "\\" (concat chart-gallery-path screenshot-file-name)  t t)) ;Relative to workspace.

    (call-process "c:\\opt\\irfanview\\i_view32.exe" nil nil nil (concat "/clippaste /convert="  myvar/img-Abs-Path))
    (setq myvar/relative-filename (concat "./"   (format-time-string "%Y/%Y-%m-%b/") screenshot-file-name))
    (my/chartgallery/add-entry-to-index myvar/relative-filename)
    (org-insert-link 0 (concat "file:" myvar/img-Abs-Path) nil)
    (org-display-inline-images)))

(defun my/trading/create-trading-journal-entry ()
  (interactive)
  (setq trade-journal-dir
        (concat
        "c:/Users/gopinat/Dropbox/emacs-apps/wikis/trading-wiki/contents/trading/journal/2022/"
         (format-time-string "%Y-%m-%b/")))
  (setq myvar/file-name
        (concat (my/clean-spaces-from-path
                 (read-string "Enter Coments for the day :"
                              (concat (format-time-string "%y%m%d-%a-")) nil  nil))
                ".org"))

  (find-file (concat trade-journal-dir myvar/file-name)))

(defun zk/update-timestamp ()
  "Update the :UPDATED: property only if content has changed."
  (when (and (derived-mode-p 'org-mode)
             (org-entry-get (point) "CUSTOM_ID")
             (buffer-modified-p))  ; Only if buffer is modified
    (org-set-property "UPDATED" (format-time-string "%Y-%m-%d %H:%M:%S"))))

;(add-hook 'before-save-hook 'zk/update-timestamp)


(defun zk/get-id-by-title (title)
  "Get the CUSTOM_ID of a Zettel by TITLE in the current Org buffer.
Returns the CUSTOM_ID if found, otherwise nil."
  (let (custom-id)
    (org-map-entries
     (lambda ()
       (when (and (string= (org-get-heading t t t t) title)
                  (org-entry-get (point) "CUSTOM_ID"))
         (setq custom-id (org-entry-get (point) "CUSTOM_ID"))))
     nil 'file)
    custom-id))

(defun zk/get-all-titles ()
  "Get all Zettel titles (headings) in the current Org buffer."
  (when (derived-mode-p 'org-mode)
    (org-map-entries
     (lambda () (org-get-heading t t t t))
     nil 'file)))

(defun zk/insert-link-by-title ()
  "Search for a Zettel by title and insert a link to its CUSTOM_ID."
  (interactive)
  (if-let* ((titles (zk/get-all-titles)))
      (let* ((title (completing-read "Zettel title: " titles))
             (id (zk/get-id-by-title title)))
        (if id
            (insert (format "[[#%s][%s]]" id title))
          (message "No CUSTOM_ID found for title: %s" title)))
    (message "No Zettel titles found in the current buffer.")))

(defun zk/copy-link-to-zettel ()
  "Create and copy a link to the current Zettel."
  (interactive)
  (let ((id (org-entry-get (point) "CUSTOM_ID"))
        (title (org-get-heading t t t t)))
    (if id
        (progn
          (kill-new (format "[[#%s][%s]]" id title))
          (message "Copied link: [[#%s][%s]]" id title))
      (message "No CUSTOM_ID found for this heading."))))

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun my/replace-garbage-chars ()
"Replace non-rendering MS and other garbage characters with latin1 equivalents."
(interactive)
(save-excursion             ;save the current point
(replace-string "\221" "`" nil (point-min) (point-max))
(replace-string "\222" "'" nil (point-min) (point-max))
(replace-string "\226" "-" nil (point-min) (point-max))
(replace-string "\227" "--" nil (point-min) (point-max))
(replace-string "\223" "(" nil (point-min) (point-max))
(replace-string "\224" ")" nil (point-min) (point-max))
(replace-string "\205" "..." nil (point-min) (point-max))
(replace-string "\225" "-" nil (point-min) (point-max))
(replace-string "\344" "" nil (point-min) (point-max))
(replace-string "\374" "" nil (point-min) (point-max))
(replace-string "\337" "" nil (point-min) (point-max))
(replace-string "\366" "" nil (point-min) (point-max))
(replace-string "\247" "***" nil (point-min) (point-max))
(replace-string "\267" "****" nil (point-min) (point-max))
));end replace-garbage-characters
;bind-key replace-garbage-characters
(bind-key  "\C-cr"  'my/replace-garbage-chars)

(defun set-proxy()
  (interactive)
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10.*\\)")
          ("http" . "15.122.63.30:8080")
          ("https" . "15.122.63.30:8080"))))

(defun unset-proxy()
  (interactive)
  (setq url-proxy-services nil)
  (setenv "HTTP_PROXY" "")
  (setenv "HTTPS_PROXY" "")
)

(defun server-shutdown ()
"Save buffers, Quit, and Shutdown (kill) server"
(interactive)
(save-some-buffers)
(kill-emacs)
)

(defun gs/volatile-kill-buffer ()
  "Kill current buffer unconditionally."
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))
    (delete-window)))

(global-set-key (kbd "C-x k") 'gs/volatile-kill-buffer)

(defun gs/vsplit-previous-buff ()
  "find file and close previous file"
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-x 2")   'gs/vsplit-previous-buff)

(defun gs/hsplit-previous-buff ()
  "find file and close previous file"
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(global-set-key (kbd "C-x 3")   'gs/hsplit-previous-buff)

(defun my/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; https://github.com/magnars/.emacs.d/blob/master/defuns/buffer-defuns.el
(defun my/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
         (next-win-buffer (window-buffer (next-window)))
         (this-win-edges (window-edges (selected-window)))
         (next-win-edges (window-edges (next-window)))
         (this-win-2nd (not (and (<= (car this-win-edges)
                     (car next-win-edges))
                     (<= (cadr this-win-edges)
                     (cadr next-win-edges)))))
         (splitter
          (if (= (car this-win-edges)
             (car (window-edges (next-window))))
          'split-window-horizontally
        'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1))))))

(defun my/string-utils/convert-backward-slash-to-forward-slash ()
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (while (search-forward "\\" nil t)
        (replace-match "/" nil t)))))

(defun my/load-helpers()
  (interactive)
  (load "~/emacstools/load-helpers.el"))

(defun my/open/config-org ()
  (interactive)
  (split-window-right)
  (find-file "~/.doom.d/config.org"))

(defun my/open/work-rum-standup-org ()
  (interactive)
  (split-window-right)
  (find-file "c:/my/work/gitrepos/rum-work-notes.git/contents/private/standups/this-month-standups.org"))

(defun my/open/work-org-repo-projects-view ()
  (interactive)
  (delete-other-windows)
  (find-file "c:/my/org-farm/work.ord/tasks/my-tasks.org")
  (split-window-right)
  (find-file "c:/my/org-farm/work.ord/projects/active-projects.org")
  (my/org-roam-switch-repo-by-path "c:/my/org-farm/work.ord"))

(defun my/open/work-org-repo-review-view ()
  (interactive)
  (delete-other-windows)
  (find-file "c:/my/org-farm/work.ord/review/weekly-review.org")
  (split-window-right)
  (find-file "c:/my/org-farm/work.ord/projects/active-projects.org")
  (my/org-roam-switch-repo-by-path "c:/my/org-farm/work.ord"))


(map! :leader
      :desc "Speed dial to to file"
      "0" #'my/open/config-org
      "1" #'my/open/work-org-repo-projects-view
      "2" #'my/open/work-org-repo-review-view
      "<f2>1" #'my/open/work-org-rep
      )

(load (expand-file-name "emops/core.el" doom-user-dir))

(with-system windows-nt
  (progn
  (cd "c:/my/tmp")
  (find-file "c:/my/emacs/start-page.org")))
