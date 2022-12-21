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
  ;(setq myfont "JetBrainsMonoMedium NF"  myfontsize 16)
  ;;(setq myfont "Fira Code Medium"  myfontsize 17)
  (setq myfont "Iosevka"  myfontsize 20)
  (setq doom-font (font-spec :family myfont :size myfontsize :weight 'medium)
         doom-variable-pitch-font (font-spec :family "sans" :size 13))
  ;;(setq doom-font (font-spec :family "Fira Code Medium" :size 17 :weight 'medium)
  ;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))


  ;; There are two ways to load a theme. Both assume the theme is installed and
  ;; available. You can either set `doom-theme' or manually load a theme with the
  ;; `load-theme' function. This is the default:
  ;;(setq doom-theme 'doom-one-light)
  ;;(setq doom-theme 'doom-opera-light)

(setq hl-sexp-foreground-color nil
      hl-sexp-background-color "gray20") ;;light yellow
(setq tao-theme-use-height t
      tao-theme-use-sepia nil
      tao-theme-use-boxes nil)
(setq doom-theme 'doom-zenburn)


;;  (setq hl-sexp-foreground-color nil
;;        hl-sexp-background-color "#00253c") ;;dark blue
;;         hl-sexp-foreground-color "#00253c" ;;light yellow

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
(powershell . t)
(sql . t)))
(setq org-plantuml-jar-path (expand-file-name "~/emacstools/.local/jars/plantuml.jar"))

;; avoid tangling into dos eol in linux files edited using tramp
(add-hook 'org-babel-pre-tangle-hook (lambda () (setq coding-system-for-write 'utf-8-unix)))

(use-package! org-auto-tangle
  :defer 3
  :hook (org-mode . org-auto-tangle-mode))

(setq my-org-todo-file "~/org/orgagenda/todo.org")
;(setq life-agenda-file "~/org/orgagenda/life-inbox.org")
;(setq work-agenda-file "~/org/orgagenda/work-inbox.org")

(use-package! doct
  :defer 3
  :demand t
  :commands (doct)
  :init (setq org-capture-templates
              (doct '(("TODO"
                       :keys "t"
                       :children (("life"
                                   :keys "l"
                                   :template ("* TODO %^{Description}"
                                              ;;"SCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))"
                                              "%^{SCHEDULED}p"
                                              ":PROPERTIES:"
                                              ":Category: %^{Home|Family|Friends|Learnings|Misc}"
                                              ":END:"
                                              )
                                   :headline "Personal Tasks"
                                   :file my-org-todo-file)
                                  ("work"
                                   :keys "w"
                                   :template ("* TODO %^{Description}"
                                              "%^{SCHEDULED}p"
                                              ":PROPERTIES:"
                                              ":Category: %^{sprint|learning|Misc}"
                                              ":Created: %U"
                                              ":END:"
                                              ":LOGBOOK:"
                                              "- State \"TODO\"       from \"\"           %U"
                                              ":END:")
                                   :headline "Work Tasks"
                                   :file my-org-todo-file)))

                      ("Journal"
                       :keys "j"
                       :prepend t
                       :children (("private journal"
                                   :keys "p"
                                   :file "c:/my/org-0.10.d/private/journal/yearly-journal.org.gpg"
                                   :template ("* %?" "%U")
                                   :datetree t
                                   :time-prompt t
                                   :unnarrowed  t
                                   )
                                  ("apm-journal"
                                   :keys "a"
                                   :file "c:/my/work/apm-bpm/apmbpm.git/private/agenda/apm-journal.org"
                                   :template ("* %?" "%U")
                                   :datetree t)
                                  ))

                      ))))

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
(setq org-todo-keywords
    (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING" "EVENT"))))

(setq org-todo-keyword-faces
      (quote (("TODO"      :background "red" :foreground "white" :weight bold)
              ("NEXT"      :background "slate blue" :foreground "white" :weight bold)
              ("DONE"      :background "forest green" :foreground "white" :weight bold)
              ("WAITING"   :background "orange" :foreground "white" :weight bold)
              ("HOLD"      :background "magenta" :foreground "white" :weight bold)
              ("CANCELLED" :background "forest green" :foreground "white" :weight bold)
              ("MEETING"   :background "forest green" :foreground "white" :weight bold)
              ("EVENT"     :background "black" :foreground "white" :weight bold)
              )))
(setq org-todo-state-tags-triggers
    (quote (("CANCELLED" ("CANCELLED" . t))
            ("WAITING" ("WAITING" . t))
            ("HOLD" ("WAITING") ("HOLD" . t))
            (done ("WAITING") ("HOLD"))
            ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
            ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
            ("EVENT" ("WAITING") ("CANCELLED") ("HOLD"))
            ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

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


(global-set-key [f5] 'my/org-screenshot)

(after! org-roam
  (setq org-roam-directory (file-truename "c:/my/org-roam"))

  (setq org-roam-root "c:/my/org-roam/root/")

  (defun my/get-roam-dir-name-action-list-candidates (str pred _)
    (setq roam-dir-list  (cl-delete-if (lambda (k) (string-match-p "^\\." k))
                                       (directory-files org-roam-root))))

  (defun my/set-roam-dir-name-action (x)
    (setq org-roam-directory  (concat org-roam-root x))
    (org-roam-db-sync)
    )

  (defun my/set-org-roam-directory ()
    "pick a wiki from dropbox folder."
    (interactive)
    (ivy-read "List of wikis: "  #'my/get-roam-dir-name-action-list-candidates
              :preselect (ivy-thing-at-point)
              :require-match t
              :action #'my/set-roam-dir-name-action
              :caller 'my/get-roam-dir-name))
  (map! :leader
        :desc "set org-roam directory"
        "n r ." #'my/set-org-roam-directory)
  )

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

(setq text-mode-hook '(lambda() (flyspell-mode t)))

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

(use-package! esup :ensure t)

(global-set-key (kbd "<f2>")  (lambda()(interactive)(switch-to-buffer "*scratch*")))

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
(setq diredp-hide-details-initially-flag nil)
(setq ls-lisp-dirs-first t)

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

;(doom-themes-neotree-config)
;(setq doom-themes-neotree-file-icons t)

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

(defun my/get-gist (filepath)
  (interactive)
  (find-file filepath)
  (counsel-org-goto)
  (search-forward "#+begin_src")
  (org-edit-src-code)
  (clipboard-kill-region (point-min) (point-max))
  (org-edit-src-abort)
  ;(kill-buffer)
  (previous-buffer)
  (yank))

(defun my/get-gist-all()
  (interactive)
  (my/get-gist "~/emacstools/code-gists/code-gists-all.org")
  )

(defun my/get-gist-python()
  (interactive)
  (my/get-gist "~/emacstools/code-gists/code-gists.python.org")
  )

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

(setq yas-snippet-dirs
      '("~/emacstools/snippets"                 ;; personal snippets
        ))

(use-package ivy-yasnippet
  :bind ("C-c y" . ivy-yasnippet))

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq evil-shift-width 4)))

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

(setq growl "C:/Program Files (x86)/Growl for Windows/growlnotify.exe")

(defun get-icon (mood)
  (if (cl-equalp mood "happy")
      "C:/Program Files/ShareX/Stickers/BlobEmoji/googlecatface.png"
    "C:/Program Files/ShareX/Stickers/BlobEmoji/blobfacepalm.png"
    )
  )
(defun my/growl-notify (mood title msg)
  (call-process growl nil nil nil (concat "/t:" title )
                (concat "/i:" (get-icon mood))
                msg))

;;(my/growl-notify "happy" "welcome!" "hello")

(defun my/compile-on-save()
  (interactive)
  (setq response-javac (process-exit-code-and-output "javac" (file-name-nondirectory (buffer-file-name))))
  (if (zerop (nth 0 response-javac))
      (progn
        (setq response-java
              (process-exit-code-and-output
               "java"
               "-cp"
               "."
               (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
        (my/growl-notify "happy" "Output" (nth 1 response-java)))
    (my/growl-notify "sad" "Error" (nth 1 response-javac))
    )
  )


(defun process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process program nil (current-buffer) nil args)
          (buffer-string))))

(define-minor-mode compile-on-save-mode
  "Minor mode to automatically call `recompile' whenever the
current buffer is saved. When there is ongoing compilation,
nothing happens."
  :lighter " CoS"
  (if compile-on-save-mode
      (progn
        (setq super-save-mode nil)
        (make-local-variable 'after-save-hook)
        (add-hook 'after-save-hook 'my/compile-on-save nil t))
    (kill-local-variable 'after-save-hook)))

(use-package! lsp-java
  :init
  (with-system windows-nt
    (setq lsp-java-java-path "C:/Users/gopinat/.jabba/jdk/openjdk@1.11.0/bin/java.exe"))
  :config
  (with-system windows-nt
    (setq lsp-java-configuration-runtimes '[(:name "OpenJDK-11"
                                             :path "C:/Users/gopinat/.jabba/jdk/openjdk@1.11.0"
                                             )
                                            (:name "JavaSE-1.8"
                                             :path "C:/Users/gopinat/.jabba/jdk/adopt@1.8.0-292"
                                             :default t
                                             )])))
(after! lsp-mode
  (advice-remove #'lsp #'+lsp-dont-prompt-to-install-servers-maybe-a))

(add-to-list 'exec-path "C:/tools/ghc-9.2.3/bin")

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
(use-package! sly-quicklisp
  )
(use-package! highlight-sexp
  :config
  (add-hook 'lisp-mode-hook 'highlight-sexp-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode))

(use-package! symex
  :config
  (symex-initialize)
  (global-set-key (kbd "C-c ;") 'symex-mode-interface))  ; or whatever keybinding you like

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
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package! devdocs
  :config
  (add-hook 'python-mode-hook
          (lambda () (setq-local devdocs-current-docs '("python~3.10"))))
  )

(with-eval-after-load 'counsel
  (when (eq system-type 'windows-nt)
    (setq counsel-locate-cmd 'counsel-locate-cmd-es)
    (defun counsel-locate-cmd-es (input)
      "Return a shell command based on INPUT."
      (format "c:/opt/localbin/es.exe  -n 30 -p -r %s"
              (counsel--elisp-to-pcre
               (ivy--regex input t)))))
  ;;https://github.com/abo-abo/swiper/issues/1218
  (setq ivy-dynamic-exhibit-delay-ms 500)
  (map! :leader
        :desc "voidtools everything search"
        "s f" #'counsel-locate))

(map! :leader
      :desc "New journal entry"
      "o w" #'my/pick-wiki-name)

(setq wiki-root "C:\\Users\\gopinat\\Dropbox\\emacs-apps\\wikis")

(defun my/pick-wiki-name-action-list-candidates (str pred _)
  (setq wiki-list  (cl-delete-if (lambda (k) (string-match-p "^\\." k))
                                 (directory-files wiki-root))))
(defun my/open-wiki (wiki-root wiki-name)
  (if(file-directory-p wiki-root)
      (progn
        ;(persp-mode t)
        ;(persp-frame-switch wiki-name)
        (delete-other-windows)
        (find-file  (concat wiki-root "/" wiki-name "/contents/index.org"))
        (split-window-right 30)
        (find-file-other-window (concat wiki-root "/" wiki-name "/tmp/" wiki-name "-" "inbox.org"))
        (when (file-exists-p  (concat wiki-root "/" ".config.el"))
          (load-file  (concat wiki-root "/" ".config.el"))))
    (message "Wiki not found %s" wiki-root)))

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

(defun my/zk-insert-link-into-selection(selected-file start end)
  (if (use-region-p)
      (let ((selected-text (buffer-substring start end)))
        (kill-region start end)
        (org-insert-link 0 (concat "file:" (projectile-project-root) selected-file )
                         selected-text)
        (save-buffer)))
  (setq mark-active nil))

(defun my/zk-add-backlink (selected-file current-file)
  (find-file (concat (projectile-project-root) selected-file))
  (goto-char (point-max))
  (insert "\n- ")
  (org-insert-link 0 (concat "file:" current-file )
                   (concat
                    "<- "
                    (file-name-sans-extension
                     (file-name-nondirectory current-file))))
  (save-buffer))

(defun my/zk-add-see-also (selected-file)
  (goto-char (point-max))
  (insert "\n- ")
  (org-insert-link 0 (concat "file:" (projectile-project-root) selected-file )
                   (concat
                    "-> "
                    (file-name-sans-extension
                     (file-name-nondirectory selected-file))))
  (save-buffer))

(defun my/zk-embed-link-and-add-back-link()
  (interactive)
  (setq current-file (buffer-file-name))
  (setq selected-file (ivy-completing-read
                       "File : "
                       (projectile-current-project-files)))
  (my/zk-insert-link-into-selection selected-file (region-beginning) (region-end))
  (my/zk-add-see-also selected-file)
  (my/zk-add-backlink selected-file current-file))

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

(setq myvar/rum-work-notes-path "c:/my/work/gitrepos/rum-work-notes.git/")

(defun my/work/open-file-in-sidebar (it)
  (split-window-right)
  (find-file (concat myvar/rum-work-notes-path it)))

(defun my/work/open-todo ()
  (interactive)
  (my/work/open-file-in-sidebar "contents/private/todo-for-today.org"))

(bind-key  "\C-cwot"  'my/work/open-todo)

(defun my/work/open-bookmarks ()
  (interactive)
  (my/work/open-file-in-sidebar "contents/bookmarks.org"))

(bind-key  "\C-cwob"  'my/work/open-bookmarks)


(defun my/work/task/create ()
  (interactive)
  (setq myvar/task-dir
        (concat (my/clean-spaces-from-path
                 (read-string "Enter Task for the day :"
                              (concat (format-time-string "%Y-%m-%d-")) nil  nil))
                ".task/"))
  (make-directory (concat myvar/rum-work-notes-path "contents/internal/tasks/" myvar/task-dir) :parents)
  (find-file (concat myvar/rum-work-notes-path "contents/internal/tasks/" myvar/task-dir "index.org")))

(bind-key  "\C-cwtc"  'my/work/task/create)

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

(defun my/open/work-rum-tasks ()
  (interactive)
  (split-window-right)
  (find-file "c:/my/work/gitrepos/rum-work-notes.git/contents/private/rum-tasks.org"))


(defun my/open/org-second-brain ()
  (interactive)
  (split-window-right)
  (find-file "c:/my/org-0.10.d/meta/cat-index.org"))

(map! :leader
      :desc "Speed dial to to file"
      "0" #'my/open/config-org
      "1" #'my/open/work-rum-tasks
      "2" #'my/open/org-second-brain
      )

(with-system windows-nt
  (cd "c:/my/tmp"))
