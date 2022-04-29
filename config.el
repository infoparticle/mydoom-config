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
(setq myfont "Fira Code Medium"  myfontsize 17)
;(setq myfont "Iosevka Medium"  myfontsize 20)
(setq doom-font (font-spec :family myfont :size myfontsize :weight 'medium)
       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;;(setq doom-font (font-spec :family "Fira Code Medium" :size 17 :weight 'medium)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one-light)
(setq doom-theme 'doom-zenburn)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/orgagenda")
;(setq org-agenda-root-dir "~/org/orgagenda")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)
(tab-bar-mode)

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

(setq
time-stamp-active t          ; do enable time-stamps
time-stamp-pattern "34/\\(\\(L\\|l\\)ast\\( \\|-\\)\\(\\(S\\|s\\)aved\\|\\(M\\|m\\)odified\\|\\(U\\|u\\)pdated\\)\\|Time-stamp\\) *: [\"]%b %02d, %Y %a[\"]")


;; can also add this to source code: // (set-variable time-stamp-format "%04y-%:b-%02d %02H:%02M:%02S")

(add-hook 'before-save-hook 'time-stamp)  ; update time stamps when saving

;; https://emacs.stackexchange.com/questions/62720/open-org-link-in-the-same-window
;; https://emacs.stackexchange.com/questions/16652/change-the-behavior-of-org-mode-auto-expand-relative-path-in-link
(after! org
  (setq org-hide-emphasis-markers t)
  (setq org-link-file-path-type 'relative) ;; insert relative links in org-insert-link
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file-other-window))

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
(shell . t)
(ledger . t)
(plantuml . t)
(napkin . t)
(gnuplot . t)
(haskell . t)
(java . t)
(dot . t)
(sql . t)))

(use-package! ob-napkin :ensure t
              :init
              (with-eval-after-load 'ob
  ;; Optional for syntax highlight of napkin-puml src block.
  ;; (require 'plantuml)
  (require 'ob-napkin)))

(setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/.local/jars/plantuml.jar"))

;; avoid tangling into dos eol in linux files edited using tramp
(add-hook 'org-babel-pre-tangle-hook (lambda () (setq coding-system-for-write 'utf-8-unix)))

(setq my-org-todo-file "~/org/orgagenda/todo.org")
;(setq life-agenda-file "~/org/orgagenda/life-inbox.org")
;(setq work-agenda-file "~/org/orgagenda/work-inbox.org")

(use-package! doct
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
                       :children (("general"
                                   :keys "g"
                                   :file "~/org/journal/general-journal.org"
                                   :template ("* %?" "%U")
                                   :datetree t)
                                  ("apm-journal"
                                   :keys "a"
                                   :file "c:/my/work/apm-bpm/apmbpm.git/private/agenda/apm-journal.org"
                                   :template ("* %?" "%U")
                                   :datetree t)
                                  ))

                      ))))

(global-set-key (kbd "C-c a") 'org-agenda-list)

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

(use-package! atomic-chrome :ensure t)

(use-package! org-id)
(use-package! org-super-links
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
  :hook (org-mode . org-appear-mode)
  :config
  (setq  org-appear-autoemphasis t
        org-appear-autosubmarkers t
        org-appear-autolinks nil)
  ;; for proper first-time setup, `org-appear--set-elements'
  ;; needs to be run after other hooks have acted.
  (run-at-time nil nil #'org-appear--set-elements))

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

(setq trading-wiki-root "c:/Users/gopinat/Dropbox/emacs-apps/wikis/trading-wiki/")
(defun my/chartgallery/add-entry-to-index(it)
  (save-excursion
    (find-file (concat trading-wiki-root "contents/chart-gallery/chart-gallery-index.org"))
    (end-of-buffer)
    (org-insert-heading)
    (insert (read-string "Enter comments for the screenshot :"
                         (concat (format-time-string "%Y-%m-%d-%a-"))))
    (insert "\n#+ATTR_ORG: :width 400\n[[file:"  it "]]" "\n")
    (org-display-inline-images))
  )


(defun my/save-screenshot-to-chart-gallery()
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
    (org-display-inline-images)
    )
  )

(setq org-roam-directory "c:/my/org-roam")

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

(setq-default ispell-program-name "C:/opt/hunspell/bin/hunspell.exe")

;; "en_US" is key to lookup in `ispell-local-dictionary-alist`, please note it will be passed   to hunspell CLI as "-d" parameter
(setq ispell-local-dictionary "en_US")

(setq ispell-hunspell-dict-paths-alist
      '(("en_US" "C:/opt/hunspell/dict/en_US.aff")))

(setq ispell-local-dictionary "en_US")
(setq ispell-local-dictionary-alist
      '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))

(setq text-mode-hook '(lambda() (flyspell-mode t)))

(use-package! super-save
  :ensure t
  :config
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil)
  (setq super-save-remote-files nil)
  (setq super-save-exclude '(".gpg"))
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (super-save-mode +1))

(setq dired-recursive-copies (quote always)) ;no asking
(setq dired-recursive-deletes (quote top)) ; ask once
(setq dired-dwim-target t)

;hide details
(defun xah-dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'xah-dired-mode-setup)

(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file

; put directories first
(setq ls-lisp-dirs-first t)
(setq dired-recursive-deletes 'top)
(setq dired-listing-switches "-hal")
(setq diredp-hide-details-initially-flag nil)

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
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

;(doom-themes-neotree-config)
;(setq doom-themes-neotree-file-icons t)

(use-package! beacon
  :defer t
  :config
  (setq beacon-push-mark 35)
  (setq beacon-color "#666600")
  (beacon-mode 1))

(use-package! skeletor)

(defun my/get-gist ()
  (interactive)
  (find-file "~/emacstools/code-gists/my-code-gists.org")
  (counsel-org-goto)
  (search-forward "#+begin_src")
  (org-edit-src-code)
  (clipboard-kill-region (point-min) (point-max))
  (org-edit-src-abort)
  (kill-buffer)
  (yank))

 (use-package! highlight-symbol
        :ensure t
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

(setq JAVA_BASE "c:/opt/jdks")

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

(with-eval-after-load 'counsel
  (when (eq system-type 'windows-nt)
    (setq counsel-locate-cmd 'counsel-locate-cmd-es)
    (defun counsel-locate-cmd-es (input)
      "Return a shell command based on INPUT."
      (format "c:/opt/localbin/es.exe  -p -r %s"
              (counsel--elisp-to-pcre
               (ivy--regex input t))))))

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

(defun my/create-trading-journal-entry ()
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



(map! :leader
      :desc "Speed dial to to file"
      "0" #'my/open/config-org
      "1" #'my/open/work-rum-standup-org
      )
