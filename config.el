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
(setq doom-theme 'doom-one-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/orgagenda")
;(setq org-agenda-root-dir "~/org/orgagenda")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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
(after! org
(setf (cdr (assoc 'file org-link-frame-setup)) 'find-file-other-window))

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

(setq org-agenda-inhibit-startup t) ;; ~50x speedup
(setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup

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
  (insert (concat (read-string (format"Enter Image Header (%s): " myvar/img-name) nil nil  (concat (format-time-string "%Y-%m-%d"))) "\n"))
  ;;(insert "\n[[file:" (url-encode-url myvar/relative-filename) "]]" "\n")
  (insert "#+ATTR_ORG: :width 600\n[[file:"  myvar/relative-filename "]]" "\n"))

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

(doom-themes-neotree-config)
(setq doom-themes-neotree-file-icons t)

(use-package! skeletor)
