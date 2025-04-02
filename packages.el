;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! doct :disable t)
(package! gitconfig-mode
          :recipe (:host github :repo "magit/git-modes"
                         :files ("gitconfig-mode.el")))
(package! gitignore-mode
  :recipe (:host github :repo "magit/git-modes"
           :files ("gitignore-mode.el")))

(package! code-review :disable t)

;(package! atomic-chrome :disable t)
(package! atomic-chrome)

;(package! org-msg)
(package! org-super-links :recipe (:host github
                       :repo "toshism/org-super-links"
                       :files ("*.el")
                       )
  :disable t)

(unpin! org-roam)
(package! emacsql-sqlite
  :recipe (:host github :repo "skeeto/emacsql"))
 
(package! org-roam-ui)
;;(package! org-attach)
(package! org-auto-tangle)
(package! ob-ipython)
(package! ob-mermaid)
;;(package! vterm)

(package! pulsar)
;;(package! hl-line+)
(package! company-box :disable t)
(package! popper)

;; ~/.doom.d/package.el
(package! org-transclusion)
(package! org-appear :disable t)

;;(package! ob-napkin)
(package! org-super-agenda)
(package! org-sidebar)
(package! org-present)
(package! org-modern)
(package! visual-fill-column)
(package! ox-reveal)

;;(package! skeletor)
(package! super-save)
(package! dired-sidebar)
;;(package! zk)

;; coding
(package! ivy-yasnippet)
(package! highlight-symbol)

;(package! sly)
(package! sly-quicklisp)
(package! highlight-sexp)
(package! paren-face)
(package! aggressive-indent)
(package! lispyville)
(package! lispy)
;;(package! symex)
;(package! all-the-icons :disable t)
(package! all-the-icons)
(package! speed-type)
;(package! multiple-cursors)
(package! evil-mc)

(package! key-chord)

;; rust
(package! rust-mode)
(package! rustic)

;;(package! slime)
(package! restclient)
(package! ob-restclient)
(package! ob-powershell)
(package! haskell-mode)
;(package! devdocs)
;(package! tao-theme)
;(package! almost-mono-themes)


(package! emacsql)
(package! emacsql-sqlite)
;;(package! hyperbole :disable t)
(package! hyperbole)
;;(package! denote)
(package! dumb-jump)
(package! citre)

;;(package! esup :disable t)

;(package! helm-posframe)
;(package! centaur-tabs)
;(package! modus-themes)

;; chatGPT!!
;; (package! gptel)

(package! anki-editor)

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))

(package! perspective)
(package! eyebrowse)
