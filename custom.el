(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("24de61f5bff32b4d505c804f3cc12e0cfd5c0af05ef790da2cb9ff8d913d9195" "0f0e3de6e1599045abbd5e56a5d1ca686900fd564d27866b1269617c199559f0" "b616b5b7a808c4d46a90549bca2f4dcb5bff4f0e04ddaece5611b186f7e9de53" "039b086d595ebc1e32a33039dafa2003569ede1abb3c0022cc35974887b4c58f" "e8bbe184ba98a77000a75b261993c3aa4d24e0d1224a96784315b0dae1d6f64a" default))
 '(doom-themes-neotree-enable-closed-chevron-icons t)
 '(doom-themes-neotree-enable-folder-icons t)
 '(ledger-binary-path "hledger")
 '(safe-local-variable-values
   '((eval progn
           (pp-buffer)
           (indent-buffer))
     (eval progn
           (setq-local org-roam-directory
                       (locate-dominating-file default-directory ".dir-locals.el"))
           (setq-local org-roam-db-location
                       (concat org-roam-directory "org-roam.db")))
     (org-roam-db-location expand-file-name "./org-roam.db")
     (org-roam-directory expand-file-name "."))))

;(custom-set-faces
; '(cursor ((t (:background "gold" :foreground "#151718"))))
; '(mode-line ((t (:background "black" :foreground "#4499FF"))))
; '(neo-dir-link-face ((t (:family "Fira Code Medium" :height 120))))
; '(neo-file-link-face ((t (:family "Fira Code Medium" :size 24 )))))

(put 'customize-face 'disabled nil)
(put 'customize-variable 'disabled nil)
(put 'customize-group 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'projectile-ripgrep 'disabled nil)
(put 'projectile-grep 'disabled nil)
