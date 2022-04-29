(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "a82fd8a6b8fbf0ef6ab6fe51d4fb758a7112fe743f928c457d8da6b3d5cc12f4" "51bff19f142e39bde517a1af0d0d1c3bb0ac208d11dbbf33fc26b91f68470317" "0a60c6265487ddcf3e4cf9c430a390abcbd7b39b998672fe3a47edf62a346050" "800d55c1d97eb9e93efdeb6ccda10b8da867a7fc812ab6b8a4c03a545be0887f" "b732f05b0391fcb3b50e8b79d78c6b903fac7cc894d528eb06408d9bc6da5e67" "92591f7527410be0666c6c65db3d4bf7346cf94595666a4ee0be356f4a5063b0" "24de61f5bff32b4d505c804f3cc12e0cfd5c0af05ef790da2cb9ff8d913d9195" "0f0e3de6e1599045abbd5e56a5d1ca686900fd564d27866b1269617c199559f0" "b616b5b7a808c4d46a90549bca2f4dcb5bff4f0e04ddaece5611b186f7e9de53" "039b086d595ebc1e32a33039dafa2003569ede1abb3c0022cc35974887b4c58f" "e8bbe184ba98a77000a75b261993c3aa4d24e0d1224a96784315b0dae1d6f64a" default))
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
     (org-roam-directory expand-file-name ".")))
 '(warning-suppress-types '((org-element-cache))))

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
