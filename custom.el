(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("eb145d87122c57bd6d7f900aabdd8d81bf89aa0c1e62f23312f42b0d88beb866" "3549a2b1f48237d590ce5209fe7293f83d1da4c07ef191678d6c007fc2db151a" "16c4fa2decb03a94c3b96b942d191eb9c0870c901267ce7374626b13be26f678" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "bdb89df7761c4a92035bf6748014fe0ebaeede1b822e9632dc3b77e52ff900e2" "cc83320de05481856f04ef113ab6dcf93c5173d099a0b5a30fd9bf45cdfa5b1d" "568763a776c2a8478584a1c1337256cbc748d66e5d7552bd9b7a4f46ae2d978d" "85aec9d2cf7f707ccfea192317cc0de0cf9fa8413cde78f2f6e401ca04730fab" "6041a732c5f03d92ac11dc30dea2ec674d11b1340772387ae327041edceb36cb" "34235be96d680dee818d58371286aed3c13441f287156bb03f302c46637bcfca" "1a886643ba65ef51c90f25db6ec1cef8939ae68c532fb6d9c9e75b8714d3a26e" "86014f0f5b78a610977acc74f127b56c7f0506a5973d872d11752879855d401e" "58115a5caf2c19aa48de6562c70dfaec47b429707389e03ef59f22f6f6be65ab" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "a82fd8a6b8fbf0ef6ab6fe51d4fb758a7112fe743f928c457d8da6b3d5cc12f4" "51bff19f142e39bde517a1af0d0d1c3bb0ac208d11dbbf33fc26b91f68470317" "0a60c6265487ddcf3e4cf9c430a390abcbd7b39b998672fe3a47edf62a346050" "800d55c1d97eb9e93efdeb6ccda10b8da867a7fc812ab6b8a4c03a545be0887f" "b732f05b0391fcb3b50e8b79d78c6b903fac7cc894d528eb06408d9bc6da5e67" "92591f7527410be0666c6c65db3d4bf7346cf94595666a4ee0be356f4a5063b0" "24de61f5bff32b4d505c804f3cc12e0cfd5c0af05ef790da2cb9ff8d913d9195" "0f0e3de6e1599045abbd5e56a5d1ca686900fd564d27866b1269617c199559f0" "b616b5b7a808c4d46a90549bca2f4dcb5bff4f0e04ddaece5611b186f7e9de53" "039b086d595ebc1e32a33039dafa2003569ede1abb3c0022cc35974887b4c58f" "e8bbe184ba98a77000a75b261993c3aa4d24e0d1224a96784315b0dae1d6f64a" default))
 '(doom-themes-neotree-enable-closed-chevron-icons t)
 '(doom-themes-neotree-enable-folder-icons t)
 '(epg-gpg-home-directory "c:/Users/gopinat/AppData/Roaming/gnupg")
 '(epg-gpg-program "C:/Program Files (x86)/GnuPG/bin/gpg.exe")
 '(epg-gpgconf-program "C:/Program Files (x86)/GnuPG/bin/gpgconf.exe")
 '(global-company-mode 0 nil nil "Customized with use-package company")
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
(put 'erase-buffer 'disabled nil)
