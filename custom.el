(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "kubernetes")
      tramp-kubernetes-connection-local-default-profile)
     ((:application eshell) eshell-connection-default-profile)
     ((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp) tramp-connection-local-default-system-profile
      tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-kubernetes-connection-local-default-profile
      (tramp-config-check . tramp-kubernetes--current-context-data)
      (tramp-extra-expand-args 97
       (tramp-kubernetes--container (car tramp-current-connection)) 104
       (tramp-kubernetes--pod (car tramp-current-connection)) 120
       (tramp-kubernetes--context-namespace (car tramp-current-connection))))
     (eshell-connection-default-profile (eshell-path-env-list))
     (tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin"
                         "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin"
                         "/local/bin" "/local/freeware/bin" "/local/gnu/bin"
                         "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin"
                         "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "state=abcde" "-o"
                                        "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number)
                                          (user . string) (egid . number)
                                          (comm . 52) (state . 5)
                                          (ppid . number) (pgrp . number)
                                          (sess . number) (ttname . string)
                                          (tpgid . number) (minflt . number)
                                          (majflt . number)
                                          (time . tramp-ps-time) (pri . number)
                                          (nice . number) (vsize . number)
                                          (rss . number) (etime . tramp-ps-time)
                                          (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o"
                                        "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o" "stat=abcde" "-o"
                                        "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format (pid . number) (user . string)
                                          (group . string) (comm . 52)
                                          (state . 5) (ppid . number)
                                          (pgrp . number) (ttname . string)
                                          (time . tramp-ps-time) (nice . number)
                                          (etime . tramp-ps-time) (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o"
                                        "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                        "-o"
                                        "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format (pid . number) (euid . number)
                                          (user . string) (egid . number)
                                          (group . string) (comm . 52)
                                          (state . string) (ppid . number)
                                          (pgrp . number) (sess . number)
                                          (ttname . string) (tpgid . number)
                                          (minflt . number) (majflt . number)
                                          (time . tramp-ps-time) (pri . number)
                                          (nice . number) (vsize . number)
                                          (rss . number) (etime . number)
                                          (pcpu . number) (pmem . number) (args)))
     (tramp-connection-local-default-shell-profile (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile (path-separator . ":")
      (null-device . "/dev/null"))))
 '(custom-safe-themes
   '("7ec8fd456c0c117c99e3a3b16aaf09ed3fb91879f6601b1ea0eeaee9c6def5d9"
     "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d"
     "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
     "4990532659bb6a285fee01ede3dfa1b1bdf302c5c3c8de9fad9b6bc63a9252f7"
     "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1"
     "8c7e832be864674c220f9a9361c851917a93f921fedb7717b1b5ece47690c098"
     "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c"
     "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc"
     "6e18353d35efc18952c57d3c7ef966cad563dc65a2bba0660b951d990e23fc07"
     "014cb63097fc7dbda3edf53eb09802237961cbb4c9e9abd705f23b86511b0a69"
     "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738"
     "13096a9a6e75c7330c1bc500f30a8f4407bd618431c94aeab55c9855731a95e1"
     "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392"
     "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb"
     "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290"
     "f5f80dd6588e59cfc3ce2f11568ff8296717a938edd448a947f9823a4e282b66"
     "456697e914823ee45365b843c89fbc79191fdbaff471b29aad9dcbe0ee1d5641"
     "dccf4a8f1aaf5f24d2ab63af1aa75fd9d535c83377f8e26380162e888be0c6a9"
     "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19"
     "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d"
     "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b"
     "29b4f767c48da68f8f3c2bbf0dde2be58e4ed9c97e685af5a7ab7844f0d08b8b"
     "02d422e5b99f54bd4516d4157060b874d14552fe613ea7047c4a5cfa1288cf4f"
     "6f96a9ece5fdd0d3e04daea6aa63e13be26b48717820aa7b5889c602764cf23a"
     "4b026ac68a1aa4d1a91879b64f54c2490b4ecad8b64de5b1865bca0addd053d9"
     "a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2"
     "a589c43f8dd8761075a2d6b8d069fc985660e731ae26f6eddef7068fece8a414"
     "7a424478cb77a96af2c0f50cfb4e2a88647b3ccca225f8c650ed45b7f50d9525"
     "02f57ef0a20b7f61adce51445b68b2a7e832648ce2e7efb19d217b6454c1b644"
     "aec7b55f2a13307a55517fdf08438863d694550565dee23181d2ebd973ebd6b8"
     "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00"
     "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2"
     "512ce140ea9c1521ccaceaa0e73e2487e2d3826cc9d287275550b47c04072bc4"
     "944d52450c57b7cbba08f9b3d08095eb7a5541b0ecfb3a0a9ecd4a18f3c28948"
     "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5"
     "234dbb732ef054b109a9e5ee5b499632c63cc24f7c2383a849815dacc1727cb6"
     "546298a25c93ebd2ee49f9676ab427e5e7adadf49b21341b445d877a4faba368"
     "75700837b982cc235ac5c8e5c4aecb6dd6911b3b6a7663dcfe2771ad26cda225"
     "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53"
     "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb"
     "070dce437039a35f6527bbb612756a99febcb8b49f73594f118b0f25da89c918"
     "eb145d87122c57bd6d7f900aabdd8d81bf89aa0c1e62f23312f42b0d88beb866"
     "3549a2b1f48237d590ce5209fe7293f83d1da4c07ef191678d6c007fc2db151a"
     "16c4fa2decb03a94c3b96b942d191eb9c0870c901267ce7374626b13be26f678"
     "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5"
     "bdb89df7761c4a92035bf6748014fe0ebaeede1b822e9632dc3b77e52ff900e2"
     "cc83320de05481856f04ef113ab6dcf93c5173d099a0b5a30fd9bf45cdfa5b1d"
     "568763a776c2a8478584a1c1337256cbc748d66e5d7552bd9b7a4f46ae2d978d"
     "85aec9d2cf7f707ccfea192317cc0de0cf9fa8413cde78f2f6e401ca04730fab"
     "6041a732c5f03d92ac11dc30dea2ec674d11b1340772387ae327041edceb36cb"
     "34235be96d680dee818d58371286aed3c13441f287156bb03f302c46637bcfca"
     "1a886643ba65ef51c90f25db6ec1cef8939ae68c532fb6d9c9e75b8714d3a26e"
     "86014f0f5b78a610977acc74f127b56c7f0506a5973d872d11752879855d401e"
     "58115a5caf2c19aa48de6562c70dfaec47b429707389e03ef59f22f6f6be65ab"
     "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b"
     "a82fd8a6b8fbf0ef6ab6fe51d4fb758a7112fe743f928c457d8da6b3d5cc12f4"
     "51bff19f142e39bde517a1af0d0d1c3bb0ac208d11dbbf33fc26b91f68470317"
     "0a60c6265487ddcf3e4cf9c430a390abcbd7b39b998672fe3a47edf62a346050"
     "800d55c1d97eb9e93efdeb6ccda10b8da867a7fc812ab6b8a4c03a545be0887f"
     "b732f05b0391fcb3b50e8b79d78c6b903fac7cc894d528eb06408d9bc6da5e67"
     "92591f7527410be0666c6c65db3d4bf7346cf94595666a4ee0be356f4a5063b0"
     "24de61f5bff32b4d505c804f3cc12e0cfd5c0af05ef790da2cb9ff8d913d9195"
     "0f0e3de6e1599045abbd5e56a5d1ca686900fd564d27866b1269617c199559f0"
     "b616b5b7a808c4d46a90549bca2f4dcb5bff4f0e04ddaece5611b186f7e9de53"
     "039b086d595ebc1e32a33039dafa2003569ede1abb3c0022cc35974887b4c58f"
     "e8bbe184ba98a77000a75b261993c3aa4d24e0d1224a96784315b0dae1d6f64a" default))
 '(display-line-numbers-type nil)
 '(doom-themes-neotree-enable-closed-chevron-icons t)
 '(doom-themes-neotree-enable-folder-icons t)
 '(epg-gpg-home-directory "c:/Users/gopinat/AppData/Roaming/gnupg")
 '(epg-gpg-program "C:/Program Files (x86)/GnuPG/bin/gpg.exe")
 '(epg-gpgconf-program "C:/Program Files (x86)/GnuPG/bin/gpgconf.exe")
 '(global-company-mode 0 nil nil "Customized with use-package company")
 '(ledger-binary-path "hledger")
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/ReadTheOrg\\.setup\\'"))
 '(safe-local-variable-values
   '((eval progn (pp-buffer) (indent-buffer))
     (eval progn
      (setq-local org-roam-directory
                  (locate-dominating-file default-directory ".dir-locals.el"))
      (setq-local org-roam-db-location (concat org-roam-directory "org-roam.db")))
     (org-roam-db-location expand-file-name "./org-roam.db")
     (org-roam-directory expand-file-name ".")))
 '(size-indication-mode t)
 '(tool-bar-mode nil)
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
