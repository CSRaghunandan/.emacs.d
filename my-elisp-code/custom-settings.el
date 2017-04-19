(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(bmkp-last-as-first-bookmark-file "/Users/csraghunandan/.emacs.d/bookmarks")
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "4fc7f95b03416aa4270cdeefecc45f3153b8ceadef2e8d5722dce63d1bf83400" "227edf860687e6dfd079dc5c629cbfb5c37d0b42a3441f5c50873ba11ec8dfd2" "2997ecd20f07b99259bddba648555335ffb7a7d908d8d3e6660ecbec415f6b95" "9d91458c4ad7c74cf946bd97ad085c0f6a40c370ac0a1cbeb2e3879f15b40553" "20cf456bb8a275221e217521777f1901f6704ef0a3341dcfc31593dbd972aa96" "3d47d88c86c30150c9a993cc14c808c769dad2d4e9d0388a24fee1fbf61f0971" "f508fd10aaaf64978c73036c81afd50a7e5e0ea47c76bdb654f24987335f0b4f" "14f0fbf6f7851bfa60bf1f30347003e2348bf7a1005570fd758133c87dafe08f" "486759384769d44b22bb46072726c2cfb3ccc3d49342e5af1854784d505ffc01" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "3693403316f0127326fa08067c2e3013eda29216829e1478e1656ea4fbbc6560" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "cdbd0a803de328a4986659d799659939d13ec01da1f482d838b68038c1bb35e8" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "44048f3a208ccfa3286b426a995696871e6403d951b23d7b55a1af850d7aec93" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default)))
 '(erc-modules
   (quote
    (autoaway autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notify readonly ring smiley sound stamp spelling track)))
 '(explicit-shell-file-name "/usr/local/bin/bash")
 '(hl-sexp-background-color "#efebe9")
 '(ivy-mode t)
 '(org-agenda-files
   (quote
    ("~/org-mode-files/life.org" "~/org-mode-files/work.org" "~/org-mode-files/math.org" "~/org-mode-files/computer-science.org")))
 '(org-capture-templates
   (quote
    (("c" "Computer Science template")
     ("cb" "Add a book to read list" entry
      (file+headline "~/org-mode-files/computer-science.org" "Read List")
      (file "~/.emacs.d/org-capture-templates/book.txt"))
     ("ct" "Add a TODO entry" entry
      (file+headline "~/org-mode-files/computer-science.org" "captured ideas")
      (file "~/.emacs.d/org-capture-templates/todo.txt"))
     ("l" "Life capture template")
     ("lb" "Add a book to read list" entry
      (file+headline "~/org-mode-files/life.org" "Read List")
      (file "~/.emacs.d/org-capture-templates/book.txt"))
     ("lt" "Add a TODO entry" entry
      (file+headline "~/org-mode-files/life.org" "captured ideas")
      "~/.emacs.d/org-capture-templates/todo.txt")
     ("w" "Work capture template")
     ("wb" "Add a book to read list" entry
      (file+headline "~/org-mode-files/work.org" "Read List")
      (file "~/.emacs.d/org-capture-templates/book.txt"))
     ("wt" "Add a TODO entry" entry
      (file+headline "~/org-mode-files/work.org" "captured ideas")
      (file "~/.emacs.d/org-capture-templates/todo.txt"))
     ("m" "Math capture template")
     ("mb" "Add a book to read list" entry
      (file+headline "~/org-mode-files/math.org" "Read List")
      (file "~/.emacs.d/org-capture-templates/book.txt"))
     ("mt" "Add a TODO entry" entry
      (file+headline "~/org-mode-files/math.org" "captured ideas")
      (file "~/.emacs.d/org-capture-templates/todo.txt")))))
 '(org-modules (quote (org-habit org-info org-irc org-drill)))
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-targets (quote ((org-agenda-files :level . 1))))
 '(org-refile-use-outline-path (quote file))
 '(package-archives
   (quote
    (("melpa" . "https://melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("org" . "http://orgmode.org/elpa/"))))
 '(package-selected-packages
   (quote
    (org-sticky-header transpose-frame indium org-autolist mwim column-enforce-mode python-docstring intero racket-mode mocha-snippets git-link jade dumb-jump golden-ratio-scroll-screen org-plus-contrib diff-hl disable-mouse mocha markdown-toc cycle-quotes tide all-the-icons neotree smart-dash gitattributes-mode gitconfig-mode gitignore-mode org-download org-cliplink shm quickrun ivy-hydra yaml-mode toml-mode highlight-quoted highlight-numbers git-messenger tern js2-mode avy company visual-regexp smartparens auto-yasnippet json-snatcher rust-playground realgud inf-mongo psc-ide purescript-mode origami sicp headlong bookmark+ multi-term ace-link osx-trash ox-gfm ws-butler whole-line-or-region ibuffer-vc rainbow-mode web-beautify hindent sphinx-doc yapfify kurecolor which-key anzu counsel swiper ivy dired-quick-sort org-pomodoro dired+ free-keys recursive-narrow langtool pyvenv bm page-break-lines manage-minor-mode region-bindings-mode wgrep move-text visual-regexp-steroids hydra avy-zap imenu-anywhere company-statistics pytest volatile-highlights org-journal company-anaconda anaconda-mode racer ac-html-bootstrap company-web company-tern cargo rust-mode web-completion-data flycheck-rust f info+ spaceline js2-refactor git-timemachine impatient-mode undo-tree google-this zenburn-theme web-mode use-package smex rainbow-delimiters projectile powerline multiple-cursors markdown-mode magit hungry-delete highlight-symbol haskell-snippets haskell-mode flycheck expand-region exec-path-from-shell emmet-mode beacon auctex ace-window)))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(python-shell-completion-native-enable nil)
 '(send-mail-function (quote sendmail-send-it)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(term ((t (:foreground "#E5D9BD")))))
