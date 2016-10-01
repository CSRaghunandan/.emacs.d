;;; -*- lexical-binding: t -*-

(setq gc-cons-threshold 100000000)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; emacs setup file
(add-to-list 'load-path (concat user-emacs-directory "setup-files/"))

(delete-selection-mode t)
(setq-default indent-tabs-mode nil)
(setq-default fill-column 80) ;; default is 70
;; make emacs auto-refresh all buffers when files have changed on the disk
(global-auto-revert-mode t)
;; set all yes or no prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; By default, Emacs thinks a sentence is a full-stop followed by 2 spaces.
(setq sentence-end-double-space nil)

;; Ensure that we can quickly pop the mark several times by typing
;; C-u C-SPC C-SPC, instead of having to type C-u C-SPC C-u C-SPC.
(setq set-mark-command-repeat-pop t)
;; enable disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'set-goal-column  'disabled nil)

;; Kill ring
(setq kill-ring-max 200
      kill-do-not-save-duplicates t
      save-interprogram-paste-before-kill t)
(setq select-enable-primary t)
(setq select-enable-clipboard t)

;; remove all trailing whitespaces in a file after saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq switch-to-visible-buffer nil)
(setq scroll-preserve-screen-position t)

;; theme config ------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-modes
   (quote
    (emacs-lisp-mode lisp-mode lisp-interaction-mode slime-repl-mode nim-mode c-mode cc-mode c++-mode go-mode java-mode malabar-mode clojure-mode clojurescript-mode scala-mode scheme-mode ocaml-mode tuareg-mode coq-mode haskell-mode agda-mode agda2-mode perl-mode cperl-mode ruby-mode lua-mode tcl-mode ecmascript-mode javascript-mode js-mode js-jsx-mode js2-mode js2-jsx-mode php-mode css-mode scss-mode less-css-mode makefile-mode sh-mode fortran-mode f90-mode ada-mode xml-mode sgml-mode web-mode ts-mode sclang-mode verilog-mode qml-mode apples-mode)))
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes
   (quote
    ("0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "cdbd0a803de328a4986659d799659939d13ec01da1f482d838b68038c1bb35e8" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "44048f3a208ccfa3286b426a995696871e6403d951b23d7b55a1af850d7aec93" "bcc6775934c9adf5f3bd1f428326ce0dcd34d743a92df48c128e6438b815b44f" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" default)))
 '(erc-modules
   (quote
    (autoaway autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications readonly ring sound stamp spelling track)))
 '(fci-rule-color "#383838")
 '(google-this-modeline-indicator " ")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-log-into-drawer t)
 '(org-log-redeadline (quote note))
 '(org-log-reschedule (quote note))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m)))
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
    (wgrep region-bindings-mode move-text fold-dwim highlight-indent-guides indent-guide visual-regexp-steroids hydra aggressive-indent osx-trash avy-zap dired-hacks-utlis flyspell-correct-ivy ibuffer-projectile electric-operator reveal-in-osx-finder imenu-anywhere flx counsel swiper company-statistics pytest racket racket-mode volatile-highlights org-journal intero ac-html-angular company-anaconda anaconda-mode racer ac-html-bootstrap company-web company-tern cargo rust-mode web-completion-data flycheck-rust f info+ spaceline diff-hl flyspell-mode org-plus-contrib misc-cmds dired-narrow js2-refactor git-timemachine impatient-mode undo-tree google-this zenburn-theme web-mode use-package smex smartparens rainbow-delimiters projectile powerline multiple-cursors markdown-mode magit hungry-delete highlight-symbol haskell-snippets haskell-mode goto-chg flycheck expand-region exec-path-from-shell emmet-mode company-quickhelp beacon auctex ace-window)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(python-shell-completion-native-enable nil)
 '(send-mail-function (quote sendmail-send-it))
 '(sp-ignore-modes-list (quote (minibuffer-inactive-mode web-mode org-mode)))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight-symbol-face ((t (:background "DarkSeaGreen4"))))
 '(org-document-title ((t (:height 1.4))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
 '(org-level-1 ((t (:inherit outline-1 :foreground "#DFAF8F" :height 1.2))))
 '(org-level-2 ((t (:inherit outline-2 :foreground "#BFEBBF" :height 1.15))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "#7CB8BB" :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :foreground "#D0BF8F" :height 1.05))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "#93E0E3" :height 1))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark grey"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "gold"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#93E0E3"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-6-face ((t nil)))
 '(vhl/default-face ((t (:background "gray40")))))

;; all use packages declarations -----------------------------------------------
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package
(require 'use-package)
(setq use-package-always-ensure t)

(require 'setup-pragmatapro)
(require 'setup-org)
(require 'setup-backup)
(require 'setup-search)
(require 'setup-ibuffer)
(require 'setup-recentf)
(require 'setup-desktop)
(require 'setup-calc)
(require 'setup-ediff)
(require 'setup-white-space)
(require 'setup-dired)
(require 'setup-elisp-mode)
(require 'setup-fly)
(require 'setup-bookmark)
(require 'setup-hydra)
(require 'setup-company)
(require 'setup-beacon)
(require 'setup-smartparens)
(require 'setup-mc)
(require 'setup-git-stuff)
(require 'setup-undo-tree)
(require 'setup-avy)
(require 'setup-smex)
(require 'setup-projectile)
(require 'setup-web-mode)
(require 'setup-yas)
(require 'setup-ivy)
(require 'setup-counsel)
(require 'setup-swiper)
(require 'setup-movement)
(require 'setup-js)
(require 'setup-markdown)
(require 'setup-highlight)
(require 'setup-mode-line)
(require 'setup-racket)
(require 'setup-hungry-delete)
(require 'setup-rust)
(require 'setup-haskell)
(require 'setup-python)
(require 'setup-tex)
(require 'setup-osx)
(require 'setup-misc)
(require 'setup-fold)
(require 'setup-buffers)
(require 'setup-theme)
(require 'setup-aggresive-indent)
(require 'setup-move-line)
(require 'setup-duplicate-line)
(require 'setup-editing)
(require 'setup-google-this)
(require 'setup-windmove)
(bind-keys*
 ("C-c o b" . browse-url-of-file)
 ("C-?" . help-command)
 ("s-l" . shell-command))

(setq gc-cons-threshold 800000)
;;; init.el ends here
