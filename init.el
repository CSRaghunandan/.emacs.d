;;; init.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-10 00:02:52 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; Every file opened and loaded by Emacs will run through this list to check for
;; a proper handler for the file, but during startup, it won’t need any of them.
(defvar rag--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; load directory for configuration files for emacs
(add-to-list 'load-path (concat user-emacs-directory "setup-files/"))
(add-to-list 'load-path (concat user-emacs-directory "my-elisp-code"))

;; set home and emacs directories
(defvar user-home-directory (concat (getenv "HOME") "/"))
(setq user-emacs-directory (concat user-home-directory ".emacs.d/"))

(load (locate-user-emacs-file "general.el") nil :nomessage)

;; run package-initialize if running emacs version < 27
(>=e "27.0"
    nil
  (package-initialize))



;; load all use-package related configuration
(load (locate-user-emacs-file "setup-packages.el") nil :nomessage)

(require 'setup-no-littering)
(require 'setup-osx)
(require 'setup-org)
(require 'setup-backup)
(require 'setup-selected)
(require 'setup-treemacs)
(require 'setup-search)
(require 'setup-rg)
(require 'setup-ibuffer)
(require 'setup-recentf)
(require 'setup-desktop)
(require 'setup-calc)
(require 'setup-ediff)
(require 'setup-dired)
(require 'setup-elisp-mode)
(require 'setup-flycheck)
(require 'setup-spell)
(require 'setup-bookmark)
(require 'setup-hydra)
(require 'setup-company)
(require 'setup-smartparens)
(require 'setup-git-stuff)
(require 'setup-avy)
(require 'setup-ace-window)
(require 'setup-projectile)
(require 'setup-yas)
(require 'setup-buffers)
(require 'setup-ivy)
(require 'setup-counsel)
(require 'setup-swiper)
(require 'setup-movement)
(require 'setup-markdown)
(require 'setup-highlight)
(require 'setup-info)
(require 'setup-mode-line)
(require 'setup-editing)
(require 'setup-racket)
(require 'setup-rust)
(require 'setup-lsp)
(require 'setup-cc)
(require 'setup-haskell)
(require 'setup-python)
(require 'setup-tex)
(require 'setup-origami)
(require 'setup-duplicate-line)
(require 'setup-white-space)
(require 'setup-mc)
(require 'setup-js)
(require 'setup-json)
(require 'setup-typescript)
(require 'setup-ocaml)
(require 'setup-web-mode)
(require 'setup-css)
(require 'setup-eshell)
(require 'setup-comint)
(require 'setup-term)
(require 'setup-which-key)
(require 'setup-kurecolor)
(require 'setup-erc)
(require 'setup-font-check)
(require 'setup-misc)
(require 'setup-visual)
(require 'setup-tramp)
(require 'setup-theme)
(require 'setup-command-log-mode)
(require 'setup-calendar)
(require 'setup-minibuffer)
(require 'setup-purescript)
(require 'setup-abbrev)
(require 'setup-compile)
(require 'setup-macro)
(require 'setup-help)
(require 'setup-config-files)
(require 'setup-ansible)
(require 'setup-shell)
(require 'setup-annotate)
(require 'setup-smerge)
(require 'setup-nov)
(require 'setup-xkcd)
(require 'setup-docker)
(require 'setup-pdf)
(require 'setup-engine-mode)

;; install all packages (if they already not installed by use-package)
(package-install-selected-packages)



;; start emacs server only it has not already been started
(require 'server)
(unless (server-running-p) (server-start))

;; set gc-cons-threshold back to original value
(setq file-name-handler-alist rag--file-name-handler-alist
      gc-cons-threshold 800000
      gc-cons-percentage 0.1)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("3577ee091e1d318c49889574a31175970472f6f182a9789f1a3e9e4513641d86" default))
 '(package-selected-packages
   '(no-littering yasnippet-snippets yapfify xkcd whole-line-or-region which-key wgrep web-mode vterm-toggle volatile-highlights vlf visual-regexp-steroids utop use-package-chords undo-tree tuareg treemacs-projectile treemacs-magit treemacs-icons-dired toc-org tide systemd ssh-config-mode sphinx-doc solaire-mode smartparens smart-dash sicp shm selected sass-mode rustic rmsbolt rjsx-mode rigid-tabs restart-emacs request regex-tool realgud rainbow-mode rainbow-delimiters racket-mode quickrun qml-mode python-docstring pytest pyenv-mode py-isort purescript-mode psc-ide prettier-js powershell poporg pip-requirements pdfgrep pdf-tools pandoc-mode page-break-lines ox-gfm osx-trash osx-clipboard org-super-agenda org-pomodoro org-journal org-download org-cliplink org-autolist ocp-indent ob-http nov nginx-mode mwim move-text modern-cpp-font-lock mocha-snippets mocha minions markdown-toc manage-minor-mode magit-todos lsp-ui lsp-treemacs lsp-python-ms lsp-origami langtool kurecolor jinja2-mode ivy-hydra intero info-colors inf-mongo indium impatient-mode imenu-anywhere ibuffer-vc hungry-delete hlint-refactor hindent highlight-symbol highlight-quoted highlight-numbers highlight-indent-guides highlight-escape-sequences highlight-doxygen helpful headlong hasky-stack gotest gorepl-mode golden-ratio-scroll-screen go-rename go-guru go-eldoc gnu-elpa-keyring-update gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gist free-keys forge fontify-face flycheck-pycheckers flycheck-ocaml eyebrowse expand-region exec-path-from-shell eterm-256color eslintd-fix erc-image erc-hl-nicks engine-mode emmet-mode elisp-def elfeed-org dumb-jump dotenv-mode doom-themes doom-modeline dockerfile-mode docker-compose-mode docker disable-mouse diredfl dired-quick-sort diff-hl default-text-scale deadgrep cycle-quotes csv-mode crontab-mode counsel-projectile company-shell company-quickhelp company-prescient company-nginx company-lsp company-go company-ansible command-log-mode column-enforce-mode cmake-font-lock ccls bm blacken beginend auto-yasnippet auctex apache-mode ansible-doc ansible annotate amx add-node-modules-path adaptive-wrap ace-link)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
