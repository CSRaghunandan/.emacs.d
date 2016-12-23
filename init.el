;; Time-stamp: <2016-12-23 16:36:20 csraghunandan>
;; Author: C S Raghunandan

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(defvar gc-cons-threshold--orig gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; emacs setup file
(add-to-list 'load-path (concat user-emacs-directory "setup-files/"))

;; set home and emacs directories
(defvar user-home-directory (concat (getenv "HOME") "/")) ; must end with /
(setq user-emacs-directory (concat user-home-directory ".emacs.d/")) ; must end with /

;; save custom file to a separate directory
(setq custom-file (concat user-emacs-directory "my-elisp-code/custom-settings.el"))
(load custom-file :noerror :nomessage)


;; all use packages declarations
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(when (memq window-system '(mac ns x))
  (require 'setup-osx))
(require 'setup-org)
(require 'setup-org-journal)
(require 'setup-backup)
(require 'setup-region-bindings-mode)
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
(require 'setup-git-stuff)
(require 'setup-undo-tree)
(require 'setup-avy)
(require 'setup-ace-window)
(require 'setup-smex)
(require 'setup-projectile)
(require 'setup-yas)
(require 'setup-ivy)
(require 'setup-counsel)
(require 'setup-swiper)
(require 'setup-movement)
(require 'setup-markdown)
(require 'setup-highlight)
(require 'setup-info)
(require 'setup-mode-line)
(require 'setup-racket)
(require 'setup-hungry-delete)
(require 'setup-rust)
(require 'setup-haskell)
(require 'setup-python)
(require 'setup-tex)
(require 'setup-misc)
(require 'setup-origami)
(require 'setup-buffers)
(require 'setup-move-line)
(require 'setup-duplicate-line)
(require 'setup-editing)
(require 'setup-google-this)
(require 'setup-mc)
(require 'setup-page-break-lines)
(require 'setup-js)
(require 'setup-web-mode)
(require 'setup-recursive-narrow)
(require 'setup-shell)
(require 'setup-which-key)
(require 'setup-kurecolor)
(require 'setup-erc)
(require 'setup-theme)
(require 'setup-pragmatapro)



;; start emacs server only it has not already been started
(unless (server-running-p) (server-start))

;; set gc-cons-threshold back to original value
(setq gc-cons-threshold gc-cons-threshold--orig)
;;; init.el ends here
