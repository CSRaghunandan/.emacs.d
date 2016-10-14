;; Time-stamp: <2016-10-13 11:34:46 csraghunandan>
;; Author: C S Raghunandan

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(defvar gc-cons-threshold--orig gc-cons-threshold)
(setq gc-cons-threshold (* 100 1024 1024))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; emacs setup file
(add-to-list 'load-path (concat user-emacs-directory "setup-files/"))

;; save custom file to a separate directory
(setq custom-file "~/.emacs.d/my-elisp-code/custom-settings.el")
(load custom-file :noerror :nomessage)


;; all use packages declarations
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package
(require 'use-package)
(setq use-package-always-ensure t)

(require 'setup-pragmatapro)
(require 'setup-theme)
(require 'setup-org)
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
(require 'setup-web-mode)
(require 'setup-yas)
(require 'setup-ivy)
(require 'setup-counsel)
(require 'setup-swiper)
(require 'setup-movement)
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
(require 'setup-move-line)
(require 'setup-duplicate-line)
(require 'setup-editing)
(require 'setup-google-this)
(require 'setup-windmove)
(require 'electric-operator)
(require 'setup-mc)
(require 'setup-page-break-lines)
(require 'setup-aggresive-indent)
(require 'setup-js)
(require 'setup-tags)



;; set gc-cons-threshold back to original value
(setq gc-cons-threshold gc-cons-threshold--orig)
;;; init.el ends here
