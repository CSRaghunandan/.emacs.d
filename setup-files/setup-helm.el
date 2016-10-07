;; Time-stamp: <2016-10-07 12:14:51 csraghunandan>

;; NOTE: I no longer use helm-mode as of Sept. 2016

;; helm - incremental narrowing framework for emacs
;; https://github.com/emacs-helm/helm
(use-package helm
  :config
  (require 'helm)
  (require 'helm-config)

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (helm-autoresize-mode 1)
  (setq helm-move-to-line-cycle-in-source     t
        helm-ff-file-name-history-use-recentf t
        helm-split-window-in-side-p t
        helm-display-header-line nil ;; t by default
        helm-follow-mode-persistent t
        helm-ff-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-autoresize-max-height 20
        helm-autoresize-min-height 20)

  (define-key helm-map (kbd "<tab>")    'helm-execute-persistent-action)
  (define-key helm-map (kbd "S-<tab>") 'helm-select-action)

  ;; get rid of source header line for helm mini buffers with just one source file
  (defun helm-toggle-header-line ()
    (if (= (length helm-sources) 1)
        (set-face-attribute 'helm-source-header nil :height 0.1)
      (set-face-attribute 'helm-source-header nil :height 1.0)))
  (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)

  ;; make helm-find file fetch results from spotlight.
  (setq helm-locate-command "mdfind -name %s %s")
  :bind
  ("C-c h i" . helm-imenu-in-all-buffers)
  ("C-M-y" . helm-show-kill-ring)
  ("C-x C-f" . helm-find-files))

;; helm-swoop - search with an overview
;; https://github.com/ShingoFukuyama/helm-swoop
(use-package helm-swoop
  :config
  (setq helm-multi-swoop-edit-save t)
  (setq helm-swoop-split-with-multiple-windows nil)
  (setq helm-swoop-split-direction 'split-window-vertically)
  (setq helm-swoop-speed-or-color t)
  (setq helm-swoop-move-to-line-cycle t)
  (setq helm-swoop-use-line-number-face t)
  (setq helm-swoop-use-fuzzy-match nil)
  :bind* (("M-i" . helm-swoop)
	  ("M-I" . helm-swoop-back-to-last-point)
	  ("C-c M-i" . helm-multi-swoop)
	  ("C-c M-I" . helm-multi-swoop-all)
	  ("C-c M-m" . helm-multi-swoop-current-mode)))

;; manage projectile with helm
;; https://github.com/bbatsov/helm-projectile
(use-package helm-projectile
  :bind* (("C-c p h" . helm-projectile))
  :init
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

;; helm-descbinds :- show keybindings
;; https://github.com/emacs-helm/helm-descbinds
(use-package helm-descbinds
  :bind* ("C-c h d" . helm-descbinds))

;; helm-ag :- use ag with helm
;; https://github.com/syohex/emacs-helm-ag
(use-package helm-ag
  :bind* (("C-c h p" . helm-do-ag-project-root)
	  ("C-c h s" . helm-do-ag)))

;; add this line to spaceline configuration to get a better looking modeline for helm
;; (spaceline-helm-mode)

;; helm-smex :- smex backend for helm M-x
;; https://github.com/ptrv/helm-smex
(use-package helm-smex
  :bind* ("M-x" . helm-smex))

(provide 'setup-helm)
