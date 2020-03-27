;;; setup-misc.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-03-27 12:31:55 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; set scratch major mode to `emacs-lisp-mode'
(setq initial-major-mode 'emacs-lisp-mode)

(defun my/package-upgrade-packages (&optional no-fetch)
  "Upgrade all packages.  No questions asked.
This function is equivalent to `list-packages', followed by a
`package-menu-mark-upgrades' and a `package-menu-execute'.  Except
the user isn't asked to confirm deletion of packages.
The NO-FETCH prefix argument is passed to `list-packages'.  It
prevents re-download of information about new versions.  It does
not prevent downloading the actual packages (obviously)."
  (interactive "P")
  (let ((package-menu-async nil)) ; This variable was introduced in emacs 25.0
    (save-window-excursion
      (package-list-packages no-fetch)
      (package-menu-mark-upgrades)
      (package-menu-execute 'noquery))))

;; to list all the keys-chord not bound to a command
;; https://github.com/Fuco1/free-keys
(use-package free-keys
  :defer t)

;; enable disabled commands
(setq disabled-command-function nil)

;; don't use dialog boxes
(setq use-dialog-box nil)

;; prevents slowdown when using fancy fonts
(setq inhibit-compacting-font-caches t)

;; default idle delay for eldoc is way too long
(setq eldoc-idle-delay 0.1
      eldoc-echo-area-use-multiline-p t)

(setq delete-by-moving-to-trash t)

;; echo commands as I type
(setq echo-keystrokes 0.01)
;; set all yes or no prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; make links clickable
(add-hook 'prog-mode-hook 'goto-address-prog-mode)

(defun conditional-disable-modes ()
  ;; disable heavy minor modes if file is large (above 2MB)
  (when (> (buffer-size) 2000000)
    (flycheck-mode -1)
    (rainbow-delimiters-mode -1)
    (highlight-indent-guides-mode -1)))
(add-hook 'prog-mode-hook 'conditional-disable-modes)

;; for when you want to disable the mouse
;; https://github.com/purcell/disable-mouse/tree/master
(use-package disable-mouse
  :hook ((prog-mode . disable-mouse-mode)
         (magit-mode . disable-mouse-mode)
         (minibuffer-setup . disable-mouse-mode)))

;; prefer new files if one exists while loading
(setq load-prefer-newer t)

;; regex-tool: A regular expression IDE for Emacs, to help with the creation and testing of regular expressions.
;; https://github.com/jwiegley/regex-tool
(use-package regex-tool
  :defer t)

;; restart-emacs: A simple emacs package to restart emacs from within emacs
;; https://github.com/iqbalansari/restart-emacs
(use-package restart-emacs
  :bind ("C-c r s" . restart-emacs))

(defun rag-suspend-frame ()
  "In a GUI environment, do nothing; otherwise `suspend-frame'."
  (interactive)
  (if (display-graphic-p)
      (message "suspend-frame disabled for graphical displays.")
    (suspend-frame)))

(global-unset-key (kbd "C-z"))
(bind-key "C-z C-z" 'rag-suspend-frame)

;; use firefox-developer-edition as default browser
(setq browse-url-firefox-program "firefox-developer-edition")
(setq browse-url-browser-function 'browse-url-firefox)

(provide 'setup-misc)
