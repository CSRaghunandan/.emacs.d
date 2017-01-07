;; Time-stamp: <2017-01-07 16:10:33 csraghunandan>

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

(defun sk/insert-date (prefix)
  "Insert the current date. With prefix-argument, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%Y-%m-%d")
                 ((equal prefix '(4)) "%A, %d %B %Y")
                 ((equal prefix '(16)) "%Y-%m-%d %H:%M:%S"))))
    (insert (format-time-string format))))
(bind-key "s-d" 'sk/insert-date)

(use-package calendar :defer t
  :config
  ;; Highlight today's date in the calendar
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

;; to list all the keys-chord not bound to a command
;; https://github.com/Fuco1/free-keys
(use-package free-keys)

;; enable disabled commands
(setq disabled-command-function nil)

;; don't use dialog boxes
(setq use-dialog-box nil)

;; default idle delay for eldoc is way too long
(setq eldoc-idle-delay 0.1
      eldoc-echo-area-use-multiline-p nil)

;; enable some extra syntax highlighting for dash
(with-eval-after-load 'dash
  (dash-enable-font-lock))

(bind-key* "C-?" 'help-command)

;; echo commands as I type
(setq echo-keystrokes 0.01)
;; set all yes or no prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; more info in apropos
(setq apropos-do-all t)
;; always select help window when opened
(setq-default help-window-select t)
;; garbage collect when focus out
(add-hook 'focus-out-hook 'garbage-collect)
;; make links clickable
(add-hook 'prog-mode-hook 'goto-address-mode)

;; delete files by moving to trash in macOS
;; https://github.com/lunaryorn/osx-trash.el
(use-package osx-trash
  :init
  (when (eq system-type 'darwin)
    (osx-trash-setup))
  (setq delete-by-moving-to-trash t))

;; view-mode. Used to view long files like `less' command
(use-package view
  :diminish (view-mode . "𝐕"))

;; load bookmark list
(bookmark-bmenu-list)

;; don't garbage collect when in minibuffer-mode
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(defun conditional-disable-modes ()
  ;; disable flycheck if file is large (above 2MB)
  (when (> (buffer-size) 2000000)
    (flycheck-mode -1)
    (highlight-indent-guides-mode -1)
    (rainbow-delimiters-mode -1)))
(add-hook 'prog-mode-hook 'conditional-disable-modes)

;; google-this : google line, region, symbol, etc.
;; https://github.com/Malabarba/emacs-google-this
(use-package google-this
  :diminish google-this-mode
  :config (google-this-mode 1))

;; prefer new files if one exists while loading
(setq load-prefer-newer t)

;; toml-mode
;; https://github.com/dryman/toml-mode.el
(use-package toml-mode)

;; yaml-mode
;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode)

(provide 'setup-misc)
