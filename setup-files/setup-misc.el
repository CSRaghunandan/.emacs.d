;; Time-stamp: <2016-12-23 09:45:38 csraghunandan>

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
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region   'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column  'disabled nil)

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
;; use aspell for spell checking
(setq ispell-program-name "aspell")

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

;; set emacs source code directory
(setq source-directory "~/emacs--git/src/")

;; load bookmark list
(bookmark-bmenu-list)

;; don't garbage collect when in minibuffer-mode
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(provide 'setup-misc)
