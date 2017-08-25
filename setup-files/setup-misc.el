;; Time-stamp: <2017-08-26 00:20:40 csraghunandan>

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

(setq recenter-positions '(0.50 0.07 0.93)) ;default: '(middle top bottom)
;; First C-l  -> 0.50: Put point vertically at the middle of the window
;; Second C-l -> 0.07: Put point close to the top of the window. If
;;                     (window-height) returns 70, that's roughly 4 lines.
;; Third C-l  -> 0.93: Put point close to the bottom of the window ~ 3 lines.
;; With the default values of `recenter-positions' and `scroll-margin' (0),
;; the "top" position is the first line of the window, and the "bottom"
;; position is the last line. Above settings provides a margin of 3 or 4 lines
;; for my default window size for the "top" and "bottom" iterations.

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

;; load bookmark list
(bookmark-bmenu-list)

(defun conditional-disable-modes ()
  ;; disable heavy minor modes if file is large (above 2MB)
  (when (> (buffer-size) 2000000)
    (flycheck-mode -1)
    (rainbow-delimiters-mode -1)))
(add-hook 'prog-mode-hook 'conditional-disable-modes)

;; google-this : google line, region, symbol, etc.
;; https://github.com/Malabarba/emacs-google-this
(use-package google-this
  :diminish google-this-mode
  :config (google-this-mode 1))

;; prefer new files if one exists while loading
(setq load-prefer-newer t)

;; enable recursive minibuffers
(setq enable-recursive-minibuffers t)

(when (eq system-type 'darwin)
  (setq source-directory "/Users/csraghunandan/Library/Caches/Homebrew/emacs--git"))

(defvar emacs-git-branch
  (when (and emacs-repository-version
             (file-exists-p source-directory))
    (let ((shell-return
           (replace-regexp-in-string
            "[\n)]" " "                 ;Replace newline and ) chars with spaces
            (shell-command-to-string
             (concat "cd " source-directory " && "
                     "git branch --contains "
                     emacs-repository-version)))))
      ;; Below regexp is tested for following "git branch --contains" values
      ;; Output for a commit in master branch too
      ;;   "* (HEAD detached at origin/emacs-25)
      ;;     master
      ;;   "
      ;; Output for a commit only in emacs-25 branch
      ;;   "* (HEAD detached at origin/emacs-25)
      ;;   "
      ;; (message "%S" shell-return)
      (when (not (string= "" shell-return))
	(string-match ".*[/ ]\\([^ ]+?\\)\\s-*$" shell-return)
	(match-string-no-properties 1 shell-return))))
  "Name of git branch from which the current emacs is built.")

(defun emacs-version-dev (here)
  "Display emacs build info and also save it to the kill-ring.
If HERE is non-nil, also insert the string at point."
  (interactive "P")
  (let ((emacs-build-info
         (concat "Emacs version: " (emacs-version) ","
                 " built using commit " emacs-repository-version ".\n\n"
                 "./configure options:\n  " system-configuration-options "\n\n"
                 "Features:\n  " system-configuration-features "\n")))
    (kill-new emacs-build-info)
    (message "%s" emacs-build-info)
    (when here
      (insert emacs-build-info))
    emacs-build-info))

(provide 'setup-misc)
