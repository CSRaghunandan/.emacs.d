;; Time-stamp: <2018-06-11 22:12:20 csraghunandan>

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
(use-package free-keys)

;; enable disabled commands
(setq disabled-command-function nil)

;; don't use dialog boxes
(setq use-dialog-box nil)

;; default idle delay for eldoc is way too long
(setq eldoc-idle-delay 0.1
      eldoc-echo-area-use-multiline-p nil)

(bind-key* "C-?" 'help-command)

;; echo commands as I type
(setq echo-keystrokes 0.01)
;; set all yes or no prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; garbage collect when focus out
(add-hook 'focus-out-hook 'garbage-collect)
;; make links clickable
(add-hook 'prog-mode-hook 'goto-address-mode)

(defun conditional-disable-modes ()
  ;; disable heavy minor modes if file is large (above 2MB)
  (when (> (buffer-size) 2000000)
    (flycheck-mode -1)
    (rainbow-delimiters-mode -1)
    (highlight-indent-guides-mode -1)))
(add-hook 'prog-mode-hook 'conditional-disable-modes)

;; google-this : google line, region, symbol, etc.
;; https://github.com/Malabarba/emacs-google-this
(use-package google-this
  :config (google-this-mode 1))

;; for when you want to disable the mouse
;; https://github.com/purcell/disable-mouse/tree/master
(use-package disable-mouse
  :config (global-disable-mouse-mode))

;; prefer new files if one exists while loading
(setq load-prefer-newer t)

;; regex-tool: A regular expression IDE for Emacs, to help with the creation and testing of regular expressions.
;; https://github.com/jwiegley/regex-tool
(use-package regex-tool
  :defer t)

(provide 'setup-misc)
