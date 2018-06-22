;; Time-stamp: <2018-06-22 12:24:46 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghuandan rnraghunandan@gmail.com

;;; Emacs version check
(defmacro >=e (version &rest body)
  "Emacs VERSION check wrapper around BODY.
BODY can contain both `if' block (for stuff to execute if emacs
is equal or newer than VERSION) and `else' block (for stuff to
execute if emacs is older than VERSION).
Example:
  (>=e \"25.0\"
      (defun-compatible-with-25.0)
    (defun-not-compatible-in-older-version))"
  (declare (indent 2))          ;`if'-style indentation where this macro is used
  `(if (version<= ,version emacs-version)
       ,@body))

(when (eq system-type 'darwin)
  (setq source-directory
        (concat user-home-directory "/Library/Caches/Homebrew/emacs--git")))

(defun byte-recompile-elpa ()
  "Force byte-compile every `.el' file in `package-user-dir'.
The `.el' files are re-compiled even if the corresponding `.elc' files exist,
in all the sub-directories under `package-user-dir'.
If the `.elc' file does not exist, this function *does not* compile the
corresponding `.el' file."
  (interactive)
  (byte-recompile-directory package-user-dir nil :force))

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

;; Quitting emacs via `C-x C-c` or the GUI 'X' button
(setq confirm-kill-emacs #'y-or-n-p)

(setq user-mail-address "rnraghunandan@gmail.com")

(defun is-mac-p ()
  (eq system-type 'darwin))

(defun is-linux-p ()
  (eq system-type 'gnu/linux))

(defun is-windows-p ()
  (or
   (eq system-type 'ms-dos)
   (eq system-type 'windows-nt)
   (eq system-type 'cygwin)))

(provide 'general)
