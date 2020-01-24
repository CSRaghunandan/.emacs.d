;;; general.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-01-24 17:49:22 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

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

;; https://github.com/hlissner/doom-emacs/blob/develop/core/core-lib.el
(defun doom--resolve-path-forms (spec &optional directory)
  "Converts a simple nested series of or/and forms into a series of
`file-exists-p' checks.
For example
  (doom--resolve-path-forms
    '(or \"some-file\" (and path-var \"/an/absolute/path\"))
    \"~\")
Returns
  '(let ((_directory \"~\"))
     (or (file-exists-p (expand-file-name \"some-file\" _directory))
         (and (file-exists-p (expand-file-name path-var _directory))
              (file-exists-p \"/an/absolute/path\"))))
This is used by `associate!', `file-exists-p!' and `project-file-exists-p!'."
  (declare (pure t) (side-effect-free t))
  (cond ((stringp spec)
         `(file-exists-p
           ,(if (file-name-absolute-p spec)
                spec
              `(expand-file-name ,spec ,directory))))
        ((and (listp spec)
              (memq (car spec) '(or and)))
         `(,(car spec)
           ,@(cl-loop for i in (cdr spec)
                      collect (doom--resolve-path-forms i directory))))
        ((or (symbolp spec)
             (listp spec))
         `(file-exists-p ,(if (and directory
                                   (or (not (stringp directory))
                                       (file-name-absolute-p directory)))
                              `(expand-file-name ,spec ,directory)
                            spec)))
        (t spec)))

(defmacro file-exists-p! (spec &optional directory)
  "Returns t if the files in SPEC all exist.
SPEC can be a single file or a list of forms/files. It understands nested (and
...) and (or ...), as well.
DIRECTORY is where to look for the files in SPEC if they aren't absolute. This
doesn't apply to variables, however.
For example:
  (file-exists-p! (or doom-core-dir \"~/.config\" \"some-file\") \"~\")"
  (if directory
      `(let ((--directory-- ,directory))
         ,(doom--resolve-path-forms spec '--directory--))
    (doom--resolve-path-forms spec)))

(defvar doom--transient-counter 0)
(defmacro add-transient-hook! (hook &rest forms)
  "Attaches transient forms to a HOOK.
HOOK can be a quoted hook or a sharp-quoted function (which will be advised).
These forms will be evaluated once when that function/hook is first invoked,
then it detaches itself."
  (declare (indent 1))
  (let ((append (eq (car forms) :after))
        (fn (intern (format "doom-transient-hook-%s" (cl-incf doom--transient-counter)))))
    `(when ,hook
       (fset ',fn
             (lambda (&rest _)
               ,@forms
               (cond ((functionp ,hook) (advice-remove ,hook #',fn))
                     ((symbolp ,hook)   (remove-hook ,hook #',fn)))
               (unintern ',fn nil)))
       (cond ((functionp ,hook)
              (advice-add ,hook ,(if append :after :before) #',fn))
             ((symbolp ,hook)
              (add-hook ,hook #',fn ,append))))))

(provide 'general)
