;; Time-stamp: <2018-03-03 09:49:52 csraghunandan>

;; Projectile: Project Interaction Library for Emacs
;; https://github.com/bbatsov/projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t)

  ;; ignore stack directory as projectile project
  (add-to-list 'projectile-ignored-projects
               (concat user-home-directory ".stack/global-project/"))
  ;; ignore all projects under exercism directory
  (require 'f)
  (defun my-projectile-ignore-project (project-root)
    (f-descendant-of? project-root (expand-file-name "~/exercism")))
  (setq projectile-ignored-project-function #'my-projectile-ignore-project)



;;; Default rg arguments
  ;; https://github.com/BurntSushi/ripgrep
  (when (executable-find "rg")
    (progn
      (defconst modi/rg-arguments
        `("--line-number" ; line numbers
          "--follow" ; follow symlinks
          "--color never"
          "--max-columns 150"        ;Emacs doesn't handle long line lengths very well
          "--mmap") ; apply memory map optimization when possible
        "Default rg arguments used in the functions in `projectile' package.")

      (defun modi/advice-projectile-use-rg ()
        "Always use `rg' for getting a list of all files in the project."
        (mapconcat 'identity
                   (append '("\\rg") ; used unaliased version of `rg': \rg
                           modi/rg-arguments
                           '("--null" ; output null separated results,
                             "--files")) ; get file names matching the regex '' (all files)
                   " "))

      (advice-add 'projectile-get-ext-command
                  :override #'modi/advice-projectile-use-rg)))


  ;; Make the file list creation faster by NOT calling `projectile-get-sub-projects-files'
  (defun modi/advice-projectile-no-sub-project-files ()
    "Directly call `projectile-get-ext-command'. No need to try to get a
  list of sub-project files if the vcs is git."
    (projectile-files-via-ext-command (projectile-get-ext-command)))
  (advice-add 'projectile-get-repo-files :override
  	      #'modi/advice-projectile-no-sub-project-files)

  (defun modi/projectile-known-projects-sort ()
    "Move the now current project to the top of the `projectile-known-projects' list."
    (let* ((prj (projectile-project-root))
           ;; Set `prj' to nil if that project is supposed to be ignored
           (prj (and (not (projectile-ignored-project-p prj)) prj))
           (prj-true (and prj (file-truename prj)))
           (prj-abbr (and prj (abbreviate-file-name prj-true))))
      (when prj
        ;; First remove the current project from `projectile-known-projects'.
        ;; Also make sure that duplicate instance of the project name in form of symlink
        ;; name, true name and abbreviated name, if any, are also removed.
        (setq projectile-known-projects
              (delete prj (delete prj-true
                                  (delete prj-abbr projectile-known-projects))))
        ;; Then add back only the abbreviated true name to the beginning of
        ;; `projectile-known-projects'.
        (add-to-list 'projectile-known-projects prj-abbr))))
  (add-hook 'projectile-after-switch-project-hook
            #'modi/projectile-known-projects-sort)

  (defun modi/kill-non-project-buffers (&optional kill-special)
    "Kill buffers that do not belong to a `projectile' project.
With prefix argument (`C-u'), also kill the special buffers."
    (interactive "P")
    (let ((bufs (buffer-list (selected-frame))))
      (dolist (buf bufs)
        (with-current-buffer buf
          (let ((buf-name (buffer-name buf)))
            (when (or (null (projectile-project-p))
                      (and kill-special
                           (string-match "^\*" buf-name)))
              ;; Preserve buffers with names starting with *scratch or *Messages
              (unless (string-match "^\\*\\(\\scratch\\|Messages\\)" buf-name)
                (message "Killing buffer %s" buf-name)
                (kill-buffer buf))))))))

  (defun modi/projectile-find-file-literally (&optional arg)
    "Jump to a project's file literally (see `find-file-literally') using
completion.  With a prefix ARG invalidates the cache first.
Using this function over `projectile-find-file' is useful for opening files that
are slow to open because of their major mode. `find-file-literally' always opens
files in Fundamental mode."
    (interactive "P")
    (projectile-maybe-invalidate-cache arg)
    (projectile-completing-read
     "Find file literally: "
     (projectile-current-project-files)
     :action `(lambda (file)
                (find-file-literally (expand-file-name file ,(projectile-project-root)))
                (run-hooks 'projectile-find-file-hook))))

  (bind-keys
   ("C-c p K" . modi/kill-non-project-buffers)
   ("C-c p r" . projectile-replace-regexp)
   ("C-c s m" . modi/projectile-switch-project-magit-status))



  (defun modi/projectile-switch-project-magit-status ()
    "Switch to other project and open Magit status there."
    (interactive)
    (let ((projectile-switch-project-action #'magit-status))
      (call-interactively #'projectile-switch-project)))

  (projectile-mode))

(provide 'setup-projectile)

;; projectile
;; This configuration uses `rg'(ripgrep) to generate the project list
;; * to clear the cache when searching for files in a project, prefix
;;   `projectile-find-file' with `C-u'.
;; * use `projectile-ibuffer' [C-c p I] to open `ibuffer' for the current project only
;; * use `projectile-kill-buffers' [C-c p k] to kill all buffers related to a project
;; * use `projectile-recentf' [C-c p e] to list all recently opened file in a project
;; * use `projectile-switch-open-project' [C-c p q] to switch to an open project
;; * use `projectile-replace-regexp' [C-c Q] to replace regexp in the project
;; * use `projectile-find-dir' to select all the directories in a project
;; * use `projectile-dired' to open the dired buffer of project root
;; `projectile-edit-dir-locals' -> to edit the .dirlocals of the project
;; `projectile-find-file-in-known-projects' -> to find file in all known projects
