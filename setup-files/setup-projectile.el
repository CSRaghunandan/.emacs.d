(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t)

  ;; Git projects should be marked as projects in top-down fashion,
  ;; so that each git submodule can be a projectile project.
  (setq projectile-project-root-files-bottom-up
  	(delete ".git" projectile-project-root-files-bottom-up))
  (add-to-list 'projectile-project-root-files ".git")

  (setq projectile-project-root-files-functions
  	'(projectile-root-local
  	  projectile-root-top-down ; First look for projects in top-down order
  	  projectile-root-bottom-up)) ; Then in bottom-up order

  ;;; Default ag arguments
  ;; https://github.com/ggreer/the_silver_searcher
  (defconst modi/ag-arguments
    '("--nogroup" ; mandatory argument for ag.el as per https://github.com/Wilfred/ag.el/issues/41
      "--skip-vcs-ignores"               ; Ignore files/dirs ONLY from `.ignore'
      "--numbers"                        ; line numbers
      "--smart-case"
      ;; "--one-device"                      ; do not cross mounts when searching
      "--follow")                        ; follow symlinks
    "Default ag arguments used in the functions in `ag', `counsel' and `projectile'
packages.")

;;; Default rg arguments
  ;; https://github.com/BurntSushi/ripgrep
  (defconst modi/rg-arguments
    '("--no-ignore-vcs"                  ; Ignore files/dirs ONLY from `.ignore'
      "--line-number"                    ; line numbers
      "--smart-case"
      "--follow")                        ; follow symlinks
    "Default rg arguments used in the functions in `projectile' package.")

  ;; Use `ag' all the time if available
  (defun modi/advice-projectile-use-ag ()
    "Always use `ag' for getting a list of all files in the project."
    (mapconcat 'identity
  	       (append '("\\ag") ; used unaliased version of `ag': \ag
  		       modi/ag-arguments
  		       '("-0" ; output null separated results
  			 "-g ''")) ; get file names matching the regex '' (all files)
  	       " "))

  (defun modi/advice-projectile-use-rg ()
    "Always use `rg' for getting a list of all files in the project."
    (mapconcat 'identity
  	       (append '("\\rg") ; used unaliased version of `rg': \rg
  		       modi/rg-arguments
		       '("--null" ; output null separated results,
			 "--files")) ; get file names matching the regex '' (all files)
  	       " "))

  ;; Use `rg' all the time if available
  (if (executable-find "rg")
      (progn
  	(advice-remove 'projectile-get-ext-command #'modi/advice-projectile-use-ag)
  	(advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-rg))
    ;; Else use `ag' if available
    (when (executable-find "ag")
      (advice-remove 'projectile-get-ext-command #'modi/advice-projectile-use-rg)
      (advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-ag)))

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
           (prj-true (file-truename prj))
           (prj-abbr (abbreviate-file-name prj-true)))
      ;; First remove the current project from `projectile-known-projects'.
      ;; Also make sure that duplicate instance of the project name in form of symlink
      ;; name, true name and abbreviated name, if any, are also removed.
      (setq projectile-known-projects
            (delete prj (delete prj-true (delete prj-abbr projectile-known-projects))))
      ;; Then add back only the abbreviated true name to the beginning of
      ;; `projectile-known-projects'.
      (add-to-list 'projectile-known-projects prj-abbr)))
  (add-hook 'projectile-after-switch-project-hook #'modi/projectile-known-projects-sort)

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
  (projectile-global-mode))

(provide 'setup-projectile)
