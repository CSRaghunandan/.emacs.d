;; Time-stamp: <2016-10-25 13:19:09 csraghunandan>

;; Projectile
;; https://github.com/bbatsov/projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t)

  ;; Don't consider my home dir as a project
  (add-to-list 'projectile-ignored-projects `,(concat (getenv "HOME") "/"))

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
      ;;"--skip-vcs-ignores"               ; Ignore files/dirs ONLY from `.ignore'
      "--numbers"                        ; line numbers
      "--smart-case"
      ;; "--one-device"                      ; do not cross mounts when searching
      "--follow")                        ; follow symlinks
    "Default ag arguments used in the functions in `ag', `counsel' and `projectile'
packages.")

;;; Default rg arguments
  ;; https://github.com/BurntSushi/ripgrep
  (defconst modi/rg-arguments
    '(;;"--no-ignore-vcs"                  ; Ignore files/dirs ONLY from `.ignore'
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
  ;; ;; Use `rg' all the time if available
  ;; (if (executable-find "rg")
  ;;     (progn
  ;; 	(advice-remove 'projectile-get-ext-command #'modi/advice-projectile-use-ag)
  ;; 	  (advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-rg))
  ;;   ;; Else use `ag' if available
  ;;   (when (executable-find "ag")
  ;;     (advice-remove 'projectile-get-ext-command #'modi/advice-projectile-use-rg)
  ;;     (advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-ag)))

  (advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-rg)



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
              (delete prj (delete prj-true (delete prj-abbr projectile-known-projects))))
        ;; Then add back only the abbreviated true name to the beginning of
        ;; `projectile-known-projects'.
        (add-to-list 'projectile-known-projects prj-abbr))))
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

  (bind-key "C-c p K" 'modi/kill-non-project-buffers)



  (defhydra hydra-projectile-other-window (:color teal)
    "projectile-other-window"
    ("f"  projectile-find-file-other-window        "file")
    ("g"  projectile-find-file-dwim-other-window   "file dwim")
    ("d"  projectile-find-dir-other-window         "dir")
    ("b"  projectile-switch-to-buffer-other-window "buffer")
    ("q"  nil                                      "cancel" :color blue))

  (defhydra hydra-projectile (:color teal
                                     :hint  nil)
    "
     PROJECTILE: %(if (fboundp 'projectile-project-root) (projectile-project-root) \"TBD\")
^^^^       Find               ^^   Search/Tags       ^^^^       Buffers               ^^   Cache                     ^^^^       Other
^^^^--------------------------^^---------------------^^^^-----------------------------^^------------------------------------------------------------------
_f_/_s-f_: file               _a_: counsel-ag        ^^    _i_: Ibuffer               _c_: cache clear               ^^    _E_: edit project's .dir-locals.el
^^    _F_: file dwim          _g_: update gtags      ^^    _b_: switch to buffer      _x_: remove known project      _s-p_/_p_: switch to any other project
^^    _d_: file curr dir      _o_: multi-occur       _K_/_s-k_: kill all buffers      _X_: cleanup non-existing      ^^    _P_: switch to an open project
^^    _r_: recent file        _G_: git-grep          ^^^^                             _z_: cache current
^^    _D_: dir                _A_: counsel-ag-root
"
    ("a"   counsel-ag)
    ("A"   rag/counsel-ag-project-at-point)
    ("G"   counsel-git-grep)
    ("b"   projectile-switch-to-buffer)
    ("c"   projectile-invalidate-cache)
    ("d"   projectile-find-file-in-directory)
    ("f"   projectile-find-file)
    ("s-f" projectile-find-file)
    ("F"   projectile-find-file-dwim)
    ("D"   projectile-find-dir)
    ("E"   projectile-edit-dir-locals)
    ("g"   ggtags-update-tags)
    ("i"   projectile-ibuffer)
    ("K"   projectile-kill-buffers)
    ("s-k" projectile-kill-buffers)
    ("m"   projectile-multi-occur)
    ("o"   projectile-multi-occur)
    ("p"   projectile-switch-project)
    ("s-p" projectile-switch-project)
    ("P"   projectile-switch-open-project)
    ("s"   projectile-switch-project)
    ("r"   projectile-recentf)
    ("x"   projectile-remove-known-project)
    ("X"   projectile-cleanup-known-projects)
    ("z"   projectile-cache-current-file)
    ("4"   hydra-projectile-other-window/body "other window")
    ("q"   nil "cancel" :color blue))
  (bind-key "C-c p h" 'hydra-projectile/body)

  (projectile-mode +1))

(provide 'setup-projectile)
