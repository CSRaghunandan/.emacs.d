;; Time-stamp: <2017-01-06 11:11:27 csraghunandan>

;; Projectile
;; https://github.com/bbatsov/projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t)

  ;; ignore stack directory as projectile project
  (add-to-list 'projectile-ignored-projects (concat user-home-directory ".stack/global-project/"))

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
        `("--line-number"                     ; line numbers
          "--smart-case"
          "--follow"                          ; follow symlinks
          "--mmap")                           ; apply memory map optimization when possible
        "Default rg arguments used in the functions in `projectile' package.")

      (defun modi/advice-projectile-use-rg ()
        "Always use `rg' for getting a list of all files in the project."
        (mapconcat 'identity
                   (append '("\\rg") ; used unaliased version of `rg': \rg
                           modi/rg-arguments
                           '("--null" ; output null separated results,
                             "--files")) ; get file names matching the regex '' (all files)
                   " "))

      (advice-add 'projectile-get-ext-command :override #'modi/advice-projectile-use-rg)))


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

  (bind-keys*
   ("C-c p K" . modi/kill-non-project-buffers)
   ("C-c h p" . hydra-projectile/body)
   ("C-c p r" . projectile-replace-regexp))



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
_f_/_s-f_: file               _r_: counsel-rg        ^^    _i_: Ibuffer               _c_: cache clear               ^^    _E_: edit project's .dir-locals.el
^^    _F_: file dwim          _g_: update gtags      ^^    _b_: switch to buffer      _x_: remove known project      _s-p_/_p_: switch to any other project
^^    _d_: file curr dir      _o_: multi-occur       _K_/_s-k_: kill all buffers      _X_: cleanup non-existing      ^^    _P_: switch to an open project
^^    _r_: recent file        _G_: git-grep          ^^^^                             _z_: cache current
^^    _D_: dir                _R_: counsel-rg-root
"
    ("r"   counsel-rg)
    ("R"   rag/counsel-rg-project-at-point)
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

  (projectile-mode +1))

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
;; * run `C-c p h' to open the hydra for projectile
