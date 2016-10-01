(use-package counsel-projectile
  :bind* (("C-c p p" . counsel-projectile)
	  ("C-c s p" . counsel-projectile-switch-project))
  :config (counsel-projectile-on))

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("C-c d d" . counsel-descbinds)
   ("C-c s s" . counsel-ag)
   ("C-c s d" . counsel-ag-projectile)
   ("C-x C-f" . counsel-find-file)
   ("M-o" . counsel-recentf)
   ("C-c g g" . counsel-git)
   ("C-c g G" . counsel-git-grep)
   ("C-x l" . counsel-locate)
   ("C-c g s" . counsel-grep-or-swiper)
   ("C-M-y" . counsel-yank-pop)
   ("C-c C-r" . ivy-resume)
   ("C-c i m" . counsel-imenu)
   ("C-c i M" . ivy-imenu-anywhere)
   ("C-c d s" . describe-symbol)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line-and-call))

  :config
  (defun reloading (cmd)
    (lambda (x)
      (funcall cmd x)
      (ivy--reset-state ivy-last)))
  (defun given-file (cmd prompt) ; needs lexical-binding
    (lambda (source)
      (let ((target
	     (let ((enable-recursive-minibuffers t))
	       (read-file-name
		(format "%s %s to:" prompt source)))))
	(funcall cmd source target 1))))
  (defun confirm-delete-file (x)
    (dired-delete-file x 'confirm-each-subdirectory))

  (ivy-add-actions
   'counsel-find-file
   `(("c" ,(given-file #'copy-file "Copy") "copy")
     ("d" ,(reloading #'confirm-delete-file) "delete")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move")))
  (ivy-add-actions
   'counsel-projectile-find-file
   `(("c" ,(given-file #'copy-file "Copy") "copy")
     ("d" ,(reloading #'confirm-delete-file) "delete")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move")
     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")))

  ;; to make counsel-ag search the root projectile directory.
  (defun counsel-ag-projectile ()
    (interactive)
    (counsel-ag nil (projectile-project-root)))

  (setq counsel-find-file-at-point t)
  ;; ignore . files or temporary files
  (setq counsel-find-file-ignore-regexp
	(concat
	 ;; File names beginning with # or .
	 "\\(?:\\`[#.]\\)"
	 ;; File names ending with # or ~
	 "\\|\\(?:\\`.+?[#~]\\'\\)")))

(provide 'setup-counsel)
