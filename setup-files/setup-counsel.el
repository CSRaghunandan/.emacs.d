;;; -*- lexical-binding: t -*-
;; Time-stamp: <2018-02-26 08:07:43 csraghunandan>

;; counsel: ivy backends for a lot more commands
;; https://github.com/abo-abo/swiper
(use-package counsel
  :diminish counsel-mode
  :bind*
  (([remap execute-extended-command] . counsel-M-x)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line-and-call))

  :init
  (counsel-mode)

  (bind-keys
   :map read-expression-map
   ("C-r" . counsel-expression-history))

  (with-eval-after-load 'org-agenda
    (bind-key "C-c C-q" #'counsel-org-tag-agenda org-agenda-mode-map))

  :config

  (if (executable-find "rg")
      ;; if rg is installed, use rg for `counsel-grep-or-swiper'
      (setq counsel-grep-base-command
            "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
    ;; ignore case sensitivity for counsel grep
    (setq counsel-grep-base-command "grep -nEi \"%s\" %s"))

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
   `(("m" ,(reloading (given-file #'rename-file "Move")) "move")
     ("d" ,(reloading #'confirm-delete-file) "delete")))
  (ivy-add-actions
   'projectile-find-file
   `(("d" ,(reloading #'confirm-delete-file) "delete")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move")
     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")))

  ;; counsel-rg
  ;; Redefine `counsel-rg-base-command' with my required options, especially
  ;; the `--follow' option to allow search through symbolic links (part of
  ;; `modi/rg-arguments').
  (when (executable-find "rg")
    (setq counsel-rg-base-command
          "rg --line-number --color never -i --follow --mmap --no-heading %s"))

  ;; find file at point
  (setq counsel-find-file-at-point t)

  ;; ignore . files or temporary files
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"))

  (bind-keys
   ([remap describe-bindings] . counsel-descbinds)
   ([remap finder-by-keyword] . counsel-package) ;C-h p
   ([remap describe-variable] . counsel-describe-variable)
   ([remap describe-function] . counsel-describe-function)
   ([remap bookmark-jump] . counsel-bookmark) ;Jump to book or set it if it doesn't exist, C-x r b
   ([remap bookmark-set] . counsel-bookmark)  ;C-x r m
   ([remap find-file] . counsel-find-file)
   ("C-c g g" . counsel-git-grep)
   ("C-c f" . counsel-imenu)
   ("M-y" . counsel-yank-pop)
   ("C-c F" . ivy-imenu-anywhere)
   ("C-c d s" . describe-symbol)
   ("C-c d f" . counsel-faces)
   ("C-c d d" . counsel-descbinds)
   ("C-c r g" . counsel-rg)))

(provide 'setup-counsel)

;; interesting counsel commands
;; `counsel-file-jump' -> get all the files in a directory recursively
;; `counsel-dired-jump' -> Switch to any open dired buffer
;; `counsel-locate' -> use locate command to find a file and search through the results
;; `counsel-colors-emacs' -> list all the colors emacs recognises
;; `counsel-colors-web' -> list all the colors that the web browser recognises
;; `counsel-command-history' -> browse through all the commands entered in `M-x'
;; `counsel-yank-pop' -> access the kill ring using ivy
;; `counsel-unicode-char' -> search through Unicode characters using ivy
;; `counsel-rg' -> search the all files in the current directory using `ripgrep'
;; `counsel-descbinds' -> lists all the key bindings in the current buffer
;; `counsel-mark-ring' -> access the mark ring for the current buffer using ivy
;; `counsel-faces' -> lists all the face colours in emacs
;; `counsel-ibuffer' -> run swiper search on content
;; `counsel-apropos' -> search apropos through counsel
;; `counsel-org-entity' -> search through a list of all the available org entities
;; `counsel-org-got-all' -> search through all headings for the open org files
;; `modi/counsel-org-tag' -> Adds aligning tags across file/heading
;; `counsel-org-agenda-headlines' -> search headings of all the files in org agenda list
;; `counsel-org-file' -> browse through all the attachments for a given org file
;; `counsel-yank-directory' -> yank the current directory in minibuffer with `C-M-y'
