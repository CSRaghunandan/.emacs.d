;;; -*- lexical-binding: t -*-
;; Time-stamp: <2018-07-08 22:30:15 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; counsel: ivy backends for a lot more commands
;; https://github.com/abo-abo/swiper
(use-package counsel
  :after ivy
  :bind* (([remap execute-extended-command] . counsel-M-x))
  :chords (("JJ" . counsel-imenu)
           (";'" . counsel-M-x))
  :init
  (counsel-mode)

  (bind-keys
   :map read-expression-map
   ("C-r" . counsel-expression-history))

  (with-eval-after-load 'org-agenda
    (bind-key "C-c C-q" #'counsel-org-tag-agenda org-agenda-mode-map))

  :config

  (if (executable-find "rg")
      ;; if rg is installed, use rg for `counsel-grep-or-swiper' and `counsel-rg'
      (setq counsel-grep-base-command "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
            ;; add `--follow' option to allow search through symbolic links
            counsel-rg-base-command "rg --line-number --color never -i --follow --mmap --no-heading %s"
            ;; Use ripgrep for counsel-git
            counsel-git-cmd "rg --files")
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
   'counsel-projectile-find-file
   `(("d" ,(reloading #'confirm-delete-file) "delete")
     ("m" ,(reloading (given-file #'rename-file "Move")) "move")
     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")))

  ;; find file at point
  (setq counsel-find-file-at-point t)

  ;; ignore . files or temporary files
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; File names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; File names ending with # or ~
         "\\|\\(?:\\`.+?[#~]\\'\\)"))

  (setq counsel-mode-override-describe-bindings t
        counsel-describe-function-function 'helpful-function
        counsel-describe-variable-function 'helpful-variable)

  (setq counsel-grep-post-action-hook '(recenter))

  (bind-keys
   ([remap describe-bindings] . counsel-descbinds) ; C-? b
   ([remap finder-by-keyword] . counsel-package) ; C-? p
   ([remap bookmark-jump] . counsel-bookmark) ; Jump to book or set it if it doesn't exist, C-x r b
   ([remap bookmark-set] . counsel-bookmark)
   ([remap find-file] . counsel-find-file)
   ("C-c d s" . describe-symbol)
   ("C-c d f" . counsel-faces)
   ("C-c r g" . counsel-rg)))

;; Add more ivy features for projectile related commands
;; https://github.com/ericdanan/counsel-projectile/tree/master
(use-package counsel-projectile
  :after (counsel projectile)
  :config (counsel-projectile-mode 1))

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
