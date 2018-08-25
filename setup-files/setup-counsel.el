;;; setup-counsel.el -*- lexical-binding: t -*-
;; Time-stamp: <2018-08-26 00:28:00 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; counsel: ivy backends for a lot more commands
;; https://github.com/abo-abo/swiper
(use-package counsel
  :after ivy
  :chords (("JJ" . counsel-imenu)
           (";'" . counsel-M-x))
  :defer 0.5
  :bind
  (:map read-expression-map
        ("C-r" . counsel-expression-history))
  :config

  (counsel-mode)
  (with-eval-after-load 'org-agenda
    (bind-key "C-c C-q" #'counsel-org-tag-agenda org-agenda-mode-map))

  (if (executable-find "rg")
      ;; if rg is installed, use rg for `counsel-grep-or-swiper' and `counsel-rg'
      (setq counsel-grep-base-command "rg --line-number --smart-case -M 150 --color never --mmap --no-heading %s %s"
            ;; add `--follow' option to allow search through symbolic links
            counsel-rg-base-command "rg --line-number --smart-case -M 150 --color never --follow --mmap --no-heading %s"
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
   `(("R" ,(reloading (given-file #'rename-file "Move")) "move")
     ("F" find-file-other-frame "other frame")
     ("d" ,(reloading #'confirm-delete-file) "delete")
     ("p" (lambda (path) (with-ivy-window (insert (file-relative-name path default-directory)))) "insert relative path")
     ("P" (lambda (path) (with-ivy-window (insert path))) "insert absolute path")
     ("l" (lambda (path) "Insert org-link with relative path"
            (with-ivy-window (insert (format "[[./%s]]" (file-relative-name path default-directory))))) "insert org-link (rel. path)")
     ("L" (lambda (path) "Insert org-link with absolute path"
            (with-ivy-window (insert (format "[[%s]]" path)))) "insert org-link (abs. path)")))

  (ivy-add-actions
   'counsel-projectile-find-file
   `(("m" ,(reloading (given-file #'rename-file "Move")) "move")
     ("b" counsel-find-file-cd-bookmark-action "cd bookmark")))

  ;;;###autoload
  (defun +ivy-git-grep-other-window-action (x)
    "Opens the current candidate in another window."
    (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
      (select-window
       (with-ivy-window
         (let ((file-name   (match-string-no-properties 1 x))
               (line-number (match-string-no-properties 2 x)))
           (find-file-other-window (expand-file-name file-name (ivy-state-directory ivy-last)))
           (goto-char (point-min))
           (forward-line (1- (string-to-number line-number)))
           (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
           (run-hooks 'counsel-grep-post-action-hook)
           (selected-window))))))

  (ivy-add-actions
   'counsel-ag ; also applies to `counsel-rg' & `counsel-pt'
   '(("O" +ivy-git-grep-other-window-action "open in other window")))

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
   ([remap finder-by-keyword] . counsel-package) ; C-? p
   ([remap bookmark-set] . counsel-bookmark)
   ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
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
;; `counsel-descbinds' -> lists all the key bindings in the current buffer `C-? b'
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
;; `counsel-bookmark' -> Jump to book or set it if it doesn't exist, C-x r b
