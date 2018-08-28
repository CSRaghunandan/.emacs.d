;;; setup-ivy.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-28 16:42:04 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; amx: An alternative M-x interface for Emacs.
;; https://github.com/DarwinAwardWinner/amx
(use-package amx
  :config
  (amx-mode))

;; ivy: incremental narrowing framework for Emacs
;; https://github.com/abo-abo/swiper
(use-package ivy
  :bind (("C-c u" . ivy-resume))
  :defer 0.5
  :config
  (ivy-mode)

  (bind-chords
   :map ivy-minibuffer-map
   ("m," . ivy-beginning-of-buffer)
   (",." . ivy-end-of-buffer))

  (setq ivy-use-virtual-buffers t
        ivy-height 13
        ivy-count-format "%d/%d "
        ivy-initial-inputs-alist nil
        ivy-virtual-abbreviate 'full ; Show the full virtual file paths
        ivy-extra-directories nil ; default value: ("../" "./")
        ivy-format-function 'ivy-format-function-arrow
        ivy-wrap t
        ivy-use-selectable-prompt t)

  (bind-keys
   :map ivy-occur-grep-mode-map
   ("n" . ivy-occur-next-line)
   ("p" . ivy-occur-previous-line)
   ("b" . backward-char)
   ("f" . forward-char)
   ("v" . ivy-occur-press) ; default f
   ("RET" . ivy-occur-press)))

;; ivy-rich: More friendly interface for ivy
;; https://github.com/Yevgnen/ivy-rich/
(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  (setq ivy-rich-path-style 'abbrev)

  (setq ivy-rich--display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 30))  ; return the candidate itself
            (ivy-rich-switch-buffer-size (:width 7))  ; return the buffer size
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right)); return the buffer indicators
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))          ; return the major mode info
            (ivy-rich-switch-buffer-project (:width 15 :face success))             ; return project name using `projectile'
            (ivy-rich-switch-buffer-path
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path
                        x
                        (ivy-rich-minibuffer-width 0.3))))))  ; return file path relative to project root or `default-directory' if project is nil
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))  ; thr original transfomer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the command
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))  ; return the docstring of the function
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))  ; the original transformer
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))  ; return the docstring of the variable
          )))

(provide 'setup-ivy)

;; ivy
;; `C-c C-o' to run `ivy-occur' on the results of ivy
;; when in `ivy-occur' enter wgrep mode by pressing `C-x C-q', to save changes
;;   press `C-x C-s' to save changes or `C-c C-k' to abort changes
;;   when in ivy-minibuffer, press `C-c C' to copy all the completion candidates to kill ring
;; 'C-' when in ivy-minibuffer to use avy to select completion candidates
;; `~' when in `counsel-find-file' to go to home directory
;; `//' when in `counsel-find-file' to go to root directory
;; `C-p RET' to select the current candidate you are typing instead of `C-M-j'
;; `M-o w' to copy the current candidate to the kill ring.
;; `M-o i' to insert the current candidate into the buffer.
;; `C-d' in `ivy-occur' to delete entries from the ivy-occur file
;; `counsel-org-capture' ->  completion for org-capture
;; `counsel-minibuffer-history' -> generalization of counsel-expression-history and counsel-shell-command-history
;; `counsel-org-file' ->  browse all attachments for the current Org file
;; `counsel-org-goto' -> completion for Org headings
;; `counsel-org-goto-all' -> completion for Org headings in all open buffers
;; `ivy-switch-view' ->  select a window configuration, decoupled from ivy-switch-buffer
;; `counsel-fzf' -> completion for fzf
;; `C-c C-k' in `ivy-switch-buffer' to kill the buffer while still in ivy-minibuffer

;; Call `ivy-immediate-done' if you want to use whatever you typed in the
;; search field, and ignore the suggestions provided by ivy in the list.
;;
;;  C-u <`ivy-alt-done' binding> <-- `ivy-immediate-done'
;;
;; This is useful especially when renaming files (and the name you want to
;; rename to partially matches one of the existing files).
;;
;; |----------------------------+----------------+------------------------------------------------------|
;; | Command                    | ivy map        | Function                                             |
;; |                            | Bindings       |                                                      |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-done                   | C-j            | Exit the minibuffer with the selected candidate.     |
;; |                            | (default: C-m) | Try to leave `ivy' as soon as possible.              |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-alt-done               | C-m or RET     | Exit the minibuffer with the selected candidate.     |
;; |                            | (default: C-j) | When ARG is t, acts like `ivy-immediate-done'.       |
;; |                            |                | Try NOT to leave `ivy' at the soonest. For           |
;; |                            |                | instance, if a directory name completion is          |
;; |                            |                | possible, do that and list that directory's          |
;; |                            |                | content in `ivy' instead of opening that dir         |
;; |                            |                | in `dired'.                                          |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-immediate-done         | C-S-m          | Exit the minibuffer with the current text,           |
;; |                            |                | ignoring the candidates.                             |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-partial-or-done        | TAB            | Attempts partial completion, extending current line  |
;; |                            |                | input as much as possible. "TAB TAB" is the same as  |
;; |                            |                | `ivy-alt-done'.                                      |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-call                   | C-M-m          | Call the current action without exiting completion.  |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-next-line-and-call     | C-M-n          | Move cursor vertically down ARG candidates.          |
;; |                            |                | Call the permanent action if possible.               |
;; | ivy-previous-line-and-call | C-M-p          | Move cursor vertically up ARG candidates.            |
;; |                            |                | Call the permanent action if possible.               |
;; |----------------------------+----------------+------------------------------------------------------|
;; | ivy-dispatching-done       | M-o            | Presents valid actions from which to choose. When    |
;; |                            |                | only one action is available, there is no difference |
;; |                            |                | between this and `ivy-done'.                         |
;; |----------------------------+----------------+------------------------------------------------------|
