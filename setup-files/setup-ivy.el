;;; setup-ivy.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-07-07 17:54:32 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; amx: An alternative M-x interface for Emacs.
;; https://github.com/DarwinAwardWinner/amx
(use-package amx :defer 0.5
  :config (amx-mode))

;; swiper: isearch with an overview!
;; https://github.com/abo-abo/swiper
;; `M-p' -> previous search item
;; `M-n' -> next search item
;; `M-n' -> to select the symbol at point in swiper
(use-package swiper
  :bind (("C-s" . swiper-isearch)
         ("M-s ." . swiper-isearch-thing-at-point))
  :config
  (setq swiper-action-recenter t
        ;; Jump to the beginning of match when leaving Swiper
        swiper-goto-start-of-match t))

;; ivy: incremental narrowing framework for Emacs
;; https://github.com/abo-abo/swiper
(use-package ivy
  :bind (("C-c u" . ivy-resume))
  :config
  (ivy-mode)

  (bind-chords
   :map ivy-minibuffer-map
   ("m," . ivy-beginning-of-buffer)
   (",." . ivy-end-of-buffer))

  (setq ivy-use-virtual-buffers t
        ivy-height 13
        ivy-count-format "%d/%d "
        ivy-virtual-abbreviate 'full ; Show the full virtual file paths
        ivy-extra-directories nil ; default value: ("../" "./")
        ivy-wrap t
        ivy-action-wrap t
        ivy-use-selectable-prompt t)

  ;; modify default search behaviour of ivy
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))

  (bind-keys
   :map ivy-occur-grep-mode-map
   ("n" . ivy-occur-next-line)
   ("p" . ivy-occur-previous-line)
   ("b" . backward-char)
   ("f" . forward-char)
   ("v" . ivy-occur-press) ; default f
   ("RET" . ivy-occur-press)))

;; Better experience with icons for ivy
;; https://github.com/seagle0128/all-the-icons-ivy-rich/
(use-package all-the-icons-ivy-rich :defer 1
  :config
  (all-the-icons-ivy-rich-mode 1)
  (setq all-the-icons-ivy-rich-icon-size 0.8))

;; More friendly interface for ivy
;; https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :hook (counsel-mode . ivy-rich-mode)
  :config
  ;; For better performance
  ;; Better experience with icons
  (setq ivy-rich-parse-remote-buffer nil))

;; hydra for ivy, its a part of the same repo as swiper
(use-package ivy-hydra)

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

;; `ivy-switch-view' ->  select a window configuration, decoupled from ivy-switch-buffer
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
