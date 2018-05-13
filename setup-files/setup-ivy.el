;; Time-stamp: <2018-05-13 18:49:49 csraghunandan>

;; Smart M-x (smex): sort extended commands by last invoked
;; https://github.com/nonsequitur/smex/
(use-package smex
  :config (smex-initialize))

;; ivy: incremental narrowing framework for Emacs
;; https://github.com/abo-abo/swiper
(use-package ivy
  :bind (("C-c u" . ivy-resume))
  :init (ivy-mode 1)
  :config

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
