;; Time-stamp: <2016-12-05 21:46:12 csraghunandan>

;; flx, ivy

;; flx - fuzzy sorting heuristics algorithm
;; needed for sorting the results from fuzzy search in ivy
(use-package flx)

;; ivy
;; incremental narrowing framework for emacs
(use-package ivy
  :diminish ivy-mode
  :init (ivy-mode 1)
  :bind
  (:map ivy-minibuffer-map
        ("C-'" . ivy-avy))
  :config
  ;; C-c C when in ivy minibuffer will copy all the completion candidates to kill ring.
  (bind-key "C-c C" 'ivy-kill-ring-save ivy-minibuffer-map)
  (setq ivy-use-virtual-buffers t
	ivy-height 13
	ivy-initial-inputs-alist nil
	ivy-count-format ""
	ivy-virtual-abbreviate 'full ; Show the full virtual file paths
	ivy-extra-directories '("./") ; default value: ("../" "./")
	ivy-wrap t)
  (setq ivy-re-builders-alist '((counsel-grep-or-swiper . ivy--regex-plus)
				(t . ivy--regex-fuzzy))))

(provide 'setup-ivy)

;; ivy
;; * `C-c C-o' to run `ivy-occur' on the results of ivy
;; * when in `ivy-occur' enter wgrep mode by pressing `C-x C-q', to save changes
;; * press `C-x C-s' to save changes or `C-c C-k' to abort changes
;;   when in ivy-minibuffer, press `C-c C' to copy all the completion candidates to kill ring
;; * press C-' when in ivy-minibuffer to use avy to select completion candidates
