;; Time-stamp: <2016-12-31 16:30:16 csraghunandan>

;; diminish, powerline, spaceline, eldoc

;; diminish-mode - to hide minor modes in mode-line
;; https://github.com/emacsmirror/diminish
(use-package diminish :demand t)

;; powerline - a better looking mode line for emacs
;; https://github.com/milkypostman/powerline
(use-package powerline
  :config
  (setq powerline-default-separator 'utf-8)

  ;; spaceline theme for powerline
  ;; https://github.com/TheBB/spaceline
  (use-package spaceline
    :config
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)
    (spaceline-info-mode)
    (spaceline-toggle-anzu-off)
    (spaceline-toggle-selection-info-off)
    (spaceline-toggle-hud-off)
    (setq spaceline-toggle-buffer-modified t)
    (spaceline-toggle-buffer-size-off)
    (spaceline-toggle-erc-track-on)
    (setq spaceline-minor-modes-separator " ")

    (spaceline-define-segment narrow
      "Display Narrowed when buffer is narrowed."
      (when (buffer-narrowed-p)
        "narrow"))

    ;; indicate narrowed buffer in modeline
    (spaceline-spacemacs-theme 'narrow)))

;; diminish eldoc-mode in mode-line
(use-package eldoc
  :diminish eldoc-mode)

(provide 'setup-mode-line)
