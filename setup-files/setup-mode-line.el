;; Time-stamp: <2017-09-30 02:30:29 csraghunandan>

;; diminish, powerline, spaceline, eldoc

;; powerline: a better looking mode line for emacs
;; https://github.com/milkypostman/powerline
(use-package powerline
  :config

  (when (eq system-type 'darwin)
    (setq powerline-image-apple-rgb t))
  (setq powerline-default-separator 'wave)
  (setq powerline-height 17)

  ;; spaceline: spacemacs powerline theme
  ;; https://github.com/TheBB/spaceline
  (use-package spaceline
    :config
    (require 'spaceline-config)
    (spaceline-spacemacs-theme)
    (spaceline-info-mode)
    (spaceline-toggle-anzu-off)
    (spaceline-toggle-buffer-modified-on)
    (spaceline-toggle-erc-track-on)
    (spaceline-toggle-minor-modes-off)

    (spaceline-define-segment narrow
      "Display Narrowed when buffer is narrowed."
      (when (buffer-narrowed-p)
        "narrow"))

    (spaceline-define-segment mc
      "Display the number of multiple cursors"
      (if (> (mc/num-cursors) 1)
        (concat "mc: " (format
                        "%d"
                        (mc/num-cursors)))
        nil))

    ;; show projectile project root and indicate when narrowed
    (spaceline-spacemacs-theme '(narrow mc projectile-root))))

;; eldoc: show function parameters/ type of thing at point
(use-package eldoc :diminish eldoc-mode)

;; macro to rename mode-name for major-modes
(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "typescript-mode" typescript-mode "TS")
(rename-modeline "haskell-mode" haskell-mode "𝞴=")

(provide 'setup-mode-line)
