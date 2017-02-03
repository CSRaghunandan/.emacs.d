;; Time-stamp: <2017-02-04 01:05:06 csraghunandan>

;; diminish, powerline, spaceline, eldoc

;; diminish-mode - to hide minor modes in mode-line
;; https://github.com/emacsmirror/diminish
(use-package diminish :demand t)

;; powerline - a better looking mode line for emacs
;; https://github.com/milkypostman/powerline
(use-package powerline)

;; yahoo-weather
;; https://github.com/emacsmirror/yahoo-weather
(use-package yahoo-weather
  :init
  (setq yahoo-weather-location "Mysore")
  (yahoo-weather-mode))

;; add all-the-icons support for mode-line
(use-package spaceline-all-the-icons :after spaceline :ensure nil
  :load-path "~/.emacs.d/my-elisp-code/spaceline-all-the-icons.el")

;; spaceline - A powerline theme
;; https://github.com/TheBB/spaceline
(use-package spaceline :after powerline
  :config (setq-default mode-line-format '("%e" (:eval (spaceline-ml-ati)))))

;; diminish eldoc-mode in mode-line
(use-package eldoc
  :diminish eldoc-mode)

(provide 'setup-mode-line)
