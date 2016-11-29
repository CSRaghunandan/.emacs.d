;; Timestamp: <2016-11-29 13:19:18>

;; which-key : show popup of keybindings starting with a prefix
;; https://github.com/justbur/emacs-which-key

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  ;; show which-key pop on the side, if not possible popup at bottom instead
  (which-key-setup-side-window-right-bottom)

  ;; List of "special" keys for which a KEY is displayed as just K but with
  ;; "inverted video" face.
  (setq which-key-special-keys '("SPC"
                                 "TAB"
                                 "RET"
                                 "DLT" ; delete key
                                 "BS" ; backspace key
                                 "ESC")))

(provide 'setup-which-key)
