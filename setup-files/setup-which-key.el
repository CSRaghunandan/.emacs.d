;;; setup-which-key.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-04-29 11:56:28 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; which-key : show popup of keybindings starting with a prefix
;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :init (which-key-mode)
  :bind ("C-h M-m" . which-key-show-full-major-mode)
  :config
  ;; make which-key popup to the right
  (which-key-setup-side-window-right)

  (setq which-key-compute-remaps t) ;Show correct descriptions for remapped keys
  (setq which-key-allow-multiple-replacements t) ;Default = nil

  (setq which-key-replacement-alist
        ;; Replacements for how part or whole of FUNCTION is replaced when
        ;; which-key displays
        ;;   KEY → FUNCTION
        '(((nil . "Prefix Command") . (nil . "prefix"))
          ((nil . "which-key-show-next-page") . (nil . "next pg"))
          ((nil . "/body\\'") . (nil . "")) ; Remove display the "/body" portion of hydra fn names
          ((nil . "\\`hydra-") . (nil . "+h/"))
          ((nil . "\\`org-babel-") . (nil . "ob/"))
          ((nil . "\\`artist-select-op-") . (nil . "")) ; Make artist-mode function names less verbose
          ((nil . "\\`artist-select-") . (nil . "sel-"))
          ((nil . "\\`artist-toggle-") . (nil . "toggle-"))

          ;; Replacements for how KEY is replaced when which-key displays
          ;;   KEY → FUNCTION
          (("\\`DEL\\'" . nil) . ("BS" . nil)) ; backspace key
          (("<next>" . nil) . ("PgDn" . nil))
          (("<prior>" . nil) . ("PgUp" . nil))))

  ;; Change what string to display for a given *complete* key binding
  ;; Eg: After "C-x", display "8 → +unicode" instead of "8 → +prefix"
  (which-key-add-key-based-replacements
    "C-x 8"   "unicode"
    "C-x a"   "abbrev/expand"
    "C-x r"   "rectangle/register/bookmark"
    "C-x v"   "VC"
    "C-c C-v" "org-babel"))

(provide 'setup-which-key)
