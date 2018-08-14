;;; setup-white-space.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-15 03:10:36 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; ws-butler: clean trailing whitespaces unobtrusively
;; https://github.com/lewang/ws-butler
(use-package ws-butler
  :hook ((prog-mode . ws-butler-mode)))

;;; Whitespace Mode/Show Long Lines
(use-package whitespace :defer t
  :ensure nil
  :config
  (setq whitespace-line-column nil)  ;When nil, set the value to `fill-column'
  (setq whitespace-style
        '(face
          trailing                    ;White space at end of lines
          tabs                        ;tab-mark ;`tab-mark' shows tabs as '»'
          spaces space-mark           ;`space-mark' shows spaces as '.'
          space-before-tab space-after-tab ;Mix of tabs and spaces
          ;; lines   ;highlight lines that extend beyond `whitespace-line-column'
          lines-tail ;highlight only characters beyond `whitespace-line-column'
          ;; newline newline-mark
          ;; empty ;blank lines at BOB or EOB
          indentation)) ;highlight spaces/tabs at BOL depending on `indent-tabs-mode'
  )

(provide 'setup-white-space)
