;;; setup-json.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-01-24 11:56:37 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; json-mode: Major mode for editing JSON files with emacs

;; https://github.com/joshwnj/json-mode
(use-package json-mode
  :mode "\\.js\\(?:on\\|[hl]int\\(rc\\)?\\)\\'"
  :config
  (add-hook 'json-mode-hook #'prettier-js-mode)
  (setq json-reformat:indent-width 2)
  (setq json-reformat:pretty-string? t)
  (setq js-indent-level 2))

(provide 'setup-json)
