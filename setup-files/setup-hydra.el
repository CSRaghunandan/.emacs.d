;; Time-stamp: <2018-07-30 14:01:33 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; hydra: tie related commands into a family of short bindings with a common
;; prefix - a Hydra
;; https://github.com/abo-abo/hydra
(use-package hydra
  :config (hydra-add-font-lock))

(provide 'setup-hydra)

;; hydras: ibuffer, origami, move, yasnippet, info, macros, diff-hl dired sort,
;;         bm, toggle case, langtools, kruecolor smartparens, multi-term,
;;         aprops, quickrun, org-clock, command-log info-to, js2-refactor,
;;         mark-org-table, smerge-mode, font-resize pdftools, timestamps, flycheck
