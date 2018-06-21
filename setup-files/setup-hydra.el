;; Time-stamp: <2018-06-22 00:38:32 csraghunandan>

;; hydra: tie related commands into a family of short bindings with a common
;; prefix - a Hydra
;; https://github.com/abo-abo/hydra
(use-package hydra
  :config (hydra-add-font-lock))

(provide 'setup-hydra)

;; hydras: ibuffer, origami, move, yasnippet, info, macros, diff-hl dired sort,
;;         bm, toggle case, langtools, kruecolor smartparens, multi-term,
;;         aprops, quickrun, org-clock, command-log info-to, js2-refactor,
;;         mark-org-table, smerge-mode, font-resize pdftools, timestamps
