;;; setup-abbrev.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-15 02:38:14 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; abbrev: expand abbreviations
(use-package abbrev :straight nil
  :hook
  ((prog-mode org-mode text-mode erc-mode LaTeX-mode) . abbrev-mode)
  (expand-load
   . (lambda ()
       (add-hook 'expand-expand-hook 'indent-according-to-mode)
       (add-hook 'expand-jump-hook 'indent-according-to-mode)))
  :bind*
  ("C-;" . endless/ispell-word-then-abbrev)
  :config
  ;; Silently save abbrevs on quitting emacs
  (setq save-abbrevs 'silently)

  ;;Read the abbreviations file on startup
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file))

  (defun endless/simple-get-word ()
    (car-safe (save-excursion (ispell-get-word nil))))

  (defun endless/ispell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
    (interactive "P")
    (let (bef aft)
      (save-excursion
        (while (if (setq bef (endless/simple-get-word))
                   ;; Word was corrected or used quit.
                   (if (ispell-word nil 'quiet)
                       nil ; End the loop.
                     ;; Also end if we reach `bob'.
                     (not (bobp)))
                 ;; If there's no word at point, keep looking
                 ;; until `bob'.
                 (not (bobp)))
          (backward-word)
          (backward-char))
        (setq aft (endless/simple-get-word)))
      (if (and aft bef (not (equal aft bef)))
          (let ((aft (downcase aft))
                (bef (downcase bef)))
            (define-abbrev
              (if p local-abbrev-table global-abbrev-table)
              bef aft)
            (message "\"%s\" now expands to \"%s\" %sally"
                     bef aft (if p "loc" "glob")))
        (user-error "No typo at or before point")))))

(provide 'setup-abbrev)
