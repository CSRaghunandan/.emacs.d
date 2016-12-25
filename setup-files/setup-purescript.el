;; Time-stamp: <2016-12-25 14:27:42 csraghunandan>

;; configuration for purescript

;; major mode for editing purescript code
;; https://github.com/dysinger/purescript-mode
(use-package purescript-mode
  :config
  (add-hook 'purescript-mode-hook 'turn-on-purescript-decl-scan)

  ;; provide IDE like features for purescript
  ;; https://github.com/epost/psc-ide-emacs
  (use-package psc-ide
    :config
    (add-hook 'purescript-mode-hook
              (lambda ()
                (psc-ide-mode)
                (company-mode)
                (flycheck-mode)
                (turn-on-purescript-indentation)))

    (add-hook 'purescript-mode-hook
              (lambda ()
                (add-hook 'before-save-hook
                          (lambda ()
                            (time-stamp)
                            (xah-clean-whitespace)
                            (force-backup-of-buffer)) nil t)))))

(provide 'setup-purescript-mode)

;; C-c C-s	psc-ide-server-start
;; C-c C-q	psc-ide-server-quit
;; C-c C-t	psc-ide-show-type
;; C-c C-i	psc-ide-add-import
;; C-c C-a	psc-ide-add-clause
;; C-c C-c	psc-ide-case-split
;; C-c C-l	psc-ide-load-all
;; C-c C-b	psc-ide-rebuild
;; C-c C-S-l	psc-ide-load-module
;; C-c M-s	psc-ide-flycheck-insert-suggestion
