;; Time-stamp: <2017-02-12 16:49:38 csraghunandan>

;; configuration for purescript

;; purescript-mode: major mode for editing purescript files
;; https://github.com/dysinger/purescript-mode
(use-package purescript-mode
  :diminish (purescript-indentation-mode . "𝐈𝐧")
  :diminish (psc-ide-mode . "𝐏-𝐈𝐃𝐄")
  :config
  (add-hook 'purescript-mode-hook 'turn-on-purescript-decl-scan)

  ;; psc-ide: provide IDE like features for purescript
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
                            (xah-clean-whitespace)) nil t)))))

(provide 'setup-purescript)

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
