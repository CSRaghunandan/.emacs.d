;;; setup-haskell.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-15 02:57:35 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; haskell-mode: major mode for editing haskell files
;; https://github.com/haskell/haskell-mode
(use-package haskell-mode :defer t
  :hook
  ((haskell-mode . (lambda ()
                     (intero-mode)
                     (my-haskell-mode-hook)
                     (company-mode)
                     (setq haskell-indentation-mode nil)
                     (haskell-collapse-mode))))
  :config
  (defun my-haskell-mode-hook ()
    "Hook for `haskell-mode'."
    (set (make-local-variable 'company-backends)
         '((company-intero company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))

;; structured-haskell-mode: paredit like features for haskell code
;; https://github.com/chrisdone/structured-haskell-mode
(use-package shm
  :if (executable-find "structured-haskell-mode")
  :hook ((haskell-mode . structured-haskell-mode)
         (structured-haskell-mode . (lambda ()
                                      (smartparens-mode -1)
                                      (smartparens-strict-mode -1))))))

;; hasky-stack: interface to stack haskell development tool
;; https://github.com/hasky-mode/hasky-stack
(use-package hasky-stack
  :bind
  (("C-c s i" . hasky-stack-new)
   ("C-c s e" . hasky-stack-execute)))

;; intero-mode: IDE features for haskell
;; commercialhaskell.github.io/intero
(use-package intero
  :after haskell-mode
  :ensure-system-package (hlint . "stack install hlint")
  :config
  ;; enable hlint checker for flycheck
  (flycheck-add-next-checker 'intero
                             'haskell-hlint))

;; hlint-refactor:Emacs bindings for hlint's --refactor option
;; https://github.com/mpickering/hlint-refactor-mode
(use-package hlint-refactor
  :ensure-system-package (refactor . "stack install apply-refact")
  :hook (haskell-mode . hlint-refactor-mode))

;; hindent: format haskell code automatically
;; https://github.com/chrisdone/hindent
(use-package hindent
  :ensure-system-package (hindent . "stack install hindent")
  :hook (haskell-mode . hindent-mode)
  :config
  ;; reformat the buffer using hindent on save
  (setq hindent-reformat-buffer-on-save t)

  ;; Suppress errors when hindent--before-save fails
  (with-eval-after-load 'hindent
    (when (require 'nadvice)
      (defun mu-hindent--before-save-wrapper (oldfun &rest args)
        (with-demoted-errors "Error invoking hindent: %s"
          (let ((debug-on-error nil))
            (apply oldfun args))))
      (advice-add
       'hindent--before-save :around 'mu-hindent--before-save-wrapper))))

(provide 'setup-haskell)

;; Haskell intero config
;; `C-c C-l' to load the current file to stack GHCi
;; `C-c C-z' to open stack GHCi
;; `C-c C-t' to see the type of the thing at point
;; `M-.' jump to definition
;; `M-,' jump back from definition
;; `M-q' to format the expression at point using hindent
;; `C-M-\' to format the selected region using hindent

;; structured-haskell-mode
;; `C-j' will automatically add a comma when inside a ilst
;; `C-+' will will look at the current node and add another operand in the direction the cursor is leaning towards
;; automatically reindents code as we type(similar to aggressive indent)
;; multi line strings: press `C-j' while inside a string to split it to next line
;; `C-c |' to add case splits for simple sum types
;; `C-k' will act like `sp-kill-hybrid-sexp'
;; `)' will move cursor to the end of  the parent
;; Skeletons: Typing prefixes of common syntax will auto-fill in the structure
;;            with "slots" that you can hit TAB to jump to, which auto-disappear
;;            when you type in them
;;
