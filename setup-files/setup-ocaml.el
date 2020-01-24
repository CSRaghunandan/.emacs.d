;;; setup-ocaml.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-15 03:05:00 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; configuration for OCaml mode

;; caml - major mode for editing OCaml, Objective Label programs
;; https://github.com/ocaml/ocaml/tree/trunk/emacs
(use-package caml :defer t
  :config
  ;; tuareg: Major mode for editing OCaml code
  ;; https://github.com/ocaml/tuareg
  (use-package tuareg
    :mode (("\\.ml[ily]?$" . tuareg-mode)
           ("\\.topml$" . tuareg-mode))
    :init
    (add-hook 'tuareg-mode-hook (lambda ()
                                  (abbrev-mode -1)))

    ;; Make OCaml-generated files invisible to filename completion
    (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".cmti" ".annot"))
      (add-to-list 'completion-ignored-extensions ext))

    (with-eval-after-load 'smartparens
      ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
      (sp-local-pair 'tuareg-mode "'" nil :actions nil)
      (sp-local-pair 'tuareg-mode "`" nil :actions nil)))

  ;; merlin: Context sensitive completion for OCaml in Vim and Emacs
  ;; https://github.com/ocaml/merlin/tree/beta
  (use-package merlin
    :defer t
    :config
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (setq merlin-completion-with-doc t)

    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)

    (defun my-ocaml-mode-hook()
      (set (make-local-variable 'company-backends)
           '((merlin-company-backend company-files :with company-yasnippet)
             (company-dabbrev-code company-dabbrev))))
    (add-hook 'tuareg-mode-hook #'my-ocaml-mode-hook)
    (add-hook 'tuareg-mode-hook 'company-mode))

  ;; OCaml support for Flycheck using Merlin
  ;; https://github.com/flycheck/flycheck-ocaml
  (use-package flycheck-ocaml
    :config
    (with-eval-after-load 'merlin
      ;; Disable Merlin's own error checking
      (setq merlin-error-after-save nil)

      ;; Enable Flycheck checker
      (flycheck-ocaml-setup))
    (add-hook 'tuareg-mode-hook 'flycheck-mode))

  ;; utop is an improved toplevel for OCaml. It can run in a terminal or in
  ;; Emacs. It supports line editing, history, real-time and context sensitive
  ;; completion, colors, and more.
  ;; https://github.com/diml/utop
  (use-package utop :defer t
    :init
    (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
    (add-hook 'tuareg-mode-hook 'utop-minor-mode)
    :config
    (if (executable-find "opam")
        (setq utop-command "opam config exec -- utop -emacs")
      (warn "Cannot find \"opam\" executable.")))

  ;; ocp-indent: Indentation tool for OCaml, to be used from editors like Emacs
  ;; and Vim. http://www.typerex.org/ocp-indent.html
  ;; https://github.com/OCamlPro/ocp-indent
  (use-package ocp-indent :defer t
    :init
    (add-hook 'tuareg-mode-hook 'ocp-setup-indent)))

(provide 'setup-ocaml)
