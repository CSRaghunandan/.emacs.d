;;; setup-docker.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-23 15:31:01 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; dockerfile-mode: An emacs mode for handling Dockerfiles
;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode)
  :hook (dockerfile-mode . (lambda ()
                             (lsp)
                             (lsp-ui-doc-mode)
                             (lsp-ui-sideline-mode)
                             (company-mode)
                             (flycheck-mode)))
  :config
  (defun my-docker-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-lsp company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (add-hook 'dockerfile-mode-hook #'my-docker-mode-hook))

;; docker: manager docker from emacs
;; https://github.com/Silex/docker.el
(use-package docker
  :defer t)

;; docker-compose-mode: Major mode for editing docker-compose files
;; https://github.com/meqif/docker-compose-mode
(use-package docker-compose-mode
  :defer t)

;; docker-tramp: TRAMP integration for docker containers
;; https://github.com/emacs-pe/docker-tramp.el
(use-package docker-tramp
  :defer t)

(provide 'setup-docker)
