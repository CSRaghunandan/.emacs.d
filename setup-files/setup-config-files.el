;;; setup-config-files.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-24 23:36:50 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; yaml-mode: major-mode for editing yaml files
;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :hook ((yaml-mode . (lambda ()
                        (run-hooks 'prog-mode-hook)))
         (yaml-mode . (lambda ()
                        (lsp)
                        (lsp-ui-doc-mode)
                        (lsp-ui-sideline-mode)
                        (company-mode)
                        (flycheck-mode)))
         (yaml-mode . (lambda ()
                        (add-hook 'before-save-hook
                                  (lambda ()
                                    (time-stamp)
                                    (lsp-format-buffer)) nil t))))
  :config
  (defun my-yaml-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-lsp company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))
  (add-hook 'yaml-mode-hook #'my-yaml-mode-hook))

;; conf-mode: major-mdoe for editing conf files
;; https://github.com/jrockway/emacs/blob/master/lisp/textmodes/conf-mode.el
(use-package conf-mode
  :ensure nil
  :mode (("\\.conf\\'"    . conf-space-mode)
         ("\\.setup.*\\'" . conf-space-mode)
         ("/\\(Cargo.lock\\|\\.cargo/config\\)\\'" . conf-toml-mode)))

;; systemd: Major mode for editing systemd units
;; https://github.com/holomorph/systemd-mode
(use-package systemd
  :defer t)

;; nginx-mode: Emacs editing mode for Nginx config files
;; https://github.com/ajc/nginx-mode
(use-package nginx-mode
  :mode (("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
         ("nginx.conf" . nginx-mode)))

;; Add Nginx directives keywords to company-mode keywords alist.
;; https://github.com/stardiviner/company-nginx/
(use-package company-nginx
  :after nginx-mode
  :hook (nginx-mode . company-nginx-keywords))

;; emacs mode for editing ssh config files.
;; https://github.com/jhgorrell/ssh-config-mode-el
(use-package ssh-config-mode
  :defer t)

;; dot-env: An Emacs major mode for .env files
;; https://github.com/preetpalS/emacs-dotenv-mode/tree/master
(use-package dotenv-mode
  :mode (("\\.env\\..*\\'" . dotenv-mode)))

;; apache-mode: Major mode for editing Apache configuration files
;; https://github.com/emacs-php/apache-mode
(use-package apache-mode
  :defer t)

;; powershell: An Emacs mode for editing and running Microsoft PowerShell code.
;; https://github.com/jschaf/powershell.el
(use-package powershell
  :defer t)

;; Better .csv files editing
;; https://elpa.gnu.org/packages/csv-mode.html
(use-package csv-mode
  :mode "\\.[Cc][Ss][Vv]\\'"
  :config (setq csv-separators '("," ";" "|" " ")))

;; major mode to edit hex files
(use-package hexl
  :ensure nil
  :mode ("\\.\\(?:hex\\|nes\\)\\'" . hexl-mode))

;; major made for crontab files
;; https://github.com/emacs-pe/crontab-mode
(use-package crontab-mode
  :mode "\\.?cron\\(tab\\)?\\'")

;; this package provides direnv integration for emacs.
;; https://github.com/wbolster/emacs-direnv/
(use-package direnv :defer 2
  :config
  (direnv-mode))

(provide 'setup-config-files)
