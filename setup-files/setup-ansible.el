;;; setup-ansible.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-05 13:35:31 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;;; ansible configuration

;; detect filenames compatible with Ansible's recommended layout.
;; http://docs.ansible.com/playbooks_best_practices.html#directory-layout
(setq spacemacs--ansible-filename-re
      ".*\\(main\.yml\\|site\.yml\\|encrypted\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+\\)")

(defun spacemacs//ansible-should-enable? ()
  (and (stringp buffer-file-name)
       (string-match spacemacs--ansible-filename-re buffer-file-name)))

(defun spacemacs/ansible-maybe-enable ()
  (when (spacemacs//ansible-should-enable?)
    (ansible 1)))

;; minor mode for ansible files
;; https://github.com/k1LoW/emacs-ansible
(use-package ansible
  :hook ((yaml-mode . spacemacs/ansible-maybe-enable)
         (ansible . company-mode)
         (ansible . ansible-auto-decrypt-encrypt))
  :commands ansible-auto-decrypt-encrypt
  :init
  (put 'ansible-vault-password-file 'safe-local-variable #'stringp)
  :config
  (setq ansible-section-face 'font-lock-variable-name-face
        ansible-task-label-face 'font-lock-doc-face))

;; company backend for ansible
;; https://github.com/krzysztof-magosa/company-ansible
(use-package company-ansible
  :config
  (defun my-ansible-hook ()
    "Add the ansible company backend only for when ansible mode is active."
    (set (make-local-variable 'company-backends)
         '((company-ansible company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))

  (add-hook 'ansible-hook #'my-ansible-hook))

;; ansible documentation for emacs
;; https://github.com/emacsorphanage/ansible-doc/
(use-package ansible-doc
  :hook (asnible . ansible-doc-mode))

;; jinja2 major mode for emacs
;; https://github.com/paradoxxxzero/jinja2-mode/
(use-package jinja2-mode
  :mode "\\.j2$")

(provide 'setup-ansible)
