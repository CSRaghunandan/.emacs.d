;; Timestamp: <2017-06-16 15:51:40>

;; yaml-mode: major-mode for editing yaml files
;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :defer t
  :hook ((yaml-mode . highlight-numbers-mode)))

;; conf-mode: major-mdoe for editing conf files
;; https://github.com/jrockway/emacs/blob/master/lisp/textmodes/conf-mode.el
(use-package conf-mode
  :ensure nil
  :defer t
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
  :defer t
  :mode (("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
         ("nginx.conf" . nginx-mode)))

;; emacs mode for editing ssh config files.
;; https://github.com/jhgorrell/ssh-config-mode-el
(use-package ssh-config-mode
  :defer t)

;; dot-env: An Emacs major mode for .env files
;; https://github.com/preetpalS/emacs-dotenv-mode/tree/master
(use-package dotenv-mode
  :defer t
  :mode (("\\.env\\..*\\'" . dotenv-mode)))

;; apache-mode: Major mode for editing Apache configuration files
;; https://github.com/emacs-php/apache-mode
(use-package apache-mode)

(provide 'setup-config-files)
