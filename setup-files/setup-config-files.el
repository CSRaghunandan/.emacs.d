;; Timestamp: <2017-06-16 15:51:40>

;; yaml-mode: major-mode for editing yaml files
;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :defer t)

;; conf-mode: major-mdoe for editing conf files
;; https://github.com/jrockway/emacs/blob/master/lisp/textmodes/conf-mode.el
(use-package conf-mode
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
  :mode (("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)))

(provide 'setup-config-files)
