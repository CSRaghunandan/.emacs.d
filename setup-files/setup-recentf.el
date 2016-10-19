;; Time-stamp: <2016-10-18 14:14:32 csraghunandan>

;; view the list recently opened files
(use-package recentf
  :defer 1
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 100)
  (setq recentf-exclude '(".*-autoloads\\.el\\'"
                          "[/\\]\\.elpa/"
                          ".*?autoloads.el$"
                          "company-statistics-cache.el"
                          "loaddefs.el")))

(provide 'setup-recentf)
