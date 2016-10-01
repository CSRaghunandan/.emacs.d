(use-package recentf
  :defer 1
  :config
  (recentf-mode 1)
  (setq recentf-max-menu-items 200)
  (setq recentf-max-saved-items 200)
  (setq recentf-exclude '(".*-autoloads\\.el\\'"
                          "[/\\]\\.elpa/"
                          ".*?autoloads.el$"
                          "company-statistics-cache.el"
                          "loaddefs.el")))

(provide 'setup-recentf)
