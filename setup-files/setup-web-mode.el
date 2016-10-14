;; Time-stamp: <2016-10-13 17:56:36 csraghunandan>

;; web-mode
;; http://web-mode.org/ , https://github.com/fxbois/web-mode
(use-package web-mode
  :mode (("\\.html$" . web-mode)
	 ("\\.djhtml$" . web-mode))
  :bind ("C-c o b" . browse-url-of-file)
  :config
  ;; start emmet mode after web-mode launches
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook (lambda() (highlight-indent-guides-mode -1)))
  (defun my-web-mode-hook ()
    "Hook for `web-mode'."
    (set (make-local-variable 'company-backends)
         '((company-tern company-css company-web-html company-files))))
  (add-hook 'web-mode-hook 'my-web-mode-hook)

  ;; Enable JavaScript completion between <script>...</script> etc.
  (defadvice company-tern (before web-mode-set-up-ac-sources activate)
    "Set `tern-mode' based on current language before running company-tern."
    (message "advice")
    (if (equal major-mode 'web-mode)
	(let ((web-mode-cur-language
	       (web-mode-language-at-pos)))
	  (if (or (string= web-mode-cur-language "javascript")
		  (string= web-mode-cur-language "jsx"))
	      (unless tern-mode (tern-mode))
	    (if tern-mode (tern-mode -1))))))
  (add-hook 'web-mode-hook 'company-mode)

  ;; to get completion data for angularJS
  (use-package ac-html-angular :defer t)
  ;; to get completion for twitter bootstrap
  (use-package ac-html-bootstrap :defer t)

  ;; to get completion for HTML stuff
  ;; https://github.com/osv/company-web
  (use-package company-web)

  ;; snippets for HTML
  ;; https://github.com/smihica/emmet-mode
  (use-package emmet-mode
    :init (setq emmet-move-cursor-between-quotes t) ;; default nil
    :diminish (emmet-mode . " 𝛆")
    :bind (("C-]" . emmet-next-edit-point)
           ("C-[" . emmet-prev-edit-point))))

;; impatient mode - Live refresh of web pages
;; https://github.com/skeeto/impatient-mode
(use-package impatient-mode
  :diminish (impatient-mode . " 𝖎")
  :commands (impatient-mode))

;; configure CSS mode company backends
(use-package css-mode
  :config
  (defun my-css-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-css company-dabbrev-code company-yasnippet company-files))))
  (add-hook 'css-mode-hook 'my-css-mode-hook)
  (add-hook 'css-mode-hook 'company-mode))

(provide 'setup-web-mode)
