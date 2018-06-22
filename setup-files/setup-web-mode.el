;; Time-stamp: <2018-06-22 12:23:40 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghuandan rnraghunandan@gmail.com

;; company-web: to get completion for HTML stuff
;; https://github.com/osv/company-web
(use-package company-web)

;; web-mode: major-mode for editing multiple web formats
;; http://web-mode.org/ , https://github.com/fxbois/web-mode
(use-package web-mode
  :mode (("\\.html$" . web-mode)
         ("\\.djhtml$" . web-mode)
         ("\\.tsx$" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.phtml\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  :bind (:map web-mode-map
         ("C-c o b" . browse-url-of-file)
         ("C-c [" . emmet-prev-edit-point)
         ("C-c ]" . emmet-next-edit-point))
  :hook ((web-mode . company-mode))
  :config

  (custom-set-variables
   '(web-mode-markup-indent-offset 2)
   '(web-mode-css-indent-offset 2)
   '(web-mode-code-indent-offset 2)
   '(css-indent-offset 2))

  ;; highlight matching tag
  (setq web-mode-enable-current-element-highlight t)

  (defun my-unfontify-function (beg end)
    (remove-list-of-text-properties beg end '(display)))
  (defun my-register-unfontify ()
    (setq font-lock-unfontify-region-function 'my-unfontify-function))
  (add-hook 'web-mode-hook 'my-register-unfontify t)

  (defun my-tide-setup-hook ()
    ;; configure tide
    (tide-setup)
    ;;enable eldoc-mode
    (eldoc-mode)
    ;; highlight identifiers
    (tide-hl-identifier-mode +1)
    ;; enable flycheck
    (flycheck-mode)

    ;; company-backends setup
    (set (make-local-variable 'company-backends)
         '((company-tide company-files company-yasnippet)))

    ;; enable typescript-tslint checker
    (flycheck-add-mode 'typescript-tslint 'web-mode))

  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (my-tide-setup-hook))))

  (defun my-web-mode-hook ()
    "Hook for `web-mode'."
    (set (make-local-variable 'company-backends)
         '((company-css company-web-html company-files))))
  (unless (string-equal "tsx" (file-name-extension buffer-file-name))
    (add-hook 'web-mode-hook 'my-web-mode-hook))

  ;; colorize colors in buffers
  (setq web-mode-enable-css-colorization t))

;; impatient mode: Live refresh of web pages
;; https://github.com/skeeto/impatient-mode
(use-package impatient-mode
  :commands (impatient-mode))

;; emmet-mode: dynamic snippets for HTML
;; https://github.com/smihica/emmet-mode
(use-package emmet-mode
  :hook ((web-mode . emmet-mode))
  :config
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-indentation 2))

(provide 'setup-web-mode)
