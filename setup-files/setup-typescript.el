;;; setup-typescript.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-12-13 19:54:00 csraghunandan>v

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; typescript config

;; typescript: major mode for editing typescript files
;; https://github.com/ananthakumaran/typescript.el
(use-package typescript-mode
  :hook ((typescript-mode . (lambda ()
                              (my-tide-setup-hook)
                              (add-node-modules-path)
                              (company-mode))))
  :bind ((:map typescript-mode-map
               ("C-c C-t" . tide-documentation-at-point)
               ("C-c T p" . typescript/open-region-in-playground)))
  :config
  (defun typescript/open-region-in-playground (start end)
    "Open selected region in http://www.typescriptlang.org/Playground
                 If nothing is selected - open the whole current buffer."
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))
                   (list (point-min) (point-max))))
    (browse-url (concat "http://www.typescriptlang.org/Playground#src="
                        (url-hexify-string (buffer-substring-no-properties start end))))))

;; tide: TypeScript Interactive Development Environment for Emacs
;; https://github.com/ananthakumaran/tide
(use-package tide
  :after typescript-mode
  :config
  (defun my-tide-setup-hook ()
    ;; configure tide
    (tide-setup)
    ;; highlight identifiers
    (tide-hl-identifier-mode +1)
    ;;enable eldoc-mode
    (eldoc-mode)
    ;; enable flycheck
    (flycheck-mode)

    ;; format typescript files using prettier
    (prettier-js-mode)

    ;; company-backends setup
    (set (make-local-variable 'company-backends)
         '((company-tide company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))

  ;; use 2 space indentation
  (setq typescript-indent-level 2)

  ;; add tslint checker for flycheck
  (flycheck-add-next-checker 'typescript-tide
                             'typescript-tslint)

  (setq tide-completion-detailed t)

  (add-hook 'tide-mode-hook
            (lambda ()
              (add-hook 'kill-buffer-hook #'+javascript|cleanup-tide-processes nil t))))

;;;###autoload
(defun +javascript|cleanup-tide-processes ()
  "Clean up dangling tsserver processes if there are no more buffers with
`tide-mode' active that belong to that server's project."
  (when tide-mode
    (unless (cl-loop with project-name = (tide-project-name)
                     for buf in (delq (current-buffer) (buffer-list))
                     if (and (buffer-local-value 'tide-mode buf)
                             (with-current-buffer buf
                               (string= (tide-project-name) project-name)))
                     return buf)
      (kill-process (tide-current-server)))))

(provide 'setup-typescript)
