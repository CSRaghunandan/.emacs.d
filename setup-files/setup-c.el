;; Timestamp: <2017-07-22 13:53:13>
;; all the configuration for C/C++ projects

;;; features:
;; Source code navigation using RTags
;; Source code completion using Irony
;; Syntax checking with Flycheck
;; CMake automation with cmake-ide

;; A c/c++ client/server indexer for c/c++/objc[++] with integration for Emacs
;; based on clang.
;; https://github.com/Andersbakken/rtags
(use-package rtags
  :config

  (rtags-enable-standard-keybindings)

  ;; ivy completion frontend for rtags
  (use-package ivy-rtags
    :config
    (setq rtags-display-result-backend 'ivy))

  ;; TODO: after you learn how to use CMake, start using rtags, till then irony
  ;; will suffice
  ;; (use-package flycheck-rtags
  ;;   :config
  ;;   (defun my-flycheck-rtags-setup ()
  ;;     (flycheck-select-checker 'rtags)
  ;;     (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  ;;     )
  ;;   ;; c-mode-common-hook is also called by c++-mode
  ;;   (add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup))
  )

;; irony: A C/C++ minor mode for Emacs powered by libclang
;; https://github.com/Sarcasm/irony-mode
(use-package irony
  :config
  ;; company backend for irony completion server
  ;; https://github.com/Sarcasm/company-irony
  (use-package company-irony)
  ;; completions for C/C++ header files
  ;; https://github.com/hotpxl/company-irony-c-headers
  (use-package company-irony-c-headers)

  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))

  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (setq company-backends (delete 'company-semantic company-backends))

  (defun my-c-mode-hook ()
    (set (make-local-variable 'company-backends)
         '((company-irony-c-headers company-irony company-files company-yasnippet))))

  (add-hook 'c-mode-common-hook #'my-c-mode-hook)

  (use-package flycheck-irony
    :config
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

  ;; irony-eldoc: eldoc support for irony
  ;; https://github.com/ikirill/irony-eldoc
  (use-package irony-eldoc
    :config (add-hook 'irony-mode-hook #'irony-eldoc)))

;; cmake-ide: Use Emacs as a C/C++ IDE
;; https://github.com/atilaneves/cmake-ide
;; (use-package cmake-ide
;;   :config (cmake-ide-setup))

;; To have cmake-ide automatically create a compilation commands file in your
;; project root create a .dir-locals.el containing the following:
;; ((nil . ((cmake-ide-build-dir . "<PATH_TO_PROJECT_BUILD_DIRECTORY>"))))

(use-package cc-mode
  :config
  ;; start the rtags process automatically if it's not started
  ;; (add-hook 'c-mode-common-hook 'rtags-start-process-unless-running)

  ;; configure autocompletions for C/C++ using irony
  (add-hook 'c-mode-common-hook 'company-mode)
  (add-hook 'c-mode-common-hook 'irony-mode)

  (add-hook 'c-mode-common-hook 'flycheck-mode)

  (add-hook 'c-mode-common-hook 'smart-dash-mode)

  (setq-default c-default-style "linux"
        c-basic-offset 4))

(provide 'setup-c)
