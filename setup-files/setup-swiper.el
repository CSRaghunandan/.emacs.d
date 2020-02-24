;;; setup-swiper.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-24 23:40:29 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; swiper: isearch with an overview!
;; https://github.com/abo-abo/swiper
(use-package swiper
  :bind (("C-s" . counsel-grep-or-isearch-swiper))
  :config
  (setq swiper-action-recenter t
        ;; Jump to the beginning of match when leaving Swiper
        swiper-goto-start-of-match t)

;;;###autoload
  (defun counsel-grep-or-isearch-swiper (&optional initial-input)
    "Call `swiper' for small buffers and `counsel-grep' for large ones.
When non-nil, INITIAL-INPUT is the initial search pattern."
    (interactive)
    (if (or (not buffer-file-name)
            (buffer-narrowed-p)
            (ignore-errors
              (file-remote-p buffer-file-name))
            (jka-compr-get-compression-info buffer-file-name)
            (funcall counsel-grep-use-swiper-p))
        (swiper-isearch initial-input)
      (when (file-writable-p buffer-file-name)
        (save-buffer))
      (counsel-grep initial-input))))

(provide 'setup-swiper)

;; swiper
;; `M-p' -> previous search item
;; `M-n' -> next search item
;; `M-n' -> to select the symbol at point in swiper
