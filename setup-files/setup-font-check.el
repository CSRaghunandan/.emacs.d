;;; setup-font-check.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-25 06:32:46 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; set Iosevka font only if it available
(defun rag-set-face (frame)
  "Configure faces on frame creation"
  (select-frame frame)
  (if (display-graphic-p)
      (when (member "Iosevka SS08" (font-family-list))
        (set-frame-font "Iosevka SS08 Medium-10" nil t))))

(add-hook 'after-make-frame-functions #'rag-set-face)

;; set frame font when running emacs normally
(when (member "Iosevka SS08" (font-family-list))
  (set-frame-font "Iosevka SS08 Medium-10" nil t))

  (provide 'setup-font-check)
