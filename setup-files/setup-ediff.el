;;; setup-ediff.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-15 02:54:51 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; ediff: show the difference between two files intearctively
(use-package ediff
  :straight nil
  :config
  ;; No separate frame for ediff control buffer
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)
  ;; Split windows horizontally in ediff (instead of vertically)
  (setq ediff-split-window-function #'split-window-horizontally)

  (defun modi/ediff-dwim ()
    "Do ediff as I mean.
If a region is active, call `ediff-regions-wordwise'.
Else if the frame has 2 windows with identical major modes,
  - Do `ediff-files' if the buffers are associated to files and the buffers
    have not been modified.
  - Do `ediff-buffers' otherwise.
Else if the current is a file buffer with a VC backend, call `vc-ediff'
Else call `ediff-buffers'."
    (interactive)
    (let* ((num-win (safe-length (window-list)))
           (bufa (get-buffer (buffer-name)))
           (filea (buffer-file-name bufa))
           (modea (with-current-buffer bufa major-mode))
           bufb fileb modeb)
      (save-excursion
        (other-window 1)
        (setq bufb (get-buffer (buffer-name)))
        (setq fileb (buffer-file-name bufb))
        (setq modeb (with-current-buffer bufb major-mode)))
      (cond
       ;; If a region is selected
       ((region-active-p)
        (call-interactively #'ediff-regions-wordwise))
       ;; Else if 2 windows with same major modes
       ((and (= 2 num-win)
             (eq modea modeb))
        (if ;; If either of the buffers is not associated to a file,
            ;; or if either of the buffers is modified
            (or (null filea)
                (null fileb)
                (buffer-modified-p bufa)
                (buffer-modified-p bufb))
            (progn
              (message "Running (ediff-buffers \"%s\" \"%s\") .." bufa bufb)
              (ediff-buffers bufa bufb))
          (progn
            (message "Running (ediff-files \"%s\" \"%s\") .." filea fileb)
            (ediff-files filea fileb))))
       ;; Else if file in current buffer has a vc backend
       ((and filea
             (vc-registered filea))
        (call-interactively #'vc-ediff))
       ;; Else call `ediff-buffers'
       (t
        (call-interactively #'ediff-buffers))))))

(provide 'setup-ediff)
