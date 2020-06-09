;;; setup-visual.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-06-10 00:03:11 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; https://github.com/Fanael/rainbow-delimiters
;; different colours for each nested delimiter
(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

;; better defaults
;; don't show splash screen when starting emacs
(setq inhibit-splash-screen t)
;; don't echo startup message of GNU emacs
(setq inhibit-startup-echo-area-message t)

;; show line numbers globally
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;;; all frame related settings
;; silence all bell rings
(setq ring-bell-function 'ignore)
;; Do not make mouse wheel accelerate its action (example: scrolling)
(setq mouse-wheel-progressive-speed nil)

(setq frame-resize-pixelwise t) ; Allow frame size to inc/dec by a pixel
(setq tooltip-mode nil) ; Disable tooltip appearance on mouse hover

;; make sure emacsclient starts at fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; make the titlebar transparent in mac
(when (is-mac-p)
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; resize windows proportionally
(setq window-combination-resize t)

;; show cursor in non-selected window as a hollow
(setq-default cursor-in-non-selected-windows 'hollow)



;; Show actual lines instead of the page break char ^L
;; https://github.com/purcell/page-break-lines
;; enter page-break character in Emacs by entering `C-q C-l'
(use-package page-break-lines
  :hook ((prog-mode org-mode) . page-break-lines-mode)
  :config
  (setq page-break-lines-max-width 80)
  (setq page-break-lines-char 45))

;; Easily adjust the font size in all Emacs frames
;; https://github.com/purcell/default-text-scale/
(use-package default-text-scale
  :defer 5
  :config (default-text-scale-mode))

;; turn on the native fill column indicator
;; requires emacs27 or newer
(use-package display-fill-column-indicator
  :straight nil
  :hook ((prog-mode
          conf-mode
          yaml-mode
          org-mode) . display-fill-column-indicator-mode)
  :config (setq-default display-fill-column-indicator-character 124))



;; fontify-face: Fontify symbols representing faces with that face.
;; https://github.com/Fuco1/fontify-face/tree/master
(use-package fontify-face
  :defer t)

;; solaire-mode is an aesthetic plugin that helps visually distinguish
;; file-visiting windows from other types of windows (like popups or sidebars)
;; by giving them a slightly different background.
;; https://github.com/hlissner/emacs-solaire-mode
(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg)

  ;; let solaire remap fringes faces as well. Looks nicer IMO
  (setq solaire-mode-remap-fringe t))

;; lorem-ipsum: Add filler lorem ipsum text to Emacs
;; https://github.com/jschaf/emacs-lorem-ipsum/
(use-package lorem-ipsum
  :defer 5
  :config

  (defcustom unpackaged/lorem-ipsum-overlay-exclude nil
    "List of regexps to exclude from `unpackaged/lorem-ipsum-overlay'."
    :type '(repeat regexp))

  ;; https://github.com/alphapapa/unpackaged.el/blob/master/unpackaged.el#L290
;;;###autoload
  (defun unpackaged/lorem-ipsum-overlay ()
    "Overlay all text in current buffer with \"lorem ipsum\" text.
When called again, remove overlays.  Useful for taking
screenshots without revealing buffer contents.
Each piece of non-whitespace text in the buffer is compared with
regexps in `unpackaged/lorem-ipsum-overlay-exclude', and ones
that match are not overlaid.  Note that the regexps are compared
against the entire non-whitespace token, up-to and including the
preceding whitespace, but only the alphabetic part of the token
is overlaid.  For example, in an Org buffer, a line that starts
with:
  #+TITLE: unpackaged.el
could be matched against the exclude regexp (in `rx' syntax):
  (rx (or bol bos blank) \"#+\" (1+ alnum) \":\" (or eol eos blank))
And the line would be overlaid like:
  #+TITLE: parturient.et"
    (interactive)
    (require 'lorem-ipsum)
    (let ((ovs (overlays-in (point-min) (point-max))))
      (if (cl-loop for ov in ovs
                   thereis (overlay-get ov :lorem-ipsum-overlay))
          ;; Remove overlays.
          (dolist (ov ovs)
            (when (overlay-get ov :lorem-ipsum-overlay)
              (delete-overlay ov)))
        ;; Add overlays.
        (let ((lorem-ipsum-words (--> lorem-ipsum-text
                                      (-flatten it) (apply #'concat it)
                                      (split-string it (rx (or space punct)) 'omit-nulls)))
              (case-fold-search nil))
          (cl-labels ((overlay-match (group)
                                     (let* ((beg (match-beginning group))
                                            (end (match-end group))
                                            (replacement-word (lorem-word (match-string group)))
                                            (ov (make-overlay beg end)))
                                       (when replacement-word
                                         (overlay-put ov :lorem-ipsum-overlay t)
                                         (overlay-put ov 'display replacement-word))))
                      (lorem-word (word)
                                  (if-let* ((matches (lorem-matches (length word))))
                                      (apply-case word (downcase (seq-random-elt matches)))
                                    ;; Word too long: compose one.
                                    (apply-case word (downcase (compose-word (length word))))))
                      (lorem-matches (length &optional (comparator #'=))
                                     (cl-loop for liw in lorem-ipsum-words
                                              when (funcall comparator (length liw) length)
                                              collect liw))
                      (apply-case (source target)
                                  (cl-loop for sc across-ref source
                                           for tc across-ref target
                                           when (not (string-match-p (rx lower) (char-to-string sc)))
                                           do (setf tc (string-to-char (upcase (char-to-string tc)))))
                                  target)
                      (compose-word (length)
                                    (cl-loop while (> length 0)
                                             for word = (seq-random-elt (lorem-matches length #'<=))
                                             concat word
                                             do (cl-decf length (length word)))))
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward (rx (group (1+ (or bol bos blank (not alpha)))
                                                   (0+ (not (any alpha blank)))
                                                   (group (1+ alpha))
                                                   (0+ (not (any alpha blank)))))
                                        nil t)
                (unless (cl-member (match-string 0) unpackaged/lorem-ipsum-overlay-exclude
                                   :test (lambda (string regexp)
                                           (string-match-p regexp string)))
                  (overlay-match 2))
                (goto-char (match-end 2))))))))))

;; Simple way to manipulate overlay for Emacs.
;; https://github.com/emacsorphanage/ov/
(use-package ov
  :defer t)

;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

;; Pop a posframe (just a child-frame) at point
;; https://github.com/tumashu/posframe
(use-package posframe)

;; built in mode `image-mode' for viewing images
(use-package image
  :straight nil
  ;; disable line numbers for images
  :hook (image-mode . (lambda ()
                        (display-line-numbers-mode -1))))

(provide 'setup-visual)
