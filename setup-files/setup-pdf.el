;;; setup-pdf.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-12-02 18:59:32 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; pdf-tools: Emacs support library for PDF files.
;; https://github.com/politza/pdf-tools
(use-package pdf-tools
  :bind
  (:map pdf-view-mode-map
        ("C-c h p" . hydra-pdftools/bodqy))
  :config
  ;; https://github.com/politza/pdf-tools/issues/312#issuecomment-329537742
  ;; Build the program (if necessary) without asking first, if NO-QUERY-P is
  ;; non-nil.
  (pdf-tools-install)

  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use isearch instead of swiper
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; turn off cua so copy works
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))

  ;; add history for PDF files
  (add-hook 'pdf-view-mode-hook #'pdf-history-minor-mode)
  ;; more fine-grained zooming
  (setq pdf-view-resize-factor 1.1)

  ;; keyboard shortcuts
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  ;; wait until map is available
  (with-eval-after-load "pdf-annot"
    (define-key pdf-annot-edit-contents-minor-mode-map (kbd "<return>") 'pdf-annot-edit-contents-commit)
    (define-key pdf-annot-edit-contents-minor-mode-map (kbd "<S-return>") 'newline)
    ;; save after adding comment
    (advice-add 'pdf-annot-edit-contents-commit :after 'bjm/save-buffer-no-args))

  (defhydra hydra-pdftools (:color blue :hint nil)
    "
                                                                      ╭───────────┐
       Move  History   Scale/Fit     Annotations  Search/Link    Do   │ PDF Tools │
   ╭──────────────────────────────────────────────────────────────────┴───────────╯
         ^^_g_^^      _B_    ^↧^    _+_    ^ ^     [_al_] list    [_s_] search    [_u_] revert buffer
         ^^^↑^^^      ^↑^    _H_    ^↑^  ↦ _W_ ↤ [_am_] markup  [_o_] outline   [_i_] info
         ^^_p_^^      ^ ^    ^↥^    _0_    ^ ^     [_at_] text    [_F_] link      [_d_] dark mode
         ^^^↑^^^      ^↓^  ╭─^─^─┐  ^↓^  ╭─^ ^─┐   [_ad_] delete  [_f_] search link
    _h_ ←pag_e_→ _l_  _N_  │ _P_ │  _-_    _b_   [_aa_] dired
         ^^^↓^^^      ^ ^  ╰─^─^─╯  ^ ^  ╰─^ ^─╯   [_y_]  yank
         ^^_n_^^      ^ ^  _r_eset slice box
         ^^^↓^^^
         ^^_G_^^
   --------------------------------------------------------------------------------
        "
    ("\\" hydra-master/body "back")
    ("<ESC>" nil "quit")
    ("al" pdf-annot-list-annotations)
    ("ad" pdf-annot-delete)
    ("aa" pdf-annot-attachment-dired)
    ("am" pdf-annot-add-markup-annotation)
    ("at" pdf-annot-add-text-annotation)
    ("y"  pdf-view-kill-ring-save)
    ("+" pdf-view-enlarge :color red)
    ("-" pdf-view-shrink :color red)
    ("0" pdf-view-scale-reset)
    ("H" pdf-view-fit-height-to-window)
    ("W" pdf-view-fit-width-to-window)
    ("P" pdf-view-fit-page-to-window)
    ("n" pdf-view-next-page-command :color red)
    ("p" pdf-view-previous-page-command :color red)
    ("d" pdf-view-dark-minor-mode)
    ("b" pdf-view-set-slice-from-bounding-box)
    ("r" pdf-view-reset-slice)
    ("g" pdf-view-first-page)
    ("G" pdf-view-last-page)
    ("e" pdf-view-goto-page)
    ("o" pdf-outline)
    ("s" pdf-occur)
    ("i" pdf-misc-display-metadata)
    ("u" pdf-view-revert-buffer)
    ("F" pdf-links-action-perfom)
    ("f" pdf-links-isearch-link)
    ("B" pdf-history-backward :color red)
    ("N" pdf-history-forward :color red)
    ("l" image-forward-hscroll :color red)
    ("h" image-backward-hscroll :color red)))

;; pdfgrep: PDFGrep is a GNU/Emacs module providing grep comparable facilities but for PDF files
;; https://github.com/jeremy-compostella/pdfgrep
(use-package pdfgrep
  :after pdf-tools
  :config (pdfgrep-mode))

(provide 'setup-pdf)

;; ** Useful key bindings
;; |--------------------------------+-----------------------------|
;; | Key Binding                    | Description                 |
;; |--------------------------------+-----------------------------|
;; | n                              | Next page                   |
;; | p                              | Previous page               |
;; | SPC                            | Scroll up                   |
;; | S-SPC                          | Scroll down                 |
;; | C-n                            | Next line/page              |
;; | C-p                            | Previous line/page          |
;; |--------------------------------+-----------------------------|
;; | <goto-line binding>            | Go to page                  |
;; |--------------------------------+-----------------------------|
;; | + / =                          | Enlarge view                |
;; | -                              | Shrink view                 |
;; | 0                              | Reset view                  |
;; | W                              | Fit page width              |
;; | H                              | Fit page height             |
;; | P                              | Fit page                    |
;; | s m <drag mouse to select box> | PDF zooms to that selection |
;; | s r                            | Resets the above view slice |
;; |--------------------------------+-----------------------------|
;; | M-s w                          | isearch-forward-word        |
;; | M-s o                          | pdf-isearch-occur           |
;; |--------------------------------+-----------------------------|
;; | m                              | bookmark-set                |
;; |                                | (jump to bookmark using     |
;; |                                |  C-x r b)                   |
;; |--------------------------------+-----------------------------|
;; | View in Printed mode           | C-c C-r p                   |
;; | View in Midnight mode          | C-c C-r m                   |
;; |--------------------------------+-----------------------------|
