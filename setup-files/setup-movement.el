;;; setup-movement.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-08-19 00:04:26 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; All the configuration related to movement in emacs

(defun rag/multi-pop-to-mark (orig-fun &rest args)
  "When popping the mark, continue popping until the cursor actually moves.
Try the repeated popping up to 10 times."
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        (apply orig-fun args)))))
(advice-add 'pop-to-mark-command :around #'rag/multi-pop-to-mark)

;; Ensure that we can quickly pop the mark several times by typing
;; C-u C-SPC C-SPC, instead of having to type C-u C-SPC C-u C-SPC.
(setq set-mark-command-repeat-pop t)

;; mwim: move to the beginning or end of line smartly
;; https://github.com/alezost/mwim.el
(use-package mwim
  :bind (:map prog-mode-map
              ("C-a" . mwim-beginning-of-code-or-line-or-comment)
              ("C-e" . mwim-end-of-code-or-line)))

;; hydra for movement keys
(defhydra hydra-move
  (:body-pre (next-line)
             :hint nil)
  "
_f_: -> char        _F_: -> word         _n_: -> line       _a_: beginning-of-line
_b_: <- char        _B_: <- word         _p_: <- line       _e_: end-of-line

_m_: set mark       _v_: scroll down     _l_: recenter      _'_: avy       _`_: avy-word
_j_: goto mark      _V_: scroll up       _w_: ace-window    _._: -> buffer _,_: <- buffer

_s_: -> sentence    _a_: -> paragraph    _g_: -> page       _>_: end-of-buffer
_S_: <- sentence    _A_: <- paragraph    _G_: <- page       _<_: beginning-of-buffer
 "
  ("n" next-line)
  ("p" previous-line)
  ("f" forward-char)
  ("b" backward-char)
  ("a" mwim-beginning-of-code-or-line-or-comment)
  ("e" mwim-end-of-code-or-line)
  ("v" scroll-up)
  ("V" scroll-down)
  ("F" forward-word)
  ("B" backward-word)
  ("l" recenter-top-bottom)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("g" forward-page)
  ("G" backward-page)
  ("s" forward-sentence)
  ("S" backward-sentence)
  ("a" forward-paragraph)
  ("A" backward-paragraph)
  ("'" avy-goto-char-timer :color blue)
  ("`" avy-goto-word-1 :color blue)
  ("w" ace-window)
  ("m" org-mark-ring-push)
  ("j" org-mark-ring-goto)
  ("." next-buffer)
  ("," previous-buffer)
  ("q" nil :color blue))
(bind-key "M-m" 'hydra-move/body)

;; scroll half screen up or down and highlight current line before and after scrolling
;; https://github.com/jixiuf/golden-ratio-scroll-screen
(use-package golden-ratio-scroll-screen
  :bind (("C-v" . golden-ratio-scroll-screen-up)
         ("M-v" . golden-ratio-scroll-screen-down))
  :config
  ;; don't use obnoxious colors for `golden-ratio-scroll'
  (set-face-attribute 'golden-ratio-scroll-highlight-line-face nil
                      :background 'unspecified :foreground 'unspecified))

;; dumb-jump: jump to definitions using `rg' or `ag'
;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :hook ((prog-mode . dumb-jump-mode))
  :config
  (setq dumb-jump-selector 'ivy)

  (defhydra dumb-jump-hydra (:color red :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")
    ("q" nil "Quit" :color blue))
  (bind-key "C-c d j" #'dumb-jump-hydra/body prog-mode-map))

(setq recenter-positions '(0.50 0.07 0.93)) ;default: '(middle top bottom)
;; First C-l  -> 0.50: Put point vertically at the middle of the window
;; Second C-l -> 0.07: Put point close to the top of the window. If
;;                     (window-height) returns 70, that's roughly 4 lines.
;; Third C-l  -> 0.93: Put point close to the bottom of the window ~ 3 lines.
;; With the default values of `recenter-positions' and `scroll-margin' (0),
;; the "top" position is the first line of the window, and the "bottom"
;; position is the last line. Above settings provides a margin of 3 or 4 lines
;; for my default window size for the "top" and "bottom" iterations.

(provide 'setup-movement)
