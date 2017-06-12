;; Time-stamp: <2017-06-12 18:23:13 csraghunandan>

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

(>=e "26.0"
    (setq auto-hscroll-mode 'current-line))

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
  ("v" golden-ratio-scroll-screen-up)
  ("V" golden-ratio-scroll-screen-down)
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
  :config
  (bind-key "C-v" #'golden-ratio-scroll-screen-up)
  (bind-key "M-v" #'golden-ratio-scroll-screen-down))

;; dumb-jump: jump to definitions using `rg' or `ag'
;; https://github.com/jacktasia/dumb-jump
(use-package dumb-jump
  :bind (:map dumb-jump-mode-map
         ("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back))
  :init
  (setq dumb-jump-selector 'ivy)
  (add-hook 'prog-mode-hook #'dumb-jump-mode))

(provide 'setup-movement)
