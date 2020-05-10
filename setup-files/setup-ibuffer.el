;;; setup-ibuffer.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-05-04 14:16:54 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; ibuffer: for easy management of buffers
(use-package ibuffer
  :straight nil
  :bind (:map ibuffer-mode-map
              ("h" . hydra-ibuffer-main/body))
  :init (setq ibuffer-filter-group-name-face
              '(:inherit (font-lock-type-face bold)))
  :config

  ;; Don't show scratch and messages in iBuffer
  (require 'ibuf-ext)
  (add-to-list #'ibuffer-never-show-predicates "^\\*Messages")
  (add-to-list #'ibuffer-never-show-predicates "^\\*Scratch")
  (add-to-list #'ibuffer-never-show-predicates "^\\*Bookmark List")
  ;; hide filter groups which are empty
  (setq ibuffer-show-empty-filter-groups nil)

  (bind-key "C-x C-b" #'ibuffer-jump)

  (defhydra hydra-ibuffer-main (:color pink :hint nil)
    "
^Mark^         ^Actions^           ^View^          ^Select^              ^Navigation^
_m_: mark      _D_: delete         _g_: refresh    _q_: quit             _k_:   ↑    _h_
_u_: unmark    _s_: save marked    _S_: sort       _TAB_: toggle         _RET_: visit
_*_: specific  _a_: all actions    _/_: filter     _o_: other window     _j_:   ↓    _l_
_t_: toggle    _h_: toggle hydra                 C-o: other win no-select
"
    ("m" ibuffer-mark-forward)
    ("u" ibuffer-unmark-forward)
    ("*" hydra-ibuffer-mark/body :color blue)
    ("t" ibuffer-toggle-marks)

    ("D" ibuffer-do-delete)
    ("s" ibuffer-do-save)
    ("a" hydra-ibuffer-action/body :color blue)

    ("g" ibuffer-update)
    ("S" hydra-ibuffer-sort/body :color blue)
    ("/" hydra-ibuffer-filter/body :color blue)
    ("H" describe-mode :color blue)

    ("h" ibuffer-backward-filter-group)
    ("k" ibuffer-backward-line)
    ("l" ibuffer-forward-filter-group)
    ("j" ibuffer-forward-line)
    ("RET" ibuffer-visit-buffer :color blue)

    ("TAB" ibuffer-toggle-filter-group)

    ("o" ibuffer-visit-buffer-other-window :color blue)
    ("q" quit-window :color blue)
    ("h" nil :color blue))

  (defhydra hydra-ibuffer-mark (:color teal :columns 5
                                       :after-exit (hydra-ibuffer-main/body))
    "Mark"
    ("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-action (:color teal :columns 4
                                         :after-exit
                                         (if (eq major-mode 'ibuffer-mode)
                                             (hydra-ibuffer-main/body)))
    "Action"
    ("A" ibuffer-do-view "view")
    ("E" ibuffer-do-eval "eval")
    ("F" ibuffer-do-shell-command-file "shell-command-file")
    ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
    ("H" ibuffer-do-view-other-frame "view-other-frame")
    ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
    ("M" ibuffer-do-toggle-modified "toggle-modified")
    ("O" ibuffer-do-occur "occur")
    ("P" ibuffer-do-print "print")
    ("Q" ibuffer-do-query-replace "query-replace")
    ("R" ibuffer-do-rename-uniquely "rename-uniquely")
    ("T" ibuffer-do-toggle-read-only "toggle-read-only")
    ("U" ibuffer-do-replace-regexp "replace-regexp")
    ("V" ibuffer-do-revert "revert")
    ("W" ibuffer-do-view-and-eval "view-and-eval")
    ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
    ("b" nil "back"))

  (defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
    "Sort"
    ("i" ibuffer-invert-sorting "invert")
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("v" ibuffer-do-sort-by-recency "recently used")
    ("s" ibuffer-do-sort-by-size "size")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
    "Filter"
    ("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("f" ibuffer-filter-by-filename "filename")
    (">" ibuffer-filter-by-size-gt "size")
    ("<" ibuffer-filter-by-size-lt "size")
    ("/" ibuffer-filter-disable "disable")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  ;; dont ask for confirmation whenever killing a buffer
  (setq ibuffer-expert t))

;; Display icons for all buffers in ibuffer.
;; https://github.com/seagle0128/all-the-icons-ibuffer
(use-package all-the-icons-ibuffer
  :config
  ;; enable ibuffer all-the-icons support
  (all-the-icons-ibuffer-mode 1)

  ;; open counsel-find-file in the same directory as the buffer in ibuffer
  (with-eval-after-load 'counsel
    (with-no-warnings
      (defun my-ibuffer-find-file ()
        (interactive)
        (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                   (if (buffer-live-p buf)
                                       (with-current-buffer buf
                                         default-directory)
                                     default-directory))))
          (counsel-find-file default-directory)))
      (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))))

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix
        (concat
         (all-the-icons-octicon "file-directory"
                                :face ibuffer-filter-group-name-face
                                :v-adjust 0.0
                                :height 1.0)
         " "
          "Project: ")))

(provide 'setup-ibuffer)
