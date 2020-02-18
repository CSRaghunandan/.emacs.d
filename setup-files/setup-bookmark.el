;;; setup-bookmark.el -*- lexical-binding: t; -*-
;; Time-stamp: <2020-02-18 12:32:53 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; bm: visible bookmarks within a buffer
;; https://github.com/joodland/bm
(use-package bm :defer t
  :config
  ;; buffer persistence on by default
  (setq-default bm-buffer-persistence t)

  ;; Load bm repository
  (when (file-exists-p bm-repository-file)
    (bm-repository-load))

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when Emacs is killed, so we
  ;; must save all bookmarks first.
  (defun modi/bm-save-all-bm-to-repository ()
    (bm-buffer-save-all)
    (bm-repository-save))
  (add-hook 'kill-emacs-hook #'modi/bm-save-all-bm-to-repository)
  (add-hook 'after-save-hook #'bm-buffer-save)
  ;; The `after-save-hook' is not necessary to use to achieve persistence,
  ;; but it makes the bookmark data in repository more in sync with the file
  ;; state.

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  (setq bm-cycle-all-buffers t
        bm-in-lifo-order t
        bm-highlight-style 'bm-highlight-line-and-fringe)

  (defun rag/bm-bookmark-regexp ()
    (interactive)
    (if (use-region-p)
        (progn
          (bm-bookmark-regexp-region (region-beginning) (region-end))
          (deactivate-mark))
      (bm-bookmark-regexp)))

  (bind-key "C-c h b"
            (defhydra hydra-bm (:color pink
                                       :hint nil
                                       :body-pre (when (not (use-region-p))
                                                   (push-mark)))
"
Bookmark _n_ext (_N_ in lifo order)            toggle book_m_ark        ^^_/_ bm lines matching regexp                          toggle per_s_istence
         _p_revious (_P_ in lifo order)        _a_nnotate               _x_/_X_ remove all bm from current/all buffer(s)        _r_eturn to from where you started
         show _A_nnotation
"
              ("m"   bm-toggle)
              ("M"   bm-toggle :color blue)
              ("a"   bm-bookmark-annotate :color blue)
              ("A"   bm-bookmark-show-annotation)
              ("n"   bm-common-next)
              ("N"   bm-lifo-next)
              ("p"   bm-common-previous)
              ("P"   bm-lifo-previous)
              ("/"   rag/bm-bookmark-regexp :color blue)
              ("s"   bm-toggle-buffer-persistence)
              ("x"   bm-remove-all-current-buffer :color blue)
              ("X"   bm-remove-all-all-buffers :color blue)
              ("r"   pop-to-mark-command :color blue)
              ("RET" nil "cancel" :color blue)
              ("q"   nil "cancel" :color blue))))

;; headlong: allows to easily jump to bookmarks
;; http://oremacs.com/2015/01/06/rushing-headlong/
(use-package headlong
  :bind (("H-b" . headlong-bookmark-jump)))

;; load bookmark list
(bookmark-bmenu-list)

(provide 'setup-bookmark)
