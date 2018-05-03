;; Time-stamp: <2018-05-04 00:32:36 csraghunandan>

;;; Smerge - to resolve merge conflicts
(use-package smerge-mode
  :ensure nil
  :bind (("C-c h r" . hydra-smerge/body))
  :init
  (progn
    (defun modi/enable-smerge-maybe ()
      "Auto-enable `smerge-mode' when merge conflict is detected."
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^<<<<<<< " nil :noerror)
          (smerge-mode 1))))
    (add-hook 'find-file-hook #'modi/enable-smerge-maybe :append))
  :config
  (progn
    (>=e "26.0"
        nil
      ;; http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=bdfee01a6567b9f08f82bc84d1196e6cb62587ca
      (defalias 'smerge-keep-upper 'smerge-keep-mine)
      (defalias 'smerge-keep-lower 'smerge-keep-other)
      (defalias 'smerge-diff-base-upper 'smerge-diff-base-mine)
      (defalias 'smerge-diff-upper-lower 'smerge-diff-mine-other)
      (defalias 'smerge-diff-base-lower 'smerge-diff-base-other))

    (defhydra hydra-smerge (:color pink
                                   :hint nil
                                   :pre (smerge-mode 1)
                                   ;; Disable `smerge-mode' when quitting hydra if
                                   ;; no merge conflicts remain.
                                   :post (smerge-auto-leave))
      "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
      ("n" smerge-next)
      ("p" smerge-prev)
      ("b" smerge-keep-base)
      ("u" smerge-keep-upper)
      ("l" smerge-keep-lower)
      ("a" smerge-keep-all)
      ("RET" smerge-keep-current)
      ("\C-m" smerge-keep-current)
      ("<" smerge-diff-base-upper)
      ("=" smerge-diff-upper-lower)
      (">" smerge-diff-base-lower)
      ("R" smerge-refine)
      ("E" smerge-ediff)
      ("C" smerge-combine-with-next)
      ("r" smerge-resolve)
      ("k" smerge-kill-current)
      ("q" nil "cancel" :color blue))))

(provide 'setup-smerge)
