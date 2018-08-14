;;; setup-tldr.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-15 03:09:19 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; TLDR
;; https://github.com/tldr-pages/tldr
;; https://github.com/kuanyui/tldr.el
(use-package tldr
  :bind (("C-x / t" . tldr)))

;; Related -- curl cheat.sh
;; https://github.com/chubin/cheat.sh
;; https://www.reddit.com/r/emacs/comments/6ddr7p/snippet_search_cheatsh_using_ivy/
(defvar ejmr/counsel-cheat-sh-history nil
  "History for `ejmr/counsel-cheat-sh'.")
(defun ejmr/counsel-cheat-sh ()
  "Search `http://cheat.sh/' for help on commands and code."
  (interactive)
  (let ((url "http://cheat.sh/")
        ;; T - omit terminal sequences (no colors)
        ;;     Without that, we get this error:
        ;;       Too deeply nested to render properly; consider increasing
        ;;       `max-specpdl-size'.
        ;; q - quiet mode, don't show github/twitter buttons
        (options "?T&q"))
    (ivy-read "Search cheat.sh: "
              (process-lines "curl" "--silent" (concat url ":list" options))
              :require-match t
              :sort t
              :history 'ejmr/counsel-cheat-sh-history
              :action (lambda (input)
                        (eww-browse-url (concat url input options)))
              :caller 'ejmr/counsel-cheat-sh)))
(defalias 'cheat.sh 'ejmr/counsel-cheat-sh)

(defun modi/eww-rename-cheat-sh-buffer (&rest _)
  "Rename the `eww' buffer if it is showing a `cheat.sh' page."
  (let ((url (eww-copy-page-url)))
    (when (string-match "^http://cheat.sh/\\([^/?]+\\)" url)
      (rename-buffer (concat "*cheat.sh " (match-string 1 url) "*") :unique))))
(advice-add 'eww :after #'modi/eww-rename-cheat-sh-buffer)

(bind-key "C-x / c" #'cheat.sh)

(provide 'setup-tldr)
