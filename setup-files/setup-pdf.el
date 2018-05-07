;; Time-stamp: <2018-05-07 17:24:58 csraghunandan>

;; pdf-tools: Emacs support library for PDF files.
;; https://github.com/politza/pdf-tools
(use-package pdf-tools
  :config
  ;; https://github.com/politza/pdf-tools/issues/312#issuecomment-329537742
  ;; Build the program (if necessary) without asking first, if NO-QUERY-P is
  ;; non-nil.
  (pdf-tools-install))

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
