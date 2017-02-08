;; Time-stamp: <2017-02-08 11:07:31 csraghunandan>

;; https://github.com/Fanael/rainbow-delimiters
;; different colours for each nested delimiter
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; better defaults
;; don't show splash screen when starting emacs
(setq inhibit-splash-screen t)
;; don't echo startup message of GNU emacs
(setq inhibit-startup-echo-area-message t)

(defun random-choice (list)
  "Return a random element from LIST."
  (let ((random-index (random (length list))))
    (nth random-index list)))

(defvar programming-quotes
  '(";; First, solve the problem. Then, write the code. -- John Johnson\n\n"
    ";; The computing scientist’s main challenge is not to get confused by the
;; complexities of his own making. -- E. W. Dijkstra\n\n"
    ";; There are two ways of constructing a software design: One way is to make
;; it so simple that there are obviously no deficiencies and the other way is to
;; make it so complicated that there are no obvious deficiencies. -- C. A. R. Hoare\n\n"
    ";; Controlling complexity is the essence of computer programming. -- Brian Kernigan\n\n"
    ";; if you’re willing to restrict the flexibility of your approach, you can
;; almost always do something better. -- John Carmack\n\n"
    ";; Measuring programming progress by lines of code is like measuring
;; aircraft building progress by weight. -- Bill Gates\n\n"
    ";; The best code is no code at all.\n\n"
    ";; There is not now, nor has there ever been, nor will there ever be, any
;; programming language in which it is the least bit difficult to write bad code.\n\n"
    ";; Code never lies, comments sometimes do. -- Ron Jeffries\n\n"
    ";; Simplicity carried to the extreme becomes elegance. -- Jon Franklin\n\n"
    ";; The unavoidable price of reliability is simplicity. -- C. A. R. Hoare\n\n"
    ";; Good code is short, simple, and symmetrical – the challenge is figuring
;; out how to get there. -- Sean Parent\n\n"
    ";; True glory consists in doing what deserves to be written in writing what
;; deserves to be read. -- Pliny the Elder\n\n"
    ";; The whole point of getting things done is knowing what to leave undone. -- Oswald Chambers\n\n"))

;; populate the initial scratch buffer with a random quote.
(setq initial-scratch-message (random-choice programming-quotes))
;; set scratch major mode to `emacs-lisp-mode'
(setq initial-major-mode 'emacs-lisp-mode)
;; Show a random programming quote in scratch buffer
(random-choice programming-quotes)



;;; all frame related settings
;; cursor settings
(setq-default cursor-type '(bar . 1))
;; disable annoying cursor blinks
(blink-cursor-mode -1)
;; disable the ugly toolbar
(tool-bar-mode -1)
;; silence all bell rings
(setq ring-bell-function 'ignore)
;; Do not make mouse wheel accelerate its action (example: scrolling)
(setq mouse-wheel-progressive-speed nil)

;; set continuation indicators to right fringe only
(setf (cdr (assq 'continuation fringe-indicator-alist))
      ;; '(nil nil) ;; no continuation indicators
      '(nil right-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )

;; make sure emacsclient starts at fullscreen
(setq default-frame-alist `((fullscreen . maximized)))

;; set PragmataPro font only if it available
(defun rag-set-face (frame)
  "Configure faces on frame creation"
  (select-frame frame)
  (if (display-graphic-p)
      (progn
        (when (member "PragmataPro" (font-family-list))
            (set-frame-font "PragmataPro-13")))))
(add-hook 'after-make-frame-functions 'rag-set-face)

;; set frame font when running emacs normally
(when (member "PragmataPro" (font-family-list))
  (set-frame-font "PragmataPro-13"))

;; show full path of the open file in title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; remove ugly scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))



;; Show actual lines instead of the page break char ^L
;; https://github.com/purcell/page-break-lines
;; enter page-break character in Emacs by entering `C-q C-l'
(use-package page-break-lines
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode))

;; all-the-icons
(use-package all-the-icons)

(provide 'setup-visual)
