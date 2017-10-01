;; Time-stamp: <2017-10-01 10:04:47 csraghunandan>

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
    ";; The whole point of getting things done is knowing what to leave undone. -- Oswald Chambers\n\n"
    ";; Do not communicate by sharing memory; instead, share memory by communicating."))

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

(setq frame-resize-pixelwise t) ; Allow frame size to inc/dec by a pixel
(setq tooltip-mode nil) ; Disable tooltip appearance on mouse hover

(setq frame-title-format
      `("emacs "
        ;; If `emacs-git-branch' is non-nil, show that
        (emacs-git-branch ,(concat "[" emacs-git-branch "]")
                          ;; Else show the version number
                          ,(concat (number-to-string emacs-major-version)
                                   "."
                                   (number-to-string emacs-minor-version)))
        "   "
        (buffer-file-name "%f" ;Show full file path if buffer is showing a file
                          (dired-directory dired-directory ;Else if in dired mode, show the directory name
                                           "%b")) ;Else show the buffer name (*scratch*, *Messages*, etc)
        " %*"))

;; display date and time
(setq display-time-format "%a-%d %H:%M")
(setq display-time-default-load-average nil)
(display-time-mode)

;; set continuation indicators to right fringe only
(setf (cdr (assq 'continuation fringe-indicator-alist))
      ;; '(nil nil) ;; no continuation indicators
      '(nil right-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )

;; make sure emacsclient starts at fullscreen
(setq default-frame-alist `((fullscreen . maximized)))

;; remove ugly scrollbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; resize windows proportionally
(setq window-combination-resize t)



;; Show actual lines instead of the page break char ^L
;; https://github.com/purcell/page-break-lines
;; enter page-break character in Emacs by entering `C-q C-l'
(use-package page-break-lines
  :diminish page-break-lines-mode
  :config (add-hook 'prog-mode-hook #'page-break-lines-mode))

;; all-the-icons: show icons in neotree/dired/modeline
;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons)

;; column-enforce-mode: highlight characters which exceed fill-column
;; https://github.com/jordonbiondo/column-enforce-mode
(use-package column-enforce-mode
  :diminish column-enforce-mode
  :config
  (add-hook 'prog-mode-hook (lambda ()
                              (unless (eq major-mode 'web-mode)
                                (column-enforce-mode))))
  ;; enforce a column of 80 for highlighting
  (setq column-enforce-column 80)
  (set-face-attribute 'column-enforce-face nil
                      :underline nil :foreground "firebrick3")
  (setq column-enforce-comments nil))

;; indicate buffer boundaries in the fringe
(setq-default indicate-buffer-boundaries '((top . right)
                                           (bottom . right)))

;;;; Global Font Resize
(defun modi/global-font-size-adj (scale &optional absolute)
  "Adjust the font sizes globally: in all the buffers, mode line, echo area, etc.
The inbuilt `text-scale-adjust' function (bound to C-x C-0/-/= by default)
does an excellent job of font resizing. But it does not change the font sizes
of text outside the current buffer; for example, in the mode line.
M-<SCALE> COMMAND increases font size by SCALE points if SCALE is +ve,
                  decreases font size by SCALE points if SCALE is -ve
                  resets    font size if SCALE is 0.
If ABSOLUTE is non-nil, text scale is applied relative to the default font size
`default-font-size-pt'. Else, the text scale is applied relative to the current
font size."
  (interactive "p")
  (if (= scale 0)
      (setq font-size-pt 13)
    (if (bound-and-true-p absolute)
        (setq font-size-pt (+ default-font-size-pt scale))
      (setq font-size-pt (+ font-size-pt scale))))
  ;; The internal font size value is 10x the font size in points unit.
  ;; So a 10pt font size is equal to 100 in internal font size value.
  (set-face-attribute 'default nil :height (* font-size-pt 10)))

(defun modi/global-font-size-incr ()  (interactive) (modi/global-font-size-adj +1))
(defun modi/global-font-size-decr ()  (interactive) (modi/global-font-size-adj -1))
(defun modi/global-font-size-reset () (interactive) (modi/global-font-size-adj 0))

;; Usage: C-c C-- = - 0 = = = = - - 0
;; Usage: C-c C-= = 0 - = - = = = = - - 0
(defhydra hydra-font-resize (nil
                             "C-c"
                             :bind (lambda (key cmd) (bind-key key cmd))
                             :color red
                             :hint nil)
  "
Font Size:     _C--_/_-_ Decrease     _C-=_/_=_ Increase     _C-0_/_0_ Reset     _q_ Cancel
"
  ;; Hydra entry bindings
  ("C--" modi/global-font-size-decr)
  ("C-=" modi/global-font-size-incr)
  ("C-0" modi/global-font-size-reset :color blue)
  ;; Hydra-internal bindings.. below work only when the hydra is active!
  ("-"   modi/global-font-size-decr :bind nil)
  ("="   modi/global-font-size-incr :bind nil)
  ("+"   modi/global-font-size-incr :bind nil)
  ("0"   modi/global-font-size-reset :bind nil)
  ("q"   nil :color blue))

(bind-key "C-c h f" 'hydra-font-resize/body)

(provide 'setup-visual)
