;; Time-stamp: <2016-12-31 10:25:36 csraghunandan>

;; Theme configuration for emacs
;; https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme
  :config

  ;; make zenburn theme look darker by default
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     `(default ((t (:foreground ,zenburn-fg :background ,zenburn-bg-05))))))

  ;; make function face brighter so it's easily distinguishable from font-lock face for types
  (set-face-attribute 'font-lock-function-name-face nil :foreground "CadetBlue1")
  ;; fontify links to make them standout
  (set-face-attribute 'link nil :foreground "#C9B8A2" :underline nil :weight 'normal)
  (set-face-attribute 'link-visited nil :foreground "#C9AE8C" :underline nil :weight 'normal)
  ;; make everything look gray :)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "gray55")
  (set-face-attribute 'font-lock-comment-face nil :foreground "gray55" :slant 'italic)
  (set-face-attribute 'font-lock-doc-face nil :foreground "gray70" :slant 'italic)
  (set-face-background 'shm-current-face "#444444")
  (set-face-attribute 'hl-line nil :background "#444444")
  (set-face-attribute 'fringe nil :background "#444444")
  (set-face-attribute 'vhl/default-face nil :background "#444444")

  (eval-after-load 'web-mode
    (lambda ()
      (set-face-attribute 'web-mode-current-column-highlight-face nil :background "#444444")))

  ;; make mode line look pretty :)
  (set-face-attribute 'mode-line nil :box '(:line-width 2 :color "gray30")
                      :weight 'normal :foreground "#C8F7C8" :background "gray20")
  (set-face-attribute 'mode-line-inactive nil :box '(:line-width 2 :color "gray30")
                      :weight 'normal :foreground "gray70")
  (set-face-attribute 'powerline-active2 nil :background "gray32")
  (set-face-attribute 'powerline-active1 nil :background "gray32" :weight 'normal)
  (set-face-attribute 'mode-line-buffer-id nil :foreground "#FFECBA" :weight 'bold)
  ;; dim inactive modeline
  (set-face-attribute 'powerline-inactive2 nil :background "gray20")
  (set-face-attribute 'powerline-inactive1 nil :background "gray32")

  ;; contrasting colors for ivy minibuffer match results
  (set-face-attribute 'ivy-minibuffer-match-face-4 nil :background "pink4")
  (set-face-attribute 'ivy-minibuffer-match-face-3 nil :background "CadetBlue4")
  (set-face-attribute 'ivy-minibuffer-match-face-2 nil :background "DarkSeaGreen4"))

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

  ;; strike-through unmatched parenthesis
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground "red"
                      :inherit 'unspecified
                      :strike-through t))

;; Better defaults for emacs
(column-number-mode t)
(line-number-mode t)
;; disable the ugly toolbar
(tool-bar-mode -1)
;; ignore bell rings
(setq ring-bell-function 'ignore)
;; resize minibuffer window to accommodate text
(setq resize-mini-window t)
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
    ";; The computing scientist’s main challenge is not to get confused by the complexities of his own making. -- E. W. Dijkstra\n\n"
    ";; There are two ways of constructing a software design: One way is to make it so simple that there are obviously no deficiencies and the other way is to make it so complicated that there are no obvious deficiencies. -- C. A. R. Hoare\n\n"
    ";; Controlling complexity is the essence of computer programming. -- Brian Kernigan\n\n"
    ";; If you’re willing to restrict the flexibility of your approach, you can almost always do something better. -- John Carmack\n\n"
    ";; Measuring programming progress by lines of code is like measuring aircraft building progress by weight. -- Bill Gates\n\n"
    ";; The best code is no code at all.\n\n"
    ";; There is not now, nor has there ever been, nor will there ever be, any programming language in which it is the least bit difficult to write bad code.\n\n"
    ";; Code never lies, comments sometimes do. -- Ron Jeffries\n\n"
    ";; Simplicity carried to the extreme becomes elegance. -- Jon Franklin\n\n"
    ";; The unavoidable price of reliability is simplicity. -- C. A. R. Hoare\n\n"
    ";; Good code is short, simple, and symmetrical – the challenge is figuring out how to get there. -- Sean Parent\n\n"
    ";; True glory consists in doing what deserves to be written in writing what deserves to be read. -- Pliny the Elder\n\n"
    ";; The whole point of getting things done is knowing what to leave undone. -- Oswald Chambers\n\n"))

;; populate the initial scratch buffer with a random quote.
(setq initial-scratch-message (random-choice programming-quotes))
;; set scratch major mode to `emacs-lisp-mode'
(setq initial-major-mode 'emacs-lisp-mode)
;; Show a random programming quote in scratch buffer
(random-choice programming-quotes)

;; cursor settings
(setq-default cursor-type '(bar . 1))
;; disable annoying cursor blinks
(blink-cursor-mode -1)

(fringe-mode '(8 . 8))
;; Do not make mouse wheel accelerate its action (example: scrolling)
(setq mouse-wheel-progressive-speed nil)

;; prevent cursor from moving when scrolling
(setq scroll-preserve-screen-position t
      scroll-conservatively 10000)

;; set continuation indicators to right fringe only
(setf (cdr (assq 'continuation fringe-indicator-alist))
      ;; '(nil nil) ;; no continuation indicators
      '(nil right-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )

;; make sure emacsclient starts at fullscreen
(setq default-frame-alist
      `((fullscreen . maximized)))

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

(provide 'setup-theme)
