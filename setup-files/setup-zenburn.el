;; Time-stamp: <2018-04-30 17:55:42 csraghunandan>

;; zenburn: A pleasing dark theme for emacs
;; https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme)

(with-eval-after-load "zenburn-theme"
  (custom-theme-set-faces
   'zenburn
   ;; make zenburn background theme darker
   '(default ((t (:foreground "#DCDCCC" :background "#383838"))))

   ;; mode-line configuration
   (let ((line (face-attribute 'mode-line :underline)))
     (zenburn-with-color-variables
       (set-face-attribute 'mode-line          nil :overline   line)
       (set-face-attribute 'mode-line-inactive nil :overline   line)
       (set-face-attribute 'mode-line-inactive nil :underline  line)
       (set-face-attribute 'mode-line          nil :box        nil)
       (set-face-attribute 'mode-line-inactive nil :box        nil)
       (set-face-attribute 'mode-line          nil :background "gray30")
       (set-face-attribute 'mode-line-inactive nil :background zenburn-bg-1)
       (set-face-attribute 'mode-line          nil :foreground zenburn-green+4)
       (set-face-attribute 'mode-line-inactive nil :foreground "gray70")))

   ;; ivy minibuffer config
   '(ivy-minibuffer-match-face-4 ((t (:background "pink4" :underline t))))
   '(ivy-minibuffer-match-face-3 ((t (:background "SteelBlue3" :underline t))))
   '(ivy-minibuffer-match-face-2 ((t (:background "DarkSeaGreen4" :underline t))))
   '(ivy-current-match ((t (:foreground "#F0DFAF" :underline nil :weight bold))))
   '(golden-ratio-scroll-highlight-line-face ((t (:background "gray27"
                                                              :weight normal))))
   ;; magit faces
   '(magit-popup-disabled-argument ((t (:foreground "gray55"))))
   '(magit-popup-key ((t (:foreground "#BFEBBF"))))
   '(magit-section-highlight ((t (:background "gray27"))))
   '(magit-diff-file-heading-highlight ((t (:background "gray27"))))
   '(magit-diff-hunk-heading-highlight ((t (:background "gray27"))))
   '(magit-diff-context-highlight ((t (:background "gray27"))))

   ;; make function face brighter so it's easily distinguishable
   '(font-lock-function-name-face ((t (:foreground "CadetBlue1"))))

   ;; fontify links to make them standout
   '(link ((t (:foreground "#C9B8A2"
                           :underline nil :weight normal))))
   '(link-visited ((t (:foreground "C9AE8C"
                                   :underline nil :weight normal))))

   ;; make everything look gray
   '(font-lock-comment-delimiter-face ((t (:foreground "gray55"))))
   '(font-lock-comment-face ((t (:foreground "gray55"))))
   '(font-lock-doc-face ((t (:foreground "gray70"))))
   '(shm-current-face ((t (:background "gray27"))))
   '(hl-line ((t (:background "gray27"))))
   '(fringe ((t (:background "gray27"))))
   '(vhl/default-face ((t (:background "gray27"))))
   '(vertical-border ((t (:foreground "gray20"))))

   ;; strike through unmatched parenthesis
   '(rainbow-delimiters-unmatched-face ((t (:foreground "red" :inherit unspecified
                                                        :strike-through t))))

   ;; org-mode face
   '(org-checkbox ((t (:foreground "gray70" :background nil
                                   :weight bold :box nil))))
   '(org-priority ((t (:foreground "gray70" :weight bold
                                   :inherit nil))))
   '(org-date ((((class color)) (:underline nil))))

   ;; term face config
   '(term ((t (:foreground "#E5D9BD"))))
   '(term-color-green ((t (:background "grey30" :foreground "#9F8300"))))

   ;; markdown header face config
   '(markdown-header-face-1 ((t (:foreground "#DFAF8F" :weight bold :height 1.8))))
   '(markdown-header-face-2 ((t (:foreground "#BFEBBF" :weight bold :height 1.6))))
   '(markdown-header-face-3 ((t (:foreground "#7CB8BB" :weight bold :height 1.4))))
   '(markdown-header-face-4 ((t (:foreground "#D0BF8F" :weight bold :height 1.2))))
   '(markdown-header-face-5 ((t (:foreground "#93E0E3" :weight bold :height 1.1))))
   '(markdown-header-face-6 ((t (:foreground "#9FC59F" :weight bold))))))

(provide 'setup-zenburn)
