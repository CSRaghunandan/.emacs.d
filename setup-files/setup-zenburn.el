;; Time-stamp: <2017-09-07 18:25:30 csraghunandan>

;; zenburn: A pleasing dark theme for emacs
;; https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme)

(with-eval-after-load "zenburn-theme"
  (custom-theme-set-faces
   'zenburn
   ;; make zenburn background theme darker
   '(default ((t (:foreground "#DCDCCC" :background "#383838"))))

   ;; mode-line related config
   '(mode-line ((t (:box (:line-width 2 :color "gray30")
                         :weight normal :foreground "#BFEBBF"
                         :background "gray30"))))
   '(mode-line-inactive ((t (:box (:line-width 2 :color "gray30")
                                  :weight normal :foreground "gray70"
                                  :background "gray20"))))
   '(powerline-active1 ((t (:background "gray30"))))
   '(powerline-active2 ((t (:background "gray30"))))
   '(mode-line-buffer-id ((t (:foreground "#FFECBA" :weight bold))))
   '(powerline-inactive1 ((t (:background "gray20"))))
   '(powerline-inactive2 ((t (:background "gray20"))))

   ;; ivy minibuffer config
   '(ivy-minibuffer-match-face-4 ((t (:background "pink4"))))
   '(ivy-minibuffer-match-face-3 ((t (:background "CadetBlue4"))))
   '(ivy-minibuffer-match-face-2 ((t (:background "DarkSeaGreen4"))))
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
   '(font-lock-comment-face ((t (:foreground "gray55" :slant italic))))
   '(font-lock-doc-face ((t (:foreground "gray70" :slant italic))))
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
                                   :inherit nil))))))

(provide 'setup-zenburn)
