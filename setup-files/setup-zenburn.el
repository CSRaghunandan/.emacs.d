;; Time-stamp: <2018-06-17 18:08:47 csraghunandan>

;; zenburn: A pleasing dark theme for emacs
;; https://github.com/bbatsov/zenburn-emacs
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t)

  ;; Moody mode-line configuration
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

  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     ;; make zenburn background theme darker
     '(default ((t (:foreground "#DCDCCC" :background "#383838"))))

     ;; ivy minibuffer config
     '(ivy-minibuffer-match-face-4 ((t (:background "pink4" :underline nil))))
     '(ivy-minibuffer-match-face-3 ((t (:background "SteelBlue3" :underline nil))))
     '(ivy-minibuffer-match-face-2 ((t (:background "DarkSeaGreen4" :underline nil))))
     '(ivy-current-match ((t (:foreground "#F0DFAF" :underline nil :weight bold))))
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

     ;; org-mode face
     '(org-checkbox ((t (:foreground "gray70" :background nil
                                     :weight bold :box nil))))
     '(org-date ((((class color)) (:underline nil)))))))

(provide 'setup-zenburn)
