;; Time-stamp: <2016-10-06 23:02:20 csraghunandan>

;; Smart M-x (smex)
;; https://github.com/nonsequitur/smex/

(use-package smex
  :config (smex-initialize))

(provide 'setup-smex)

;; Useful bindings while smex is active
;;
;; |---------+------------------------------------------------------------|
;; | Binding | Description                                                |
;; |---------+------------------------------------------------------------|
;; | C-h f   | Runs `describe-function' on the currently selected command |
;; | M-.     | Jump to the definition of the selected command             |
;; | C-h w   | Show the key bindings for the selected command             |
;; |---------+------------------------------------------------------------|
