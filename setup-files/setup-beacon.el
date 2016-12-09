;; Time-stamp: <2016-12-09 12:17:00 csraghunandan>

;; beacon :-  blink the cursor whenever scrolling or switching between windows
;; https://github.com/Malabarba/beacon
(use-package beacon
  :defer 1
  :diminish beacon-mode
  :bind (("C-!" . beacon-blink))
  :config
  (beacon-mode 1)
  (setq beacon-size 25)
  ;; don't blink in shell-mode
  (add-to-list 'beacon-dont-blink-major-modes 'comint-mode))

(provide 'setup-beacon)
