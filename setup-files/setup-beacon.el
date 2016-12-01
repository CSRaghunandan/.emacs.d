;; Time-stamp: <2016-12-01 12:00:04 csraghunandan>

;; beacon :-  blink the cursor whenever scrolling or switching between windows
;; https://github.com/Malabarba/beacon
(use-package beacon
  :defer 1
  :diminish beacon-mode
  :bind (("C-c b e" . beacon-blink))
  :config
  (beacon-mode 1)
  (setq beacon-size 23)
  (add-to-list 'beacon-dont-blink-major-modes 'shell-mode))

(provide 'setup-beacon)
