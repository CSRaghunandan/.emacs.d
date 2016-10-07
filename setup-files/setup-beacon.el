;; Time-stamp: <2016-10-07 12:16:39 csraghunandan>

;; beacon :-  blink the cursor whenever scrolling or switching between windows
;; https://github.com/Malabarba/beacon
(use-package beacon
  :defer 1
  :diminish beacon-mode
  :bind (("C-c b e" . beacon-blink))
  :config
  (beacon-mode 1)
  (setq beacon-size 23))

(provide 'setup-beacon)
