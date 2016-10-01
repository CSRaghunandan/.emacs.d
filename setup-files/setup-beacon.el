(use-package beacon
  :defer 1
  :diminish beacon-mode
  :bind* (("C-c b e" . beacon-blink))
  :config
  (beacon-mode 1)
  (setq beacon-size 23))

(provide 'setup-beacon)
