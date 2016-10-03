
(use-package google-this
  :diminish google-this-mode
  :config (google-this-mode 1)
  (defhydra hydra-google (:color blue
                                 :hint nil)
    "
 _w_: word   _r_: region    _v_: symbol   _l_: line
 _g_: google _c_: cpp       _s_: string   _q_: quit
 "
    ("w" google-this-word)
    ("r" google-this-region)
    ("v" google-this-symbol)
    ("s" google-this-clean-error-string)
    ("l" google-this-line)
    ("g" google-this-search)
    ("c" google-this-cpp-reference)
    ("q" nil :color blue))
  (bind-key "C-x g" 'hydra-google/body))

(provide 'setup-google-this)
