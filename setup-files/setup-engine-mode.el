;; Time-stamp: <2018-06-18 23:03:40 csraghunandan>

;; Minor mode for defining and querying search engines through Emacs.
;; https://github.com/hrs/engine-mode/
(use-package engine-mode
  :init (setq engine/keybinding-prefix "C-x /")
  :config
  (engine-mode t)

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")

  (defengine wikipedia
    "https://en.wikipedia.org/wiki/%s"
    :keybinding "p") ;wiki(p)edia

  (defengine word
    "http://wordnik.com/words/%s"
    :term-transformation-hook downcase
    :keybinding "w")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s"
    :keybinding "a")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y")

  (defengine wolfram-alpha
    "http://www.wolframalpha.com/input/?i=%s"
    :keybinding "o"))

(provide 'setup-engine-mode)
