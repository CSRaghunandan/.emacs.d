;; Time-stamp: <2016-12-16 12:04:59 csraghunandan>

;; to get pragmatapro ligatures to work in emacs.
;; This is just a workaround as emacs still has no official support for ligatures.

(defun setup-pragmata-ligatures ()
  (setq prettify-symbols-alist
	(append prettify-symbols-alist
		'(("!!"   . ?)
		  ("!="   . ?)
		  ("!=="  . ?)
		  ("!≡"   . ?)
		  ("!≡≡"  . ?)
		  ("!>"   . ?)
		  ("#("   . ?)
		  ("#_"   . ?)
		  ("#{"   . ?)
		  ("#?"   . ?)
		  ("#>"   . ?)
		  ("%="   . ?)
		  ("%>"   . ?)
		  ("<~"   . ?)
		  ("&%"   . ?)
		  ("&&"   . ?)
		  ("&*"   . ?)
		  ("&+"   . ?)
		  ("&-"   . ?)
		  ("&/"   . ?)
		  ("&="   . ?)
		  ("&&&"  . ?)
		  ("&>"   . ?)
		  ("$>"   . ?)
		  ("~>"   . ?)
		  ("*="   . ?)
		  ("*/"   . ?)
		  ("*>"   . ?)
		  ("++"   . ?)
		  ("+++"  . ?)
		  ("+="   . ?)
		  ("+>"   . ?)
		  ("--"   . ?)
		  ("-<"   . ?)
		  ("-<<"  . ?)
		  ("-="   . ?)
		  ("->>"  . ?)
		  ("---"  . ?)
		  (".."   . ?)
		  ("..."  . ?)
		  ("..<"  . ?)
		  (".>"   . ?)
		  (".~"   . ?)
		  (".="   . ?)
		  ("/*"   . ?)
		  ("//"   . ?)
		  ("/>"   . ?)
		  ("/="   . ?)
		  ("/=="  . ?)
		  ("///"  . ?)
		  ("/**"  . ?)
		  ("::"   . ?)
		  (":="   . ?)
		  (":≡"   . ?)
		  (":>"   . ?)
		  (":=>"  . #xE7D4)
		  ("<$>"  . ?)
		  ("<*"   . ?)
		  ("<*>"  . ?)
		  ("<+>"  . ?)
		  ("<-"   . ?) ; I like different arrows (see below)
		  ("<<"   . ?)
		  ("<<<"  . ?)
		  ("<<="  . ?)
		  ("<="   . ?)
		  ("<>"   . ?)
		  ("<|>"  . ?)
		  ("<<-"  . ?)
		  ("<|"   . ?)
		  ("<=<"  . ?)
		  ("<~~"  . ?)
		  ("<<~"  . ?)
		  ("<$"   . ?)
		  ("<+"   . ?)
		  ("<!>"  . ?)
		  ("<@>"  . ?)
		  ("<#>"  . ?)
		  ("<%>"  . ?)
		  ("<^>"  . ?)
		  ("<&>"  . ?)
		  ("<?>"  . ?)
		  ("<.>"  . ?)
		  ("</>"  . ?)
		  ("<\>"  . ?)
		  ("<\">" . ?)
		  ("<:>"  . ?)
		  ("<~>"  . ?)
		  ("<**>" . ?)
		  ("<<^"  . ?)
		  ("<!"   . ?)
		  ("<@"   . ?)
		  ("<#"   . ?)
		  ("<%"   . ?)
		  ("<^"   . ?)
		  ("<&"   . ?)
		  ("<?"   . ?)
		  ("<."   . ?)
		  ("</"   . ?)
		  ("<\\"  . ?)
		  ("<\""  . ?)
		  ("<:"   . ?)
		  ("=<<"  . ?)
		  ("=="   . ?)
		  ("==="  . ?)
		  ("==>"  . ?) ; I like different arrows (see below)
		  ("=>"   . ?)  ; I like different arrows (see below)
		  ("=~"   . ?)
		  ("=>>"  . ?)
		  ("≡≡"   . ?)
		  ("≡≡≡"  . ?)
		  ("≡:≡"  . ?)
		  (">-"   . ?)
		  (">="   . ?)
		  (">>"   . ?)
		  (">>-"  . ?)
		  (">>="  . ?)
		  (">>>"  . ?)
		  (">=>"  . ?)
		  (">>^"  . ?)
		  ("??"   . ?)
		  ("?~"   . ?)
		  ("?="   . ?)
		  ("?>"   . ?)
		  ("^="   . ?)
		  ("^."   . ?)
		  ("^?"   . ?)
		  ("^.."  . ?)
		  ("^<<"  . ?)
		  ("^>>"  . ?)
		  ("^>"   . ?)
		  ("\\\\" . ?)
		  ("\\>"  . ?)
		  ("@>"   . ?)
		  ("|="   . ?)
		  ("||"   . ?)
		  ("|>"   . ?)
		  ("|||"  . ?)
		  ("|+|"  . ?)
		  ("~="   . ?)
		  ("~~>"  . ?)
		  ("~>>"  . ?)
		  ("->"   . #XE7A4)
		  ("<=>"  . #XE7E9)
		  ))))

(defun refresh-pretty ()
  (prettify-symbols-mode -1)
  (prettify-symbols-mode +1))

;; Hooks for modes in which to install the Pragmata ligatures
(mapc (lambda (hook)
	(add-hook hook (lambda () (setup-pragmata-ligatures) (refresh-pretty))))
      '(text-mode-hook
        prog-mode-hook
        org-mode-hook))
;; enable prettify symbols mode globally
(global-prettify-symbols-mode +1)

(provide 'setup-pragmatapro)
