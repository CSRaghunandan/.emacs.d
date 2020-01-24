;;; setup-kruecolor.el -*- lexical-binding: t; -*-
;; Time-stamp: <2018-08-15 03:03:01 csraghunandan>

;; Copyright (C) 2016-2020 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan <rnraghunandan@gmail.com>

;; Kurecolor: color editing goodies for Emacs
;; https://github.com/emacsfodder/kurecolor
(use-package kurecolor
  :bind (("C-c k" . hydra-kurecolor/body))
  :config
  (defhydra hydra-kurecolor (:color pink
                                    :hint  nil)
"
Inc/Dec      _j_/_J_ brightness      _k_/_K_ saturation      _l_/_L_ hue
Set          _sj_ ^^ brightness      _sk_ ^^ saturation      _sl_ ^^ hue
Get          _gj_ ^^ brightness      _gk_ ^^ saturation      _gl_ ^^ hue

             _rh_ ^^ RGB → Hex       _hr_ ^^ Hex → RGB       _hR_ ^^ Hex → RGBA
"
    ("j"  kurecolor-decrease-brightness-by-step)
    ("J"  kurecolor-increase-brightness-by-step)
    ("k"  kurecolor-decrease-saturation-by-step)
    ("K"  kurecolor-increase-saturation-by-step)
    ("l"  kurecolor-decrease-hue-by-step)
    ("L"  kurecolor-increase-hue-by-step)
    ("sj" kurecolor-set-brightness :color blue)
    ("sk" kurecolor-set-saturation :color blue)
    ("sl" kurecolor-set-hue :color blue)
    ("gj" kurecolor-hex-val-group :color blue)
    ("gk" kurecolor-hex-sat-group :color blue)
    ("gl" kurecolor-hex-hue-group :color blue)
    ("rh" kurecolor-cssrgb-at-point-or-region-to-hex :color blue)
    ("hr" kurecolor-hexcolor-at-point-or-region-to-css-rgb :color blue)
    ("hR" kurecolor-hexcolor-at-point-or-region-to-css-rgba :color blue)
    ("q"  nil "cancel" :color blue)))

(provide 'setup-kurecolor)
