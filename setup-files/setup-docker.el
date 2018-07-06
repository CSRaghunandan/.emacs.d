;; Time-stamp: <2018-07-06 11:51:56 csraghunandan>

;; Copyright (C) 2016-2018 Chakravarthy Raghunandan
;; Author: Chakravarthy Raghunandan rnraghunandan@gmail.com

;; dockerfile-mode: An emacs mode for handling Dockerfiles
;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; docker: manager docker from emacs
;; https://github.com/Silex/docker.el
(use-package docker
  :defer t)

;; docker-compose-mode: Major mode for editing docker-compose files
;; https://github.com/meqif/docker-compose-mode
(use-package docker-compose-mode
  :defer t)

;; docker-tramp: TRAMP integration for docker containers
;; https://github.com/emacs-pe/docker-tramp.el
(use-package docker-tramp
  :defer t)

(provide 'setup-docker)
