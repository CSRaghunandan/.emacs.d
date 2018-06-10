;; Time-stamp: <2018-06-11 00:08:33 csraghunandan>

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
