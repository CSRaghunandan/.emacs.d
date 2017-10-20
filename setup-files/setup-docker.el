;; Time-stamp: <2017-10-21 02:55:02 csraghunandan>

;; dockerfile-mode: An emacs mode for handling Dockerfiles
;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :defer t
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; docker: manager docker from emacs
;; https://github.com/Silex/docker.el
(use-package docker)

;; docker-compose-mode: Major mode for editing docker-compose files
;; https://github.com/meqif/docker-compose-mode
(use-package docker-compose-mode
  :defer t)

;; docker-tramp: TRAMP integration for docker containers
;; https://github.com/emacs-pe/docker-tramp.el
(use-package docker-tramp)

(provide 'setup-docker)
