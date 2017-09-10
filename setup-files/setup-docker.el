;; Time-stamp: <2017-09-10 18:21:20 csraghunandan>

;; dockerfile-mode: An emacs mode for handling Dockerfiles
;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; docker: manager docker from emacs
;; https://github.com/Silex/docker.el
(use-package docker)

;; docker-compose-mode: Major mode for editing docker-compose files
;; https://github.com/meqif/docker-compose-mode
(use-package docker-compose-mode)

(provide 'setup-docker)
