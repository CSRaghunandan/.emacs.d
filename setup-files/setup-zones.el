;; Time-stamp: <2016-10-16 13:14:10 csraghunandan>

;; zones - to browse among multiple narrows of a buffer
;; https://www.emacswiki.org/emacs/Zones

(use-package zones)

(provide 'setup-zones)

;; ‘C-x n a’ (‘zz-add-zone’) – Add the active region to the current list of zones.
;; ‘C-x n A’ (‘zz-add-zone-and-unite’) – Add the region to the current list of zones, and then unite (coalesce) them.
;; ‘C-x n c’ (‘zz-clone-zones’) – Clone a zones variable to another one, so the clone has the same zones.
;; ‘C-x n C’ (‘zz-clone-and-unite-zones’) – Clone to a variable and then unite its zones.
;; ‘C-x n d’ (‘narrow-to-defun’) – Standard, except that it is advised to update the current list of zones with the narrowing.
;; ‘C-x n C-d’ (‘zz-izone-delete’) – Delete a zone from the current list of zones.
;; ‘C-x n n’ (‘narrow-to-region’) – Standard, except that it is advised to update the current list of zones with the narrowing.
;; ‘C-x n p’ (‘narrow-to-page’) (standard, except that it is advised to update the current list of zones with the narrowing)
;; ‘C-x n r’ (‘zz-select-region-repeat’) – Cycle zones as the active region: `C-x n r r r ...’
;; ‘C-x n u’ (‘zz-unite-zones’) – Unite (coalesce) the zones in the current list.
;; ‘C-x n v’ (‘zz-set-izones-var’) – Make another zones variable current.
;; ‘C-x n w’ (‘widen’) – Standard.
;; ‘C-x n x’ (‘zz-narrow-repeat’) – Cycle zones as buffer narrowings: `C-x n x x x ...’
