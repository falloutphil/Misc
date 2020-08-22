;;; covid --- summary info.
;;; Commentary:
;;; Calculates 14 and 7 day cumulative new cases from WHO data.

;;; Code:
(require 'org)
(require 'json)

(defun covid (country population)
   "Helper function to get covid details from COUNTRY.  If POPULATION is non-zero this is used directly (eg to match ECDC numbers)."
   (interactive "sCountry Name?  \nnPopulation or 0? ")
   (switch-to-buffer (format "%s covid" country))
   (org-mode)
   (url-handler-mode t)
   (insert-file-contents "https://covid19.who.int/WHO-COVID-19-global-data.csv")
   (move-end-of-line nil)
   (insert ",14 day,7 day,Graph")
   (keep-lines country (point) (point-max))
   (org-table-convert-region (point-min) (point-max))
   (org-table-insert-hline)
   (goto-char (point-max))
   (let ((p
	  (if (eq population 0) 
	      (cdr (assoc 'population
			  (elt (json-read-file (format "https://restcountries.eu/rest/v2/name/%s?fields=population" country)) 0)))
	    population)))
     ;; Zero data until last element of initial window, last element is then just copied (and scaled),
     ;; after that it's the difference between the last and first cumulative values in the window.
     (insert (format "#+TBLFM: @2$9..@14$9 = 0 :: @15$9 =  $6 * 100000/%d :: @16$9..@>$9 = ($6 - @-14$6) * 100000/%d :: @2$10..@7$10 = 0 :: @8$10 =  $6 * 100000/%d :: @9$10..@>$10 = ($6 - @-7$6) * 100000/%d :: $11 = '(orgtbl-uc-draw-grid $10 0 100 40)"
		     p p p p)))
   (org-ctrl-c-ctrl-c)
   (org-ctrl-c-ctrl-c) ;recalc (twice or graph is missing?!)
   (org-beginning-of-line))

(provide 'covid)
;;; covid ends here
