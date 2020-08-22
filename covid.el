;;; covid --- summary info.
;;; Commentary:
;;; Calculates 14 and 7 day cumulative new cases from WHO data.

;;; Code:
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
     (insert (format "#+TBLFM: @2$9..@15$9 = 0 :: @16$9..@>$9 = ($6 - @-14$6) * 100000/%d :: @2$10..@8$10 = 0 :: @9$10..@>$10 = ($6 - @-7$6) * 100000/%d :: $11 = '(orgtbl-ascii-draw $10 0 100 20)"
		     p p)))
   (org-ctrl-c-ctrl-c)
   (org-ctrl-c-ctrl-c)) ;recalc (twice or graph is missing?!)

(provide 'covid)
;;; covid ends here
