;;; covid --- summary info.
;;; Commentary:
;;; Only works if you comment out the user-error in org-table--row-type.

;;; Code:
(defun covid (country)
   "Helper function to get covid details from COUNTRY."
   (interactive "sCountry Name? ")
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
   (let ((population
	  (cdr (assoc 'population
		      (elt (json-read-file (format "https://restcountries.eu/rest/v2/name/%s?fields=population" country)) 0)))))
     (insert (format "#+TBLFM: $9=if (@# > 15, ($6-@-14$6)*100000/%d, 0)::$10=if (@# > 8, ($6-@-7$6)*100000/%d, 0)::$11='(orgtbl-ascii-draw $10 0 100 20)"
		     population population)))
   (org-ctrl-c-ctrl-c)) ;recalc


(provide 'covid)
;;; covid ends here