; LWE 11/28/80 Simple function reads lists of function names from
; a list of file.l root names, with extensions file.al.  It places
; each function in the list under (putprop '<function> '<file> 'autoload)

(defun autoloadify (file-list)
	(mapc '(lambda (file)
		((lambda (file-fns)
			(mapc '(lambda (fn)
					(cond
					 ((get fn 'autoload)
					  (princ "[")
					  (princ fn)
					  (princ " re-autoloadified from ")
					  (princ (get fn 'autoload))
					  (princ " to ")
					  (princ file)
					  (princ "]")(terpri)))
					(putprop fn file 'autoload)
				)
			      file-fns)
		 )
		 (read (infile (concat file ".al"))))
       )
      file-list
))
