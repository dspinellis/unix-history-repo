

(dv lloadfns ((declare (special dc-switch)) load))

(declare (special dc-switch))

(def load
  (lambda (file)
    (prog (port oldswitch piport loading)
          (setq oldswitch
                (or (and (boundp 'dc-switch) dc-switch)
                    'dc-define))
          (setq dc-switch 'dc-dskin)
          (setq loading t)
          (cond
           ((null (setq port (car (errset (infile file) nil))))
            (setq port (car (errset (infile (concat file ".l")) nil)))))
          (cond ((null port) (err '|load: file not found|)))
          (setq piport port)
          (prog (form)
           loop (setq form (read port '%#%#%#%#%EOF))
                (cond ((eq '%#%#%#%#%EOF form) (return file))
                      ((atom form) (eval form))
                      ((and (member (car form)
                                    '(def defun dv putprop defprop))
                            (getd (cadr form)))
                       (princ "[Redefining ")
                       (princ (cadr form))
                       (princ "]")
                       (terpri)
                       (eval form))
                      (t (eval form)))
                (go loop))
          (setq dc-switch oldswitch))))
