
(dv readhunksfns
    ((setsyntax 125 66)
     readhunk
     (setsyntax 123 'macro 'readhunk)))

(setsyntax 125 66)

(def readhunk
     (lambda 
      nil
      (prog (l x)
       l:   (cond ((eq (setq x (read)) '})
                   (setq l (reverse l))
                   (setq x (makhunk (length l)))
                   (for i = 0 while l (rplacx i x (car l)) (setq l (cdr l)))
                   (return x))
                  (t (setq l (cons x l)) (go l:))))))

(setsyntax 123 'macro 'readhunk)

