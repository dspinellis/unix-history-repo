cat ch8auxf.f
f77 -c ch8auxf.f
lisp
(cfasl 'ch8auxf.o '_ffoo_ 'ffoo "real-function" "-lF77 -lI77")
(array test fixnum-block 2)
(store (test 0) 10)
(store (test 1) 11)
(ffoo 385 (getd 'test) 5.678)
(test 0)
