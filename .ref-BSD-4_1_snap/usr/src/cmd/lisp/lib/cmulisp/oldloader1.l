
(load 'atlinks0)
(load 'atlinks1)
(load 'atlinks2)
(cfasl '/usr/lwe/interp/c/getwdir.o '_Mygetwd 'getwd "function" "-lcmu")
(setq top-level (getd 'top-level))
(defun top-level nil
    (prog nil
      (putd 'top-level top-level)
      (setq %changes nil)
      (princ '"/usr/lisp/newlisp 1-28-81. Interpreter purification.
" nil)

      (errset(read-in-lisprc-file) nil)
      (reset]
(setq %changes nil)
(gc)
(print "If you get an error now, things are wedged.")
(errset (mumble) nil)
(sstatus dumpmode 413)
'(LWE 1/28/81 Changing to C directory)
(chdir '\.\./c)
(dumplisp newlisp)
