






                        CHAPTER  14


                      The LISP Stepper






   14.1.  Simple Use Of Stepping

(step s_arg1...)

     NOTE: The LISP "stepping" package is intended  to  give
           the  LISP  programmer a facility analogous to the
           Instruction  Step  mode  of  running  a   machine
           language  program.  The user interface is through
           the function (fexpr) step, which sets switches to
           put the LISP interpreter in and out of "stepping"
           mode.  The most common _s_t_e_p  invocations  follow.
           These  invocations  are usually typed at the top-
           level, and will take effect immediately (i.e. the
           next  S-expression  typed in will be evaluated in
           stepping mode).


    ____________________________________________________

    (_s_t_e_p _t)                                ; Turn on stepping mode.
    (_s_t_e_p _n_i_l)                      ; Turn off stepping mode.
    ____________________________________________________




     SIDE EFFECT: In stepping mode, the LISP evaluator  will
                  print  out  each  S-exp  to  be  evaluated
                  before evaluation, and the returned  value
                  after  evaluation,  calling  itself recur-
                  sively to display the  stepped  evaluation
                  of  each argument, if the S-exp is a func-
                  tion call.  In stepping mode, the  evalua-
                  tor  will wait after displaying each S-exp
                  before evaluation for a command  character
                  from the console.






9

9The LISP Stepper                                        14-1







The LISP Stepper                                        14-2



    ____________________________________________________

    _S_T_E_P _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

    <return>                        Continue stepping recursively.

    c                               Show returned value from this level
                                    only, and continue stepping upward.

    e                               Only step interpreted code.

    g                               Turn off stepping mode. (but continue
                                    evaluation without stepping).

    n <number>                      Step through <number> evaluations without
                                    stopping

    p                               Redisplay current form in full
                                    (i.e. rebind prinlevel and prinlength to nil)

    b                               Get breakpoint

    q                               Quit

    d                               Call debug
    ____________________________________________________






   14.2.  Advanced Features



      14.2.1.  Selectively Turning On Stepping.

         If
                (_s_t_e_p _f_o_o_1 _f_o_o_2 ...)

         is typed at top level, stepping will  not  commence
         immediately,  but  rather  when the evaluator first
         encounters an S-expression  whose  car  is  one  of
         _f_o_o_1,  _f_o_o_2,  etc.   This form will then display at
         the console, and the evaluator will be in  stepping
         mode waiting for a command character.

              Normally the stepper intercepts calls to  _f_u_n_-
         _c_a_l_l  and  _e_v_a_l.   When _f_u_n_c_a_l_l is intercepted, the
         arguments  to  the  function  have   already   been
         evaluated   but   when  _e_v_a_l  is  intercepted,  the


                                      Printed: July 21, 1983







The LISP Stepper                                        14-3


         arguments have not been  evaluated.   To  differen-
         tiate  the  two  cases,  when  printing the form in
         evaluation, the stepper preceded intercepted  calls
         to  _f_u_n_c_a_l_l  with  "f:".  Calls to _f_u_n_c_a_l_l are nor-
         mally caused by compiled lisp  code  calling  other
         functions, whereas calls to _e_v_a_l usually occur when
         lisp code is interpreted.  To step  only  calls  to
         eval use:         (_s_t_e_p _e)




      14.2.2.  Stepping With Breakpoints.

              For the moment, step is turned off  inside  of
         error  breaks, but not by the break function.  Upon
         exiting the error,  step  is  reenabled.   However,
         executing  (_s_t_e_p _n_i_l) inside a error loop will turn
         off stepping globally, i.e. within the error  loop,
         and after return has be made from the loop.



   14.3.  Overhead of Stepping.

           If stepping mode has been  turned  off  by  (_s_t_e_p
      _n_i_l),  the  execution  overhead of having the stepping
      packing in your LISP is identically nil.  If one stops
      stepping  by  typing  "g", every call to eval incurs a
      small    overhead--several    machine    instructions,
      corresponding  to  the compiled code for a simple cond
      and one function pushdown.  Running  with  (_s_t_e_p  _f_o_o_1
      _f_o_o_2 ...) can be more expensive, since a member of the
      car of the current form into the list (_f_o_o_1 _f_o_o_2  ...)
      is required at each call to eval.



   14.4.  Evalhook and Funcallhook

           There are hooks in the FRANZ LISP interpreter  to
      permit  a user written function to gain control of the
      evaluation process.  These hooks are used by the  Step
      package  just described.  There are two hooks and they
      have been strategically placed in the  two  key  func-
      tions  in the interpreter: _e_v_a_l (which all interpreted
      code goes through) and  _f_u_n_c_a_l_l  (which  all  compiled
      code  goes through if (_s_s_t_a_t_u_s _t_r_a_n_s_l_i_n_k _n_i_l) has been
      done).  The hook in _e_v_a_l is compatible  with  Maclisp,
      but there is no Maclisp equivalent of the hook in _f_u_n_-
      _c_a_l_l.

9

9                                      Printed: July 21, 1983







The LISP Stepper                                        14-4


           To arm the hooks two  forms  must  be  evaluated:
      (*_r_s_e_t _t)  and  (_s_s_t_a_t_u_s _e_v_a_l_h_o_o_k _t).   Once  that  is
      done, _e_v_a_l and _f_u_n_c_a_l_l do a special  check  when  they
      enter.

           If  _e_v_a_l  is  given  a  form  to  evaluate,   say
      (_f_o_o _b_a_r),  and  the symbol `evalhook' is non nil, say
      its value is `ehook', then _e_v_a_l will lambda  bind  the
      symbols  `evalhook'  and `funcallhook' to nil and will
      call ehook passing (_f_o_o _b_a_r) as the argument.   It  is
      ehook's   responsibility  to  evaluate  (_f_o_o _b_a_r)  and
      return its value.  Typically ehook will call the func-
      tion  `evalhook'  to  evaluate  (_f_o_o _b_a_r).   Note that
      `evalhook' is a symbol whose  function  binding  is  a
      system  function  described  in  Chapter  4, and whose
      value binding, if non nil, is the name of a user writ-
      ten  function  (or  a  lambda  expression, or a binary
      object) which  will  gain  control  whenever  eval  is
      called.  `evalhook' is also the name of the _s_t_a_t_u_s tag
      which must be set for all of this to work.

           If _f_u_n_c_a_l_l is given a function, say  foo,  and  a
      set of already evaluated arguments, say barv and bazv,
      and if the symbol `funcallhook' has a non  nil  value,
      say  `fhook', then _f_u_n_c_a_l_l will lambda bind `evalhook'
      and `funcallhook' to nil  and  will  call  fhook  with
      arguments  barv,  bazv  and foo.  Thus fhook must be a
      lexpr since it may be given any number  of  arguments.
      The  function  to  call, foo in this case, will be the
      _l_a_s_t of the arguments given to fhook.   It  is  fhooks
      responsibility  to do the function call and return the
      value.  Typically fhook will call  the  function  _f_u_n_-
      _c_a_l_l_h_o_o_k  to  do the funcall.  This is an example of a
      funcallhook function which  just prints the  arguments
      on each entry to funcall and the return value.

















9

9                                      Printed: July 21, 1983







The LISP Stepper                                        14-5



    ____________________________________________________

    -> (_d_e_f_u_n _f_h_o_o_k _n (_l_e_t ((_f_o_r_m (_c_o_n_s (_a_r_g _n) (_l_i_s_t_i_f_y (_1- _n))))
                            (_r_e_t_v_a_l))
                           (_p_a_t_o_m "_c_a_l_l_i_n_g ")(_p_r_i_n_t _f_o_r_m)(_t_e_r_p_r)
                           (_s_e_t_q _r_e_t_v_a_l (_f_u_n_c_a_l_l_h_o_o_k _f_o_r_m '_f_h_o_o_k))
                           (_p_a_t_o_m "_r_e_t_u_r_n_s ")(_p_r_i_n_t _r_e_t_v_a_l)(_t_e_r_p_r)
                           _r_e_t_v_a_l))
    fhook
    -> (*_r_s_e_t _t) (_s_s_t_a_t_u_s _e_v_a_l_h_o_o_k _t) (_s_s_t_a_t_u_s _t_r_a_n_s_l_i_n_k _n_i_l)
    -> (_s_e_t_q _f_u_n_c_a_l_l_h_o_o_k '_f_h_o_o_k)
    calling (print fhook)           ;; now all compiled code is traced
    fhookreturns nil
    calling (terpr)

    returns nil
    calling (patom "-> ")
    -> returns "-> "
    calling (read nil Q00000)
    (_a_r_r_a_y _f_o_o _t _1_0)                ;; to test it, we see what happens when
    returns (array foo t 10)        ;; we make an array
    calling (eval (array foo t 10))
    calling (append (10) nil)
    returns (10)
    calling (lessp 1 1)
    returns nil
    calling (apply times (10))
    returns 10
    calling (small-segment value 10)
    calling (boole 4 137 127)
    returns 128
     ... there is plenty more ...
    ____________________________________________________


















9

9                                      Printed: July 21, 1983



