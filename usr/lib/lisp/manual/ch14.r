






                        CHAPTER  14


                      The LISP Stepper







   _1_4._1.  _S_i_m_p_l_e _U_s_e _O_f _S_t_e_p_p_i_n_g


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







   _1_4._2.  _A_d_v_a_n_c_e_d _F_e_a_t_u_r_e_s




      _1_4._2._1.  _S_e_l_e_c_t_i_v_e_l_y _T_u_r_n_i_n_g _O_n _S_t_e_p_p_i_n_g.

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


                                       Printed: July 9, 1981







The LISP Stepper                                        14-3


         arguments  to  the  function  have   already   been
         evaluated  but  when _e_v_a_l is intercepted, the argu-
         ments have not been  evaluated.   To  differentiate
         the  two  cases,  when printing the form in evalua-
         tion, the stepper preceeded  intercepted  calls  to
         _f_u_n_c_a_l_l  with  "f:".  Calls to _f_u_n_c_a_l_l are normally
         caused by compiled lisp code  calling  other  func-
         tions,  whereas  calls  to  _e_v_a_l usually occur when
         lisp code is interpreted.  To step  only  calls  to
         eval use:         (_s_t_e_p _e)





      _1_4._2._2.  _S_t_e_p_p_i_n_g _W_i_t_h _B_r_e_a_k_p_o_i_n_t_s.

              For the moment, step is turned off  inside  of
         error  breaks, but not by the break function.  Upon
         exiting the error,  step  is  reenabled.   However,
         executing  (_s_t_e_p _n_i_l) inside a error loop will turn
         off stepping globally, i.e. within the error  loop,
         and after return has be made from the loop.




   _1_4._3.  _O_v_e_r_h_e_a_d _o_f _S_t_e_p_p_i_n_g.

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













9

9                                       Printed: July 9, 1981



