






                        CHAPTER  10


                     Exception Handling







   _1_0._1.  _E_r_r_s_e_t _a_n_d _E_r_r_o_r _H_a_n_d_l_e_r _F_u_n_c_t_i_o_n_s

           FRANZ LISP allows the user to handle in a  number
      of  ways  the  errors  which arise during computation.
      One way is through the use of the _e_r_r_s_e_t function.  If
      an  error occurs during the evaluation of the _e_r_r_s_e_t's
      first argument, then the locus of control will  return
      to the errset which will return nil (except in special
      cases, such as _e_r_r).  The other method of  error  han-
      dling  is  through an error handler function.  When an
      error occurs, the interrupt handler is called  and  is
      given as an argument a description  of the error which
      just occured.  The error handler may take one  of  the
      following actions:

      (1)  it could take some drastic action like a _r_e_s_e_t or
           a _t_h_r_o_w.

      (2)  it could, assuming that the error is continuable,
           cause  a   value  to  be  returned from the error
           handler to the function which noticed the  error.
           The  error  handler  indicates  that  it wants to
           return a value from the error by returning a list
           whose _c_a_r is the value it wants to return.

      (3)  it could decide  not  to  handle  the  error  and
           return a non-list to indicate this fact.




   _1_0._2.  _T_h_e _A_n_a_t_o_m_y _o_f _a_n _e_r_r_o_r

           Each error is described by a list of these items:

      (1)  error type - This is a symbol which indicates the
           general  classification of the error.  This clas-
           sification may determine which  function  handles
           this error.

      (2)  unique id - This  is  a  fixnum  unique  to  this
           error.


Exception Handling                                      10-1







Exception Handling                                      10-2


      (3)  continuable - If this is non-nil then this  error
           is  continuable.   There  are  some who feel that
           every error should be continuable and the  reason
           that some (in fact most) errors in FRANZ LISP are
           not continuable is due to  the  laziness  of  the
           programmers.

      (4)  message string - This is  a  symbol  whose  print
           name is  a message describing the error.

      (5)  data - There may  be  from  zero  to  three  lisp
           values   which   help  describe  this  particular
           error.  For example, the unbound  variable  error
           contains  one datum value, the symbol whose value
           is unbound.  The list describing that error might
           look like:
               (ER%misc 0 t |Unbound Variable:| foobar)




   _1_0._3.  _E_r_r_o_r _h_a_n_d_l_i_n_g _a_l_g_o_r_i_t_h_m

           This is the sequence of operations which is  done
      when an error occurs:

      (1)  If the symbol _E_R%_a_l_l has a  non  nil  value  then
           this  value is the name of an error handler func-
           tion.  That function is called with a description
           of  the  error.  If that function returns (and of
           course it may choose not to) and the value  is  a
           list  and  this  error  is  continuable,  then we
           return the _c_a_r of the list to the function  which
           called  the error.  Presumably the function  will
           use this value to retry the  operation.   On  the
           other  hand,  if  the error handler returns a non
           list, then it  has  chosen  not  to  handle  this
           error,  so  we go on to step (2).  Something spe-
           cial happens before  we  call  the  _E_R%_a_l_l  error
           handler which does not happen in any of the other
           cases we will describe  below.   To  help  insure
           that we don't get infinitely recursive errors  if
           _E_R%_a_l_l is set to a bad value, the value of _E_R%_a_l_l
           is set to nil before the handler is called.  Thus
           it is the responsibility of the _E_R%_a_l_l handler to
           `reenable' itself by storing its name in _E_R%_a_l_l.

      (2)  Next the specific error handler for the  type  of
           error  which  just  occured  is  called   (if one
           exists) to see if it wants to handle  the  error.
           The  names of the handlers for the specific types
           of errors are stored as the values of the symbols
           whose  names  are  the  types.   For  example the


                                       Printed: July 9, 1981







Exception Handling                                      10-3


           handler for miscellaneous errors is stored as the
           value  of  _E_R%_m_i_s_c.   Of course, if _E_R%_m_i_s_c has a
           value of nil, then there is not error handler for
           this  type of error.  Appendix B contains list of
           all error types.  The process of classifying  the
           errors  is  not complete and thus most errors are
           lumped into the ER%misc  category.   Just  as  in
           step  (1),  the error handler function may choose
           not to handle the error by returning a  non-list,
           and then we go to step (3).

      (3)  Next a check is made to see if there is an _e_r_r_s_e_t
           surrounding  this  error.  If so the second argu-
           ment to the  _e_r_r_s_e_t  call  is  examined.  If  the
           second  argument was not given or is non nil then
           the error message associated with this  error  is
           printed  Finally  the stack is popped to the con-
           text of the _e_r_r_s_e_t and then  the  _e_r_r_s_e_t  returns
           nil.  If there was no _e_r_r_s_e_t we go to step (4).

      (4)  If the symbol _E_R%_t_p_l has a value then it  is  the
           name  of  and  error handler which is called in a
           manner similar to the that discussed  above.   If
           it chooses not to handle the error, we go to step
           (5).

      (5)  At this point it has  been  determined  that  the
           user doesn't want to handle this error.  Thus the
           error message is printed out and a _r_e_s_e_t is  done
           to send the flow of control to the top-level.

           To summarize the error handling system:  When  an
      error occurs, you have two chances to handle it before
      the search for an _e_r_r_s_e_t is done.  Then, if  there  is
      no  _e_r_r_s_e_t,  you  have  one  more chance to handle the
      error before control jumps to the  top  level.   Every
      error  handler  works  in  the same way: It is given a
      description of the error (as described in the previous
      section).   It  may or may not return.  If it returns,
      then it returns either a list or a  non-list.   If  it
      returns  a list and the error is continuable, then the
      _c_a_r of the list is  returned  to  the  function  which
      noticed  the  error.   Otherwise the error handler has
      decided not to handle the error and we go on to  some-
      thing else.




   _1_0._4.  _D_e_f_a_u_l_t _a_i_d_s

           There are two standard error handlers  which will
      probably handle the needs of most users.  One of these


                                       Printed: July 9, 1981







Exception Handling                                      10-4


      is the lisp coded function _b_r_e_a_k-_e_r_r-_h_a_n_d_l_e_r which  is
      the  default  value  of  _E_R%_t_p_l.   Thus when all other
      handlers have ignored an error, _b_r_e_a_k-_e_r_r-_h_a_n_d_l_e_r will
      take over.  It will print out the error message and go
      into a read-eval-print loop.  For a further discussion
      of _b_r_e_a_k-_e_r_r-_h_a_n_d_l_e_r, see section xx.  The other stan-
      dard error handler is _d_e_b_u_g-_e_r_r-_h_a_n_d_l_e_r.  This handler
      is  designed  to be connected to _E_R%_a_l_l.  It is useful
      if your program uses _e_r_r_s_e_t and you want  to  look  at
      the error it is thrown up to the _e_r_r_s_e_t.




   _1_0._5.  _A_u_t_o_l_o_a_d_i_n_g

           When _e_v_a_l, _a_p_p_l_y or _f_u_n_c_a_l_l are told to  call  an
      undefined  function,  an  ER%undef  error is signaled.
      The default handler  for  this  error  is  _u_n_d_e_f-_f_u_n_c-
      _h_a_n_d_l_e_r.   This  function  checks the property list of
      the undefined function for the indicator autoload.  If
      present,  the  value  of  that indicator should be the
      name of the file which contains the definition of  the
      undefined  function.  _U_n_d_e_f-_f_u_n_c-_h_a_n_d_l_e_r will load the
      file and check if it has defined  the  function  which
      caused  the  error.  If it has, the error handler will
      return and the computation will  continue  as  if  the
      error did not occur.  This provides a way for the user
      to tell the lisp system about the location of commonly
      used functions.  The trace package sets up an autoload
      property to point to /usr/lib/lisp/trace.




   _1_0._6.  _I_n_t_e_r_r_u_p_t _p_r_o_c_e_s_s_i_n_g

           The  UNIX  operating  system  provides  one  user
      interrupt character which defaults to ^C.[].  The user
      may  select  a  lisp function to run when an interrupt
      occurs.  Since this interrupt could occur at any time,
      and  in  particular  could  occur  at  a time when the
      internal stack pointers were in an inconsistent state,
      the processing of the interrupt may be delayed until a
      safe time.  When the first ^C is typed, the lisp  sys-
      tem  sets a flag that an interrupt has been requested.
      This flag is  checked at safe places within the inter-
      preter  and  in  the  _q_l_i_n_k_e_r  function.   If the lisp
____________________
9   []Actually there are two but the lisp system does not al-
low you to catch the QUIT interrupt.



9                                       Printed: July 9, 1981







Exception Handling                                      10-5


      system doesn't respond to the  first  ^C,  another  ^C
      should  be typed.  This will cause all of the transfer
      tables to be cleared forcing all calls  from  compiled
      code  to  go  through  the  _q_l_i_n_k_e_r function where the
      interrupt flag will be checked.  If  the  lisp  system
      still  doesn't  respond,  a  third  ^C  will  cause an
      immediate interrupt.  This interrupt will  not  neces-
      sarily be in a safe place so the user should _r_e_s_e_t the
      lisp system as soon as possible.











































9

9                                       Printed: July 9, 1981



