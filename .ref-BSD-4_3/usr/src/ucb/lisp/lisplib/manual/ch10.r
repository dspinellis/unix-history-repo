






                        CHAPTER  10


                     Exception Handling






   10.1.  Errset and Error Handler Functions

           FRANZ LISP allows the user to handle in a  number
      of  ways  the  errors  which arise during computation.
      One way is through the use of the _e_r_r_s_e_t function.  If
      an  error occurs during the evaluation of the _e_r_r_s_e_t's
      first argument, then the locus of control will  return
      to the errset which will return nil (except in special
      cases, such as _e_r_r).  The other method of  error  han-
      dling  is  through an error handler function.  When an
      error occurs, the error handler is called and is given
      as  an argument a description  of the error which just
      occurred.  The error handler may take one of the  fol-
      lowing actions:

      (1)  it could take some drastic action like a _r_e_s_e_t or
           a _t_h_r_o_w.

      (2)  it could, assuming that the error is continuable,
           return  to  the function which noticed the error.
           The error handler  indicates  that  it  wants  to
           return a value from the error by returning a list
           whose _c_a_r is the value it wants to return.

      (3)  it could decide  not  to  handle  the  error  and
           return a non-list to indicate this fact.



   10.2.  The Anatomy of an error

           Each error is described by a list of these items:

      (1)  error type - This is a symbol which indicates the
           general  classification of the error.  This clas-
           sification may determine which  function  handles
           this error.

      (2)  unique id - This  is  a  fixnum  unique  to  this
           error.

      (3)  continuable - If this is non-nil then this  error
           is  continuable.   There  are  some who feel that


Exception Handling                                      10-1







Exception Handling                                      10-2


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



   10.3.  Error handling algorithm

           This is the sequence of operations which is  done
      when an error occurs:

      (1)  If the symbol ER%all has a  non  nil  value  then
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
           cial happens before  we  call  the  ER%all  error
           handler which does not happen in any of the other
           cases we will describe  below.   To  help  insure
           that we don't get infinitely recursive errors  if
           ER%all is set to a bad value, the value of ER%all
           is set to nil before the handler is called.  Thus
           it is the responsibility of the ER%all handler to
           `reenable' itself by storing its name in ER%all.

      (2)  Next the specific error handler for the  type  of
           error  which  just  occurred  is  called  (if one
           exists) to see if it wants to handle  the  error.
           The  names of the handlers for the specific types
           of errors are stored as the values of the symbols
           whose  names  are  the  types.   For  example the
           handler for miscellaneous errors is stored as the
           value  of  ER%misc.   Of course, if ER%misc has a
           value of nil, then there is no error handler  for


                                      Printed: July 21, 1983







Exception Handling                                      10-3


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
           printed.   Finally   the  stack  is popped to the
           context of the _e_r_r_s_e_t and then the _e_r_r_s_e_t returns
           nil.  If there was no _e_r_r_s_e_t we go to step (4).

      (4)  If the symbol ER%tpl has a value then it  is  the
           name  of  an  error  handler which is called in a
           manner similar to that discussed  above.   If  it
           chooses  not  to  handle the error, we go to step
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



   10.4.  Default aids

           There are two standard error handlers  which will
      probably handle the needs of most users.  One of these
      is the lisp coded function _b_r_e_a_k-_e_r_r-_h_a_n_d_l_e_r which  is
      the  default  value  of  ER%tpl.   Thus when all other
      handlers have ignored an error, _b_r_e_a_k-_e_r_r-_h_a_n_d_l_e_r will
      take over.  It will print out the error message and go


                                      Printed: July 21, 1983







Exception Handling                                      10-4


      into a read-eval-print loop.  The other standard error
      handler   is   _d_e_b_u_g-_e_r_r-_h_a_n_d_l_e_r.    This  handler  is
      designed to be connected to  ER%alland  is  useful  if
      your  program  uses _e_r_r_s_e_t and you want to look at the
      error  before it is thrown up to the _e_r_r_s_e_t.



   10.5.  Autoloading

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



   10.6.  Interrupt processing

           The  UNIX  operating  system  provides  one  user
      interrupt character which defaults to ^C.[]  The  user
      may  select  a  lisp function to run when an interrupt
      occurs.  Since this interrupt could occur at any time,
      and  in  particular  could  occur  at  a time when the
      internal stack pointers were in an inconsistent state,
      the processing of the interrupt may be delayed until a
      safe time.  When the first ^C is typed, the lisp  sys-
      tem  sets a flag that an interrupt has been requested.
      This flag is  checked at safe places within the inter-
      preter  and in the _q_l_i_n_k_e_r function.  If the lisp sys-
      tem doesn't respond to the first ^C, another ^C should
      be  typed.  This will cause all of the transfer tables
      to be cleared forcing all calls from compiled code  to
      go  through  the  _q_l_i_n_k_e_r function where the interrupt
      flag will  be  checked.   If  the  lisp  system  still
      doesn't  respond,  a  third ^C will cause an immediate
      interrupt.  This interrupt will not necessarily be  in
____________________
9   []Actually there are two but the lisp system does not al-
low you to catch the QUIT interrupt.



9                                      Printed: July 21, 1983







Exception Handling                                      10-5


      a safe place so the user should _r_e_s_e_t the lisp  system
      as soon as possible.


















































9

9                                      Printed: July 21, 1983



