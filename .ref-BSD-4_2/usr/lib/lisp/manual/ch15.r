






                        CHAPTER  15


                     The FIXIT Debugger






   15.1.  Introduction   FIXIT is  a  debugging  environment
      for  FRANZ LISP users doing program development.  This
      documentation and  FIXIT  were  written  by  David  S.
      Touretzky  of  Carnegie-Mellon University for MACLisp,
      and adapted to FRANZ LISP  by  Mitch  Marcus  of  Bell
      Labs.  One of FIXIT's goals is to get the program run-
      ning again  as  quickly  as  possible.   The  user  is
      assisted  in  making  changes to his functions "on the
      fly", i.e. in the midst of execution, and then  compu-
      tation is resumed.

           To enter the debugger type (_d_e_b_u_g).  The debugger
      goes  into  its  own  read-eval-print  loop.  Like the
      top-level, the debugger  understands  certain  special
      commands.   One  of these is help, which prints a list
      of the available commands.  The basic idea is that you
      are  somewhere  in a stack of calls to eval.  The com-
      mand "bka" is probably the most appropriate for  look-
      ing  at  the stack.  There are commands to move up and
      down.  If you want to know the value of "x" as of some
      place  in  the  stack, move to that place and type "x"
      (or (cdr x) or anything else that you  might  want  to
      evaluate).   All  evaluation is done as of the current
      stack position.  You can fix the problem  by  changing
      the  values of variables, editing functions or expres-
      sions in the stack etc.  Then you  can  continue  from
      the current stack position (or anywhere else) with the
      "redo" command.  Or you can simply  return  the  right
      answer with the "return" command.

           When it is not immediately obvious why  an  error
      has  occurred  or  how the program got itself into its
      current state, FIXIT comes to the rescue by  providing
      a powerful debugging loop in which the user can:

      -  examine the stack

      -  evaluate expressions in context

      -  enter stepping mode

      -  restart the computation at any point
9

9The FIXIT Debugger                                      15-1







The FIXIT Debugger                                      15-2


      The result is that program errors can be  located  and
      fixed  extremely  rapidly, and with a minimum of frus-
      tration.

           The debugger can only work effectively when extra
      information  is  kept about forms in evaluation by the
      lisp system.  Evaluating (*_r_s_e_t _t) tells the lisp sys-
      tem to maintain this information. If you are debugging
      compiled code you should also be sure  that  the  com-
      piled   code  to  compiled  code  linkage  tables  are
      unlinked, i.e do (_s_s_t_a_t_u_s _t_r_a_n_s_l_i_n_k _n_i_l).


(debug [ s_msg ])

     NOTE: Within a program, you  may  enter  a  debug  loop
           directly  by putting in a call to _d_e_b_u_g where you
           would normally put a call to _b_r_e_a_k.  Also, within
           a break loop you may enter FIXIT by typing _d_e_b_u_g.
           If an argument is given to DEBUG, it  is  treated
           as  a message to be printed before the debug loop
           is entered.  Thus you can put (_d_e_b_u_g |_j_u_s_t _b_e_f_o_r_e
           _l_o_o_p|)  into  a  program to indicate what part of
           the program is being debugged.




























9

9                                      Printed: July 21, 1983







The FIXIT Debugger                                      15-3



    ____________________________________________________

    _F_I_X_I_T _C_o_m_m_a_n_d _S_u_m_m_a_r_y

    TOP     go to top of stack (latest expression)
    BOT     go to bottom of stack (first expression)
    P       show current expression (with ellipsis)
    PP      show current expression in full
    WHERE   give current stack position
    HELP    types the abbreviated command summary found
            in /usr/lisp/doc/fixit.help.  H and ? work too.
    U       go up one stack frame
    U n     go up n stack frames
    U f     go up to the next occurrence of function f
    U n f   go up n occurrences of function f
    UP      go up to the next user-written function
    UP n    go up n user-written functions
     ...the DN and DNFN commands are similar, but go down
     ...instead of up.
    OK      resume processing; continue after an error or debug loop
    REDO    restart the computation with the current stack frame.
            The OK command is equivalent to TOP followed by REDO.
    REDO f  restart the computation with the last call to function f.
            (The stack is searched downward from the current position.)
    STEP    restart the computation at the current stack frame,
            but first turn on stepping mode.  (Assumes Rich stepper is loaded.)
    RETURN e   return from the current position in the computation
               with the value of expression e.
    BK..    print a backtrace.  There are many backtrace commands,
            formed by adding suffixes to the BK command.  "BK" gives
            a backtrace showing only user-written functions, and uses
            ellipsis.  The BK command may be suffixed by one or more
            of the following modifiers:
     ..F..   show function names instead of expressions
     ..A..   show all functions/expressions, not just user-written ones
     ..V..   show variable bindings as well as functions/expressions
     ..E..   show everything in the expression, i.e. don't use ellipsis
     ..C..   go no further than the current position on the stack
            Some of the more useful combinations are BKFV, BKFA,
            and BKFAV.
    BK.. n    show only n levels of the stack (starting at the top).
              (BK n counts only user functions; BKA n counts all functions.)
    BK.. f    show stack down to first call of function f
    BK.. n f  show stack down to nth call of function f
    ____________________________________________________






9

9                                      Printed: July 21, 1983







The FIXIT Debugger                                      15-4


   15.2.  Interaction with _t_r_a_c_e    FIXIT  knows  about  the
      standard  Franz trace package, and tries to make trac-
      ing invisible  while  in  the  debug  loop.   However,
      because  of  the  way _t_r_a_c_e works, it may sometimes be
      the case that the functions on the  stack  are  really
      un_i_n_t_e_r_ned  atoms  that have the same name as a traced
      function.  (This  only  happens  when  a  function  is
      traced  WHEREIN  another one.)  FIXIT will call atten-
      tion to _t_r_a_c_e'_s hackery by printing an appropriate tag
      next to these stack entries.




   15.3.  Interaction with _s_t_e_p   The _s_t_e_p function  may  be
      invoked from within FIXIT via the STEP command.  FIXIT
      initially turns off stepping when the  debug  loop  is
      entered.   If  you  step through a function and get an
      error, FIXIT will still be invoked normally.   At  any
      time  during  stepping, you may explicitly enter FIXIT
      via the "D" (debug) command.




   15.4.  Multiple error levels   FIXIT will evaluate  arbi-
      trary LISP expressions in its debug loop.  The evalua-
      tion is not done within an _e_r_r_s_e_t,  so,  if  an  error
      occurs,  another  invocation  of  the  debugger can be
      made.  When there are multiple errors  on  the  stack,
      FIXIT  displays  a  barrier  symbol between each level
      that looks something  like  <------------UDF-->.   The
      UDF in this case stands for UnDefined Function.  Thus,
      the upper level debug loop was invoked by an undefined
      function error that occurred while in the lower loop.

















9

9                                      Printed: July 21, 1983



