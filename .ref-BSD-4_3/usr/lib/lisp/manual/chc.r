






                        APPENDIX  C


                      Short Subjects.





                   The Garbage Collector

     The garbage collector is invoked automatically whenever
a  collectable  data type runs out.  All data types are col-
lectable except strings and atoms are not.  After a  garbage
collection  finishes,  the  collector will call the function
_g_c_a_f_t_e_r which should be a lambda of one argument.  The argu-
ment  passed  to  _g_c_a_f_t_e_r is the name of the data type which
ran out and caused the garbage collection.  It is  _g_c_a_f_t_e_r's
responsibility  to  allocate  more pages of free space.  The
default _g_c_a_f_t_e_r makes its decision based on  the  percentage
of  space  still  in  use  after the garbage collection.  If
there is a large percentage of space still in  use,  _g_c_a_f_t_e_r
allocates a larger amount of free space than if only a small
percentage of space is still in use.   The  default  _g_c_a_f_t_e_r
will  also  print a summary of the space in use if the vari-
able $_g_c_p_r_i_n_t is non nil.  The summary always  includes  the
state  of the list and fixnum space and will include another
type if it caused the garbage collection.   The  type  which
caused the garbage collection is preceded by an asterisk.




                         Debugging

     There are two simple functions to help you  debug  your
programs:  _b_a_k_t_r_a_c_e and _s_h_o_w_s_t_a_c_k.  When an error occurs (or
when you type the interrupt character), you will be left  at
a  break  level  with the state of the computation frozen in
the stack.  At this point, calling  the  function  _s_h_o_w_s_t_a_c_k
will  cause  the contents of the lisp evaluation stack to be
printed in reverse chronological order (most recent  first).
When the programs you are running are interpreted or traced,
the output of _s_h_o_w_s_t_a_c_k can be very verbose.   The  function
_b_a_k_t_r_a_c_e  prints  a  summary of what _s_h_o_w_s_t_a_c_k prints.  That
is, if showstack would print a  list,  _b_a_k_t_r_a_c_e  would  only
print  the  first  element  of the list.  If you are running
compiled code with the (_s_t_a_t_u_s _t_r_a_n_s_l_i_n_k) non nil, then fast
links  are  being  made.   In this case, there is not enough
information on the stack for _s_h_o_w_s_t_a_c_k and _b_a_k_t_r_a_c_e.   Thus,
if  you  are  debugging compiled code you should probably do
(_s_s_t_a_t_u_s _t_r_a_n_s_l_i_n_k _n_i_l).
9

9                                                         C-1







                                                         C-2


     If the contents of the  stack  don't  tell  you  enough
about your problem, the next thing you may want to try is to
run your program with certain  functions  traced.   You  can
direct  the  trace package to stop program execution when it
enters a function, allowing you to examine the  contents  of
variables  or  call  other  functions.  The trace package is
documented in Chapter 11.

     It is also possible to single step the evaluator and to
look  at  stack frames within lisp.  The programs which per-
form these actions are described in Chapters 14 and 15.









































9

9                                      Printed: July 21, 1983







                                                         C-3


                The Interpreter's Top Level

     The default top  level  interpreter  for  Franz,  named
_f_r_a_n_z-_t_o_p-_l_e_v_e_l is defined in /usr/lib/lisp/toplevel.l It is
given control when the lisp system  starts  up  because  the
variable  top-level  is bound to the symbol _f_r_a_n_z-_t_o_p-_l_e_v_e_l.
The first action _f_r_a_n_z-_t_o_p-_l_e_v_e_l takes is to print  out  the
name  of  the  current  version of the lisp system.  Then it
loads the file .lisprc from the HOME directory of the person
invoking  the  lisp system if that file exists.  The .lisprc
file allows you to set up your own defaults, read in  files,
set up autoloading  or anything else you might want to do to
personalize the lisp system.  Next, the top level goes  into
a  prompt-read-eval-print  loop.  Each time around the loop,
before printing the prompt it checks if the  variable  user-
top-level is bound.  If so, then the value of user-top-level
will be _f_u_n_c_a_l_led.  This provides a  convenient  way  for  a
user  to  introduce  his own top level (Liszt, the lisp com-
piler, is an example of a program which uses this).  If  the
user  types  a  ^D (which is the end of file character), and
the standard input is not from a keyboard, the  lisp  system
will  exit.   If the standard input is a keyboard and if the
value of (_s_t_a_t_u_s _i_g_n_o_r_e_e_o_f) is nil,  the  lisp  system  will
also exit.  Otherwise the end of file will be ignored.  When
a _r_e_s_e_t is done the current value of _e_r_r_l_i_s_t is  saved  away
and control is thrown back up to the top level where _e_v_a_l is
mapped over the saved value of _e_r_r_l_i_s_t.

























9

9                                      Printed: July 21, 1983



