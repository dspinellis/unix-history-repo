






                        CCCCHHHHAAAAPPPPTTTTEEEERRRR  11110000


                     EEEExxxxcccceeeeppppttttiiiioooonnnn HHHHaaaannnnddddlllliiiinnnngggg







11110000....1111....  EEEErrrrrrrrsssseeeetttt aaaannnndddd EEEErrrrrrrroooorrrr HHHHaaaannnnddddlllleeeerrrr FFFFuuuunnnnccccttttiiiioooonnnnssss

     FRANZ LISP allows the user to handle  in  a  number  of
ways  the errors which arise during computation.  One way is
through the use of the _e_r_r_s_e_t function.  If an error  occurs
during  the  evaluation of the _e_r_r_s_e_t's first argument, then
the locus of control will return to the  errset  which  will
return  nil  (except  in  special  cases, such as _e_r_r).  The
other method of error handling is through an  error  handler
function.  When an error occurs, the error handler is called
and is given as an argument  a  description   of  the  error
which  just occurred.  The error handler may take one of the
following actions:

(1)  it could take some drastic action like  a  _r_e_s_e_t  or  a
     _t_h_r_o_w.

(2)  it could,  assuming  that  the  error  is  continuable,
     return  to  the  function which noticed the error.  The
     error handler indicates that it wants to return a value
     from  the  error  by  returning a list whose _c_a_r is the
     value it wants to return.

(3)  it could decide not to handle the error  and  return  a
     non-list to indicate this fact.




11110000....2222....  TTTThhhheeee AAAAnnnnaaaattttoooommmmyyyy ooooffff aaaannnn eeeerrrrrrrroooorrrr

     Each error is described by a list of these items:

(1)  error type - This is a symbol which indicates the  gen-
     eral  classification of the error.  This classification
     may determine which function handles this error.

(2)  unique id - This is a fixnum unique to this error.

(3)  continuable - If this is non-nil  then  this  error  is
     continuable.   There are some who feel that every error
     should be continuable and the reason that some (in fact


EEEExxxxcccceeeeppppttttiiiioooonnnn HHHHaaaannnnddddlllliiiinnnngggg                                      11110000----1111







EEEExxxxcccceeeeppppttttiiiioooonnnn HHHHaaaannnnddddlllliiiinnnngggg                                      11110000----2222


     most)  errors  in FRANZ LISP are not continuable is due
     to the laziness of the programmers.

(4)  message string - This is a symbol whose print  name  is
     a message describing the error.

(5)  data - There may be from  zero  to  three  lisp  values
     which  help describe this particular  error.  For exam-
     ple, the unbound  variable  error  contains  one  datum
     value,  the  symbol  whose  value is unbound.  The list
     describing that error might look like:
            (ER%misc 0 t |Unbound Variable:| foobar)




11110000....3333....  EEEErrrrrrrroooorrrr hhhhaaaannnnddddlllliiiinnnngggg aaaallllggggoooorrrriiiitttthhhhmmmm

     This is the sequence of operations which is  done  when
an error occurs:

(1)  If the symbol EEEERRRR%%%%aaaallllllll has a  non  nil  value  then  this
     value  is  the name of an error handler function.  That
     function is called with a description of the error.  If
     that  function returns (and of course it may choose not
     to) and the value is a list and this error is  continu-
     able,  then  we return the _c_a_r of the list to the func-
     tion which called the error.  Presumably  the  function
     will  use  this  value  to retry the operation.  On the
     other hand, if the error handler returns  a  non  list,
     then  it has chosen not to handle this error, so  we go
     on to step (2).  Something special  happens  before  we
     call  the EEEERRRR%%%%aaaallllllll error handler which does not happen in
     any of the other cases we will describe below.  To help
     insure  that  we  don't get infinitely recursive errors
     if EEEERRRR%%%%aaaallllllll is set to a bad value, the value of EEEERRRR%%%%aaaallllllll is
     set  to  nil  before the handler is called.  Thus it is
     the responsibility of the EEEERRRR%%%%aaaallllllll handler to  `reenable'
     itself by storing its name in EEEERRRR%%%%aaaallllllll....

(2)  Next the specific error handler for the type  of  error
     which  just  occurred is called  (if one exists) to see
     if it wants to handle the  error.   The  names  of  the
     handlers for the specific types of errors are stored as
     the values of the symbols whose names  are  the  types.
     For  example  the  handler  for miscellaneous errors is
     stored as the value of EEEERRRR%%%%mmmmiiiisssscccc....  Of course, if  EEEERRRR%%%%mmmmiiiisssscccc
     has  a value of nil, then there is no error handler for
     this type of error.  Appendix B contains  list  of  all
     error  types.  The process of classifying the errors is
     not complete and thus most errors are lumped  into  the
     EEEERRRR%%%%mmmmiiiisssscccc  category.   Just  as  in  step  (1), the error
     handler function may choose not to handle the error  by


                                     Printed: March 23, 1982







EEEExxxxcccceeeeppppttttiiiioooonnnn HHHHaaaannnnddddlllliiiinnnngggg                                      11110000----3333


     returning a non-list, and then we go to step (3).

(3)  Next a check is made to see if there is an _e_r_r_s_e_t  sur-
     rounding  this error.  If so the second argument to the
     _e_r_r_s_e_t call is examined. If the second argument was not
     given  or  is non nil then the error message associated
     with this error is  printed.   Finally   the  stack  is
     popped to the context of the _e_r_r_s_e_t and then the _e_r_r_s_e_t
     returns nil.  If there was no _e_r_r_s_e_t we go to step (4).

(4)  If the symbol EEEERRRR%%%%ttttppppllll has a value then it is the name of
     an error handler which is called in a manner similar to
     that discussed above.  If it chooses not to handle  the
     error, we go to step (5).

(5)  At this point it has  been  determined  that  the  user
     doesn't want to handle this error.  Thus the error mes-
     sage is printed out and a _r_e_s_e_t is  done  to  send  the
     flow of control to the top-level.

     To summarize the error handling system: When  an  error
occurs,  you have two chances to handle it before the search
for an _e_r_r_s_e_t is done.  Then, if there  is  no  _e_r_r_s_e_t,  you
have  one  more  chance  to  handle the error before control
jumps to the top level.  Every  error handler works  in  the
same  way:  It  is  given  a  description  of  the error (as
described in the previous  section).   It  may  or  may  not
return.   If  it returns, then it returns either a list or a
non-list.  If it returns a list and the  error  is  continu-
able,  then  the _c_a_r of the list is returned to the function
which noticed the error.  Otherwise the  error  handler  has
decided  not  to  handle the error and we go on to something
else.




11110000....4444....  DDDDeeeeffffaaaauuuulllltttt aaaaiiiiddddssss

     There are two standard error handlers  which will prob-
ably  handle  the  needs of most users.  One of these is the
lisp coded function _b_r_e_a_k-_e_r_r-_h_a_n_d_l_e_r which is  the  default
value  of EEEERRRR%%%%ttttppppllll....  Thus when all other handlers have ignored
an error, _b_r_e_a_k-_e_r_r-_h_a_n_d_l_e_r will take over.  It  will  print
out  the  error  message and go into a read-eval-print loop.
The other standard error handler is _d_e_b_u_g-_e_r_r-_h_a_n_d_l_e_r.  This
handler  is  designed to be connected to EEEERRRR%%%%aaaalllllllland is useful
if your program uses _e_r_r_s_e_t and you  want  to  look  at  the
error  before it is thrown up to the _e_r_r_s_e_t.



9

9                                     Printed: March 23, 1982







EEEExxxxcccceeeeppppttttiiiioooonnnn HHHHaaaannnnddddlllliiiinnnngggg                                      11110000----4444


11110000....5555....  AAAAuuuuttttoooollllooooaaaaddddiiiinnnngggg

     When _e_v_a_l, _a_p_p_l_y or _f_u_n_c_a_l_l are told to call  an  unde-
fined  function, an EEEERRRR%%%%uuuunnnnddddeeeeffff error is signaled.  The default
handler for this error is _u_n_d_e_f-_f_u_n_c-_h_a_n_d_l_e_r.  This function
checks  the  property list of the undefined function for the
indicator autoload.  If present, the value of that indicator
should be the name of the file which contains the definition
of the undefined function.  _U_n_d_e_f-_f_u_n_c-_h_a_n_d_l_e_r will load the
file  and  check if it has defined the function which caused
the error.  If it has, the error handler will return and the
computation  will  continue  as  if the error did not occur.
This provides a way for the user to  tell  the  lisp  system
about  the  location  of commonly used functions.  The trace
package  sets  up  an  autoload   property   to   point   to
/usr/lib/lisp/trace.




11110000....6666....  IIIInnnntttteeeerrrrrrrruuuupppptttt pppprrrroooocccceeeessssssssiiiinnnngggg

     The  UNIX operating system provides one user  interrupt
character which defaults to ^C.[] The user may select a lisp
function to run when an interrupt occurs.  Since this inter-
rupt could occur at any time, and in particular could  occur
at a time when the internal stack pointers were in an incon-
sistent state,  the  processing  of  the  interrupt  may  be
delayed  until a safe time.  When the first ^C is typed, the
lisp  system  sets  a  flag  that  an  interrupt  has   been
requested.   This flag is  checked at safe places within the
interpreter and in the _q_l_i_n_k_e_r function.  If the lisp system
doesn't respond to the first ^C, another ^C should be typed.
This will cause all of the transfer  tables  to  be  cleared
forcing  all  calls  from  compiled  code  to go through the
_q_l_i_n_k_e_r function where the interrupt flag will  be  checked.
If  the  lisp  system still doesn't respond, a third ^C will
cause an  immediate  interrupt.   This  interrupt  will  not
necessarily  be in a safe place so the user should _r_e_s_e_t the
lisp system as soon as possible.








____________________
9   []Actually there are two but the lisp system does not al-
low you to catch the QUIT interrupt.



9                                     Printed: March 23, 1982



