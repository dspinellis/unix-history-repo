






                                CHAPTER  6


                             System Functions



This chapter describes the functions which one uses to interact with  FRANZ
LISP running in the UNIX environment.


(allocate 's_type 'x_pages)

     WHERE:   s_type is one of the FRANZ LISP data types described in 1.3.

     RETURNS: x_pages.

     SIDE EFFECT: FRANZ LISP attempts to allocate x_pages of  type  s_type.
                  It  allocates  pages  one  at  a time so that if an error
                  occurs this means that all free storage  has  been  allo-
                  cated.  The storage that is allocated is not given to the
                  caller, instead it is added to the free storage  list  of
                  s_type.  The functions _s_e_g_m_e_n_t and _s_m_a_l_l-_s_e_g_m_e_n_t allocate
                  blocks  of storage and return it to the caller.


(argv 'x_argnumb)

     RETURNS: a symbol whose pname is the x_argnumb_t_h argument (starting at
              0) on the command line which invoked the current lisp.

     NOTE: if x_argnumb is less that zero, a  fixnum  whose  value  is  the
           number  of  arguments on the command line is returned.  (_a_r_g_v _0)
           returns the name of the lisp you are running.


(baktrace)

     RETURNS: nil

     SIDE EFFECT: the lisp runtime stack is examined and the name of (most)
                  of the functions currently in execution are printed, most
                  active first.

     NOTE: this will occasionally miss the names of compiled lisp functions
           due  to incomplete information on the stack.  If you are tracing
           compiled code, then _b_a_k_t_r_a_c_e won't  be  able  to  interpret  the
           stack unless (_s_s_t_a_t_u_s _t_r_a_n_s_l_i_n_k _n_i_l) was done.  See the function
           _s_h_o_w_s_t_a_c_k for another way of printing the lisp runtime stack.



9

9System Functions                                                        6-1







System Functions                                                        6-2


(boundp 's_name)

     RETURNS: nil  if s_name is unbound, that is it has never  be  given  a
              value.   If x_name has the value g_val, then (nil . g_val) is
              returned.


(chdir 's_path)

     RETURNS: t iff the system call succeeds.

     SIDE EFFECT: the current directory set to s_path. Among other  things,
                  this   will   affect   the  default  location  where  the
                  input/output functions look for and create files.

     NOTE: _c_h_d_i_r follows the standard UNIX conventions, if s_path does  not
           begin  with  a slash, the default path is changed to the current
           path with s_path appended.


(dumplisp s_name)

     RETURNS: nil

     SIDE EFFECT: the current lisp is dumped to the disk with the file name
                  s_name.   When  s_name is executed, you will be in a lisp
                  in the same state as when the dumplisp was done.

     NOTE: dumplisp will fail if  one tries to write over the current  run-
           ning  file.  UNIX  does not allow you to modify the file you are
           running.


(eval-when l_time g_exp1 ...)

     SIDE EFFECT: l_time may contain any combination of the  symbols  _l_o_a_d,
                  _e_v_a_l,  and _c_o_m_p_i_l_e.  The effects of load and compile will
                  is discussed in the section on the compiler.  If eval  is
                  present  however,  this simply means that the expressions
                  g_exp1 and so on are evaluated from left  to  right.   If
                  eval is not present, the forms are not evaluated.











9

9                                                  Printed: October 22, 1980







System Functions                                                        6-3


(exit ['x_code])

     RETURNS: nothing (it never returns).

     SIDE EFFECT: the lisp system dies with exit code x_code or 0 if x_code
                  is not specified.


(fake 'x_addr)

     RETURNS: the lisp object at address x_addr.

     NOTE: This is intended to be used by people debugging the lisp system.


(gc)

     RETURNS: nil

     SIDE EFFECT: this causes a garbage collection.

     NOTE: garbage collection occurs automatically whenever  internal  free
           lists are exhausted.


(gcafter s_type)

     WHERE:   s_type is one of the FRANZ LISP data types listed in 1.3.

     NOTE: this function is called by the garbage collector after a garbage
           collection  which was caused by running out of data type s_type.
           This function should determine if more space need  be  allocated
           and  if so should allocate it.  There is a default gcafter func-
           tion but users who want control over space allocation can define
           their own -- but note that it must be an nlambda.


(getenv 's_name)

     RETURNS: a symbol whose pname is the value of s_name  in  the  current
              UNIX  environment.   If  s_name  doesn't exist in the current
              environment, a symbol with a null pname is returned.










9

9                                                  Printed: October 22, 1980







System Functions                                                        6-4


(hashtabstat)

     RETURNS: a list of fixnums representing the number of symbols in  each
              bucket of the oblist.

     NOTE: the oblist is stored a hash table  of  buckets.   Ideally  there
           would be the same number of symbols in each bucket.


(include s_filename)

     RETURNS: nil

     SIDE EFFECT: The given filename is _l_o_a_ded into the lisp.

     NOTE: this is similar to load except the argument  is  not  evaluated.
           Include means something special to the compiler.


(includef 's_filename)

     RETURNS: nil

     SIDE EFFECT: this is the  same  as  _i_n_c_l_u_d_e  except  the  argument  is
                  evaluated.


(maknum 'g_arg)

     RETURNS: the address of its argument converted into a fixnum.


(opval 's_arg ['g_newval])

     RETURNS: the value associated with s_arg before the call.

     SIDE EFFECT: If g_newval is specified, the value associated with s_arg
                  is changed to g_newval.

     NOTE: _o_p_v_a_l keeps track of storage allocation. If s_arg is one of  the
           data  types  then  _o_p_v_a_l  will  return  a  list of three fixnums
           representing the number of items of that type in use, the number
           of  pages  allocated  and  the  number of items of that type per
           page. You should never try to change the value _o_p_v_a_l  associates
           with a data type using _o_p_v_a_l.
           If s_arg is  _p_a_g_e_l_i_m_i_t  then  _o_p_v_a_l  will  return  (and  set  if
           g_newval is given) the maximum amount of lisp data pages it will
           allocate.  This limit should remain small unless you  know  your
           program requires lots of space as this limit will catch programs
           in infinite loops which gobble up memory.


9

9                                                  Printed: October 22, 1980







System Functions                                                        6-5


(process s_pgrm [s_frompipe s_topipe])

     RETURNS: if the optional arguments are not present a fixnum  which  is
              the  exit  code  when s_prgm dies.  If the optional arguments
              are present, it returns a fixnum which is the process  id  of
              the child.

     SIDE EFFECT: If s_frompipe and s_topipe are given, they are  bound  to
                  ports  which are pipes which direct characters from FRANZ
                  LISP to the new process and to FRANZ LISP  from  the  new
                  process  respectively.  this forks a process named s_prgm
                  and waits for it to die iff there are no  pipe  arguments
                  given.


(ptime)

     RETURNS: a list of two elements, the first is the amount of  processor
              time used by the lisp system so far, the second is the amount
              of time used by the garbage collector so far.

     NOTE: the first number includes the second number.  The amount of time
           used  by garbage collection is not recorded until the first call
           to ptime.  This is done to prevent overhead when the user is not
           interested garbage collection times.


(reset)

     SIDE EFFECT: the lisp runtime stack is cleared and the system restarts
                  at the top level by executing a (_f_u_n_c_a_l_l _t_o_p-_l_e_v_e_l _n_i_l).


(retbrk ['x_level])

     WHERE:   x_level is a small integer of either sign.

     SIDE EFFECT: The default error handler keeps a notion of  the  current
                  level  of the error caught.  If x_level is negative, con-
                  trol is thrown to this default error handler whose  level
                  is  that  many  less than the present, or to _t_o_p-_l_e_v_e_l if
                  there aren't enough.  If x_level is non-negative, control
                  is  passed  to  the handler at that level.  If x_level is
                  not present, the value -1 is taken by default.








9

9                                                  Printed: October 22, 1980







System Functions                                                        6-6


(segment 's_type 'x_size)

     WHERE:   s_type is one of the data types given in 1.3

     RETURNS: a segment of contiguous lispvals of type s_type.

     NOTE: In reality, _s_e_g_m_e_n_t returns a new data cell of type  s_type  and
           allocates  space  for  x_size  -  1 more s_type's beyond the one
           returned.  _S_e_g_m_e_n_t always allocates new space and does so in 512
           byte  chunks.   If  you ask for 2 fixnums, segment will actually
           allocate 128 of them thus wasting  126  fixnums.   The  function
           _s_m_a_l_l-_s_e_g_m_e_n_t  is  a  smarter space allocator and should be used
           whenever possible.


(shell)

     RETURNS: the exit code of the shell when it dies.

     SIDE EFFECT: this forks a new shell and returns when the shell dies.


(showstack)

     RETURNS: nil

     SIDE EFFECT: all forms currently in evaluation are printed,  beginning
                  with  the  most  recent.  For compiled code the most that
                  showstack will show is the function name and it  may  not
                  show all of them.


(signal 'x_signum 's_name)

     RETURNS: nil if no previous call to signal has been made, or the  pre-
              viously installed s_name.

     SIDE EFFECT: this declares that the function named s_name will  handle
                  the signal number x_signum.  If s_name is nil, the signal
                  is ignored.  Presently only four UNIX signals are caught,
                  they and their numbers are: Interrupt(2), Floating excep-
                  tion(8), Alarm(14), and Hang-up(1).










9

9                                                  Printed: October 22, 1980







System Functions                                                        6-7


(sizeof 'g_arg)

     RETURNS: the number of bytes required to  store  one  object  of  type
              g_arg, encoded as a fixnum.


(small-segment 's_type 'x_cells)

     WHERE:   s_type is one of fixnum, flonum and value.

     RETURNS: a segment of x_cells data objects of type s_type.

     SIDE EFFECT: This may call _s_e_g_m_e_n_t to allocate new space or it may  be
                  able  to  fill  the  request on a page already allocated.
                  The value returned by _s_m_a_l_l-_s_e_g_m_e_n_t is usually stored  in
                  the data subpart of an array object.


(sstatus g_type g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_type  is  not  one  of  the  special  sstatus  codes
                  described in the next few pages this simply sets g_val as
                  the value of status type g_type in the system status pro-
                  perty list.


(sstatus appendmap g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_val is non nil then when _f_a_s_l is told  to  create  a
                  load  map,  it  will append to the file name given in the
                  _f_a_s_l command, rather than creating a new map file.


(sstatus automatic-reset g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_val is non nil then when an error  occurs  which  no
                  one  wants  to  handle,  a  _r_e_s_e_t will be done instead of
                  entering a primitive internal break loop.








9

9                                                  Printed: October 22, 1980







System Functions                                                        6-8


(sstatus chainatom g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_val is non nil and a _c_a_r or _c_d_r of a symbol is done,
                  then  nil will be returned instead of an error being sig-
                  naled.  This only affects the interpreter, not  the  com-
                  piler.


(sstatus dumpcore g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_val is nil, FRANZ LISP tells UNIX that  a  segmenta-
                  tion violation or bus error should cause a core dump.  If
                  g_val is non nil then FRANZ LISP will catch those  errors
                  and print a message advising the user to reset.

     NOTE: The  default  value  for  this  flag  is  nil,  and  only  those
           knowledgeable  of the innards of the lisp system should ever set
           this flag non nil.


(sstatus dumpmode x_val)

     RETURNS: x_val

     SIDE EFFECT: All subsequent _d_u_m_p_l_i_s_p's will be  done  in  mode  x_val.
                  x_val may be either 413 or 410 (decimal).

     NOTE: the advantage of mode 413 is that the dumped Lisp can be  demand
           paged in when first started, which will make it start faster and
           disrupt other users less.


(sstatus feature g_val)

     RETURNS: g_val

     SIDE EFFECT: g_val is added to the (_s_t_a_t_u_s _f_e_a_t_u_r_e_s) list,











9

9                                                  Printed: October 22, 1980







System Functions                                                        6-9


(sstatus ignoreeof g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_val is non nil then if a end  of  file  (CNTL  D  on
                  UNIX)  is  typed  to the top level interpreter it will be
                  ignored rather then cause the lisp system  to  exit.   If
                  the  the  standard input is  a file or pipe then this has
                  no effect, a EOF will always cause lisp to exit.


(sstatus nofeature g_val)

     RETURNS: g_val

     SIDE EFFECT: g_val is removed from the status features list if it  was
                  present.


(sstatus translink g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_val is  nil then all transfer tables are cleared and
                  further  calls  through the transfer table will not cause
                  the fast links to be set up.  If g_val is the  symbol  _o_n
                  then  all  possible transfer table entries will be linked
                  and the flag will be set to cause fast links to be set up
                  dynamically.   Otherwise  all  that is done is to set the
                  flag to cause fast links to be set up dynamically.

     NOTE: For a discussion of transfer tables, see the Section on the com-
           piler.


(sstatus uctolc g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_val is not nil then all unescaped capital letters in
                  symbols  read  by  the  reader will be converted to lower
                  case.

     NOTE: This allows FRANZ LISP to be compatible with  single  case  lisp
           systems (e.g. MAClisp).







9

9                                                  Printed: October 22, 1980







System Functions                                                       6-10


(status g_code)

     RETURNS: the value associated with the status code g_code if g_code is
              not one of the special cases given below


(status ctime)

     RETURNS: a symbol whose print name is the current time and date.

     EXAMPLE: (_s_t_a_t_u_s _c_t_i_m_e) ==> |Sun Jun 29 16:51:26 1980|


(status feature g_val)

     RETURNS: t iff g_val is in the status features list.


(status features)

     RETURNS: the value of the features code, which is a list  of  features
              which  are present in this system.  You add to this list with
              (_s_s_t_a_t_u_s _f_e_a_t_u_r_e '_g__v_a_l)  and  test  if  feature  g_feat   is
              present with (_s_t_a_t_u_s _f_e_a_t_u_r_e '_g__f_e_a_t).


(status isatty)

     RETURNS: t iff the standard input is a terminal.


(status localtime)

     RETURNS: a list of fixnums representing the current time as  described
              in the UNIX manual under LOCALTIME(3).

     EXAMPLE: (_s_t_a_t_u_s _l_o_c_a_l_t_i_m_e) ==>  (20 51 16 29 5 80 0 1 nil)


(status syntax s_char)

     RETURNS: a fixnum which is the syntax code associated with the charac-
              ter s_char in the current readtable.

     NOTE: You cannot set the syntax code  with  with  (_s_s_t_a_t_u_s _s_y_n_t_a_x _c_h),
           you must use _s_e_t_s_y_n_t_a_x.






9

9                                                  Printed: October 22, 1980







System Functions                                                       6-11


(status undeffunc)

     RETURNS: a list of all functions which transfer table entries point to
              but which are not defined at this point.

     NOTE: Some of the undefined functions listed  could  be  arrays  which
           have yet to be created.


(status version)

     RETURNS: a string which is the current lisp version name.

     EXAMPLE: (_s_t_a_t_u_s _v_e_r_s_i_o_n) ==> "Franz Lisp, Opus 33b"


(syscall 'x_index ['xst_arg1 ...])

     RETURNS: the result of issuing the UNIX  system  call  number  x_index
              with arguments xst_arg_i.

     NOTE: The UNIX system calls are described in section  2  of  the  UNIX
           manual.  If xst_arg_i is a fixnum, then its value is passed as an
           argument, if it is a symbol then its pname is passed and finally
           if  it  is a string then the string itself is passed as an argu-
           ment.  Some useful syscalls are:
           (_s_y_s_c_a_l_l _2_0) returns process id.
           (_s_y_s_c_a_l_l _1_3) returns the number of seconds since Jan 1, 1970.
           (_s_y_s_c_a_l_l _1_0 '_f_o_o) will unlink (delete) the file foo.


(top-level)

     RETURNS: nothing (it never returns)

     NOTE: This function is the top-level read-eval-print loop.   It  never
           returns any value.  Its main utility is that if you redefine it,
           and do a (reset) then the redefined (top-level) is then invoked.














9

9                                                  Printed: October 22, 1980



