






                         CHAPTER  6


                      System Functions




     This chapter describes the functions used  to  interact
with  internal  components  of the Lisp system and operating
system.

(allocate 's_type 'x_pages)

     WHERE:   s_type is one of the  FRANZ  LISP  data  types
              described in 1.3.

     RETURNS: x_pages.

     SIDE EFFECT: FRANZ LISP attempts to allocate x_pages of
                  type  s_type.   If there aren't x_pages of
                  memory left, no space  will  be  allocated
                  and an error will occur.  The storage that
                  is allocated is not given to  the  caller,
                  instead  it  is  added to the free storage
                  list of s_type.  The functions _s_e_g_m_e_n_t and
                  _s_m_a_l_l-_s_e_g_m_e_n_t  allocate blocks  of storage
                  and return it to the caller.

(argv 'x_argnumb)

     RETURNS: a symbol whose pname is the x_argnumb_t_h  argu-
              ment (starting at 0) on the command line which
              invoked the current lisp.

     NOTE: if x_argnumb is less than zero,  a  fixnum  whose
           value  is  the number of arguments on the command
           line is returned.  (_a_r_g_v _0) returns the  name  of
           the lisp you are running.

(baktrace)

     RETURNS: nil

     SIDE EFFECT: the lisp runtime stack is examined and the
                  name  of (most) of the functions currently
                  in  execution  are  printed,  most  active
                  first.

     NOTE: this will occasionally miss the names of compiled
           lisp  functions  due to incomplete information on
           the stack.  If you  are  tracing  compiled  code,
           then  _b_a_k_t_r_a_c_e  won't  be  able  to interpret the


System Functions                                         6-1







System Functions                                         6-2


           stack unless  (_s_s_t_a_t_u_s _t_r_a_n_s_l_i_n_k _n_i_l)  was  done.
           See  the  function  _s_h_o_w_s_t_a_c_k  for another way of
           printing the lisp runtime stack.   This  misspel-
           ling is from Maclisp.

(chdir 's_path)

     RETURNS: t iff the system call succeeds.

     SIDE EFFECT: the current directory set to s_path. Among
                  other things, this will affect the default
                  location where the input/output  functions
                  look for and create files.

     NOTE: _c_h_d_i_r follows the standard UNIX  conventions,  if
           s_path  does  not begin with a slash, the default
           path is changed to the current path  with  s_path
           appended.   _C_h_d_i_r  employs  tilde-expansion (dis-
           cussed in Chapter 5).

(command-line-args)

     RETURNS: a list of the arguments typed on  the  command
              line, either to the lisp interpreter, or saved
              lisp dump, or application  compiled  with  the
              autorun option (liszt -r).

(deref 'x_addr)

     RETURNS: The contents of x_addr, when thought of  as  a
              longword memory location.

     NOTE: This may be useful in constructing arguments to C
           functions out of `dangerous' areas of memory.

(dumplisp s_name)

     RETURNS: nil

     SIDE EFFECT: the current lisp is dumped  to  the  named
                  file.   When  s_name is executed, you will
                  be in a lisp in the same state as when the
                  dumplisp was done.

     NOTE: dumplisp will fail if one tries to write over the
           current  running file. UNIX does not allow you to
           modify the file you are running.





9

9                                   Printed: January 31, 1984







System Functions                                         6-3


(eval-when l_time g_exp1 ...)

     SIDE EFFECT: l_time may contain any combination of  the
                  symbols  _l_o_a_d,  _e_v_a_l,  and  _c_o_m_p_i_l_e.   The
                  effects of load and compile  is  discussed
                  in  12.3.2.1 compiler.  If eval is present
                  however,  this  simply  means   that   the
                  expressions g_exp1 and so on are evaluated
                  from  left  to  right.   If  eval  is  not
                  present, the forms are not evaluated.

(exit ['x_code])

     RETURNS: nothing (it never returns).

     SIDE EFFECT: the lisp system dies with exit code x_code
                  or 0 if x_code is not specified.

(fake 'x_addr)

     RETURNS: the lisp object at address x_addr.

     NOTE: This is intended to be used by  people  debugging
           the lisp system.

(fork)

     RETURNS: nil to  the  child  process  and  the  process
              number of the child to the parent.

     SIDE EFFECT: A copy of the current lisp system is  made
                  in  memory and both lisp systems now begin
                  to  run.   This  function  can   be   used
                  interactively   to  temporarily  save  the
                  state of Lisp (as shown  below),  but  you
                  must  be  careful  that  only  one  of the
                  lisp's interacts with the  terminal  after
                  the fork.  The _w_a_i_t function is useful for
                  this.













9

9                                   Printed: January 31, 1984







System Functions                                         6-4



    ____________________________________________________

    -> (_s_e_t_q _f_o_o '_b_a_r)              ;; set a variable
    bar
    -> (_c_o_n_d ((_f_o_r_k)(_w_a_i_t)))        ;; duplicate the lisp system and
    nil                             ;; make the parent wait
    -> _f_o_o                          ;; check the value of the variable
    bar
    -> (_s_e_t_q _f_o_o '_b_a_z)              ;; give it a new value
    baz
    -> _f_o_o                          ;; make sure it worked
    baz
    -> (_e_x_i_t)                       ;; exit the child
    (5274 . 0)                      ;; the _w_a_i_t function returns this
    -> _f_o_o                          ;; we check to make sure parent was
    bar                             ;; not modified.
    ____________________________________________________




(gc)

     RETURNS: nil

     SIDE EFFECT: this causes a garbage collection.

     NOTE: The function _g_c_a_f_t_e_r is not called  automatically
           after  this function finishes.  Normally the user
           doesn't have to call _g_c since garbage  collection
           occurs automatically whenever internal free lists
           are exhausted.

(gcafter s_type)

     WHERE:   s_type is one of the  FRANZ  LISP  data  types
              listed in 1.3.

     NOTE: this function is called by the garbage  collector
           after  a  garbage  collection which was caused by
           running out of data type s_type.   This  function
           should  determine if more space need be allocated
           and if so should allocate it.  There is a default
           gcafter  function but users who want control over
           space allocation can define their own -- but note
           that it must be an nlambda.





9

9                                   Printed: January 31, 1984







System Functions                                         6-5


(getenv 's_name)

     RETURNS: a symbol whose pname is the value of s_name in
              the   current  UNIX  environment.   If  s_name
              doesn't exist in the  current  environment,  a
              symbol with a null pname is returned.

(hashtabstat)

     RETURNS: a list of fixnums representing the  number  of
              symbols in each bucket of the oblist.

     NOTE: the oblist is stored a  hash  table  of  buckets.
           Ideally there would be the same number of symbols
           in each bucket.

(help [sx_arg])

     SIDE EFFECT: If sx_arg is a symbol then the portion  of
                  this manual beginning with the description
                  of sx_arg is printed on the terminal.   If
                  sx_arg  is  a fixnum or the name of one of
                  the appendicies, that chapter or  appendix
                  is  printed  on the terminal.  If no argu-
                  ment is provided, _h_e_l_p prints the  options
                  that it recognizes.  The program `more' is
                  used to print the manual on the  terminal;
                  it will stop after each page and will con-
                  tinue after the space key is pressed.

(include s_filename)

     RETURNS: nil

     SIDE EFFECT: The given  filename  is  _l_o_a_ded  into  the
                  lisp.

     NOTE: this is similar to load except  the  argument  is
           not  evaluated.   Include means something special
           to the compiler.

(include-if 'g_predicate s_filename)

     RETURNS: nil

     SIDE EFFECT: This has the same effect as  include,  but
                  is  only actuated if the predicate is non-
                  nil.




9

9                                   Printed: January 31, 1984







System Functions                                         6-6


(includef 's_filename)

     RETURNS: nil

     SIDE EFFECT: this is the same  as  _i_n_c_l_u_d_e  except  the
                  argument is evaluated.

(includef-if 'g_predicate s_filename)

     RETURNS: nil

     SIDE EFFECT: This has the same effect as includef,  but
                  is  only actuated if the predicate is non-
                  nil.

(maknum 'g_arg)

     RETURNS: the address of its argument converted  into  a
              fixnum.

(monitor ['xs_maxaddr])

     RETURNS: t

     SIDE EFFECT: If xs_maxaddr is t then profiling  of  the
                  entire   lisp   system   is   begun.    If
                  xs_maxaddr is a fixnum then  profiling  is
                  done  only  up  to address xs_maxaddr.  If
                  xs_maxaddr is not given, then profiling is
                  stopped  and  the data obtained is written
                  to the file  'mon.out'  where  it  can  be
                  analyzed with the UNIX 'prof' program.

     NOTE: this function only works if the lisp  system  has
           been  compiled  in  a  special way, otherwise, an
           error is invoked.

(opval 's_arg ['g_newval])

     RETURNS: the value associated  with  s_arg  before  the
              call.

     SIDE EFFECT: If g_newval is specified, the value  asso-
                  ciated with s_arg is changed to g_newval.

     NOTE: _o_p_v_a_l keeps track of storage allocation. If s_arg
           is one of the data types then _o_p_v_a_l will return a
           list of three fixnums representing the number  of
           items  of  that  type in use, the number of pages
           allocated and the number of items  of  that  type
           per  page.  You  should  never  try to change the
           value _o_p_v_a_l associates with  a  data  type  using
           _o_p_v_a_l.


                                   Printed: January 31, 1984







System Functions                                         6-7


           If s_arg is _p_a_g_e_l_i_m_i_t then _o_p_v_a_l will return (and
           set  if  g_newval is given) the maximum amount of
           lisp data pages it  will  allocate.   This  limit
           should  remain small unless you know your program
           requires lots of space as this limit  will  catch
           programs   in  infinite  loops  which  gobble  up
           memory.

(*process 'st_command ['g_readp ['g_writep]])

     RETURNS: either a fixnum if one argument is given, or a
              list of two ports and a fixnum if two or three
              arguments are given.

     NOTE: *_p_r_o_c_e_s_s  starts  another  process   by   passing
           st_command to the shell (it first tries /bin/csh,
           then it tries /bin/sh if /bin/csh doesn't exist).
           If  only one argument is given to *_p_r_o_c_e_s_s, *_p_r_o_-
           _c_e_s_s waits for the new process to  die  and  then
           returns  the  exit  code  of the new process.  If
           more two or three arguments are  given,  *_p_r_o_c_e_s_s
           starts the process and then returns a list which,
           depending on the value of g_readp  and  g_writep,
           may  contain  i/o ports for communcating with the
           new process.  If g_writep  is  non-null,  then  a
           port  will  be created which the lisp program can
           use to send characters to the  new  process.   If
           g_readp  is non-null, then a port will be created
           which the lisp program can use to read characters
           from  the  new  process.   The  value returned by
           *_p_r_o_c_e_s_s is (readport writeport pid) where  read-
           port and writeport are either nil or a port based
           on the value of g_readp and g_writep.  Pid is the
           process  id of the new process.  Since it is hard
           to remember the order of  g_readp  and  g_writep,
           the  functions *_p_r_o_c_e_s_s-_s_e_n_d and *_p_r_o_c_e_s_s-_r_e_c_e_i_v_e
           were written to perform the common functions.

(*process-receive 'st_command)

     RETURNS: a port which can be read.

     SIDE EFFECT: The command st_command  is  given  to  the
                  shell  and  it  is  started running in the
                  background.  The output of that command is
                  available   for   reading   via  the  port
                  returned.  The input of the  command  pro-
                  cess is set to /dev/null.




9

9                                   Printed: January 31, 1984







System Functions                                         6-8


(*process-send 'st_command)

     RETURNS: a port which can be written to.

     SIDE EFFECT: The command st_command  is  given  to  the
                  shell  and  it  is  started  runing in the
                  background.  The lisp program can  provide
                  input  for that command by sending charac-
                  ters to the port returned  by  this  func-
                  tion.   The  output of the command process
                  is set to /dev/null.

(process s_pgrm [s_frompipe s_topipe])

     RETURNS: if the optional arguments are  not  present  a
              fixnum  which  is  the  exit  code when s_prgm
              dies.  If the optional arguments are  present,
              it returns a fixnum which is the process id of
              the child.

     NOTE: This command is obsolete.   New  programs  should
           use one of the *_p_r_o_c_e_s_s commands given above.

     SIDE EFFECT: If s_frompipe and s_topipe are given, they
                  are  bound  to ports which are pipes which
                  direct characters from FRANZ LISP  to  the
                  new process and to FRANZ LISP from the new
                  process  respectively.   _P_r_o_c_e_s_s  forks  a
                  process  named  s_prgm and waits for it to
                  die iff there are no pipe arguments given.

(ptime)

     RETURNS: a list of two  elements.   The  first  is  the
              amount of processor time used by the lisp sys-
              tem so far, and the second is  the  amount  of
              time used by the garbage collector so far.

     NOTE: the time is measured in those units used  by  the
           _t_i_m_e_s(2)  system call, usually 60_t_hs of a second.
           The first number includes the second number.  The
           amount  of time used by garbage collection is not
           recorded until the first call to ptime.  This  is
           done  to  prevent  overhead  when the user is not
           interested in garbage collection times.







9

9                                   Printed: January 31, 1984







System Functions                                         6-9


(reset)

     SIDE EFFECT: the lisp runtime stack is cleared and  the
                  system  restarts  at the top level by exe-
                  cuting a (_f_u_n_c_a_l_l _t_o_p-_l_e_v_e_l _n_i_l).

(restorelisp 's_name)

     SIDE EFFECT: this  reads  in  file  s_name  (which  was
                  created  by  _s_a_v_e_l_i_s_p)  and  then  does  a
                  (_r_e_s_e_t).

     NOTE: This is only used on VMS systems  where  _d_u_m_p_l_i_s_p
           cannot be used.

(retbrk ['x_level])

     WHERE:   x_level is a small integer of either sign.

     SIDE EFFECT: The default error handler keeps  a  notion
                  of  the current level of the error caught.
                  If x_level is negative, control is  thrown
                  to  this default error handler whose level
                  is that many less than the present, or  to
                  _t_o_p-_l_e_v_e_l  if  there  aren't  enough.   If
                  x_level is non-negative, control is passed
                  to  the handler at that level.  If x_level
                  is not present, the value -1 is  taken  by
                  default.

(*rset 'g_flag)

     RETURNS: g_flag

     SIDE EFFECT: If g_flag is non nil then the lisp  system
                  will   maintain  extra  information  about
                  calls to _e_v_a_l and  _f_u_n_c_a_l_l.   This  record
                  keeping slows down the evaluation but this
                  is required  for the  functions  _e_v_a_l_h_o_o_k,
                  _f_u_n_c_a_l_l_h_o_o_k,  and  _e_v_a_l_f_r_a_m_e  to  work. To
                  debug  compiled  lisp  code  the  transfer
                  tables       should      be      unlinked:
                  (_s_s_t_a_t_u_s _t_r_a_n_s_l_i_n_k _n_i_l)









9

9                                   Printed: January 31, 1984







System Functions                                        6-10


(savelisp 's_name)

     RETURNS: t

     SIDE EFFECT: the state of the Lisp system is  saved  in
                  the  file  s_name.   It  can be read in by
                  _r_e_s_t_o_r_e_l_i_s_p.

     NOTE: This is only used on VMS systems  where  _d_u_m_p_l_i_s_p
           cannot be used.

(segment 's_type 'x_size)

     WHERE:   s_type is one of the data types given in 1.3

     RETURNS: a  segment  of  contiguous  lispvals  of  type
              s_type.

     NOTE: In reality, _s_e_g_m_e_n_t returns a new  data  cell  of
           type  s_type  and  allocates space for x_size - 1
           more s_type's beyond the one  returned.   _S_e_g_m_e_n_t
           always  allocates  new  space  and does so in 512
           byte chunks.  If you ask for 2  fixnums,  segment
           will  actually  allocate 128 of them thus wasting
           126 fixnums.  The  function  _s_m_a_l_l-_s_e_g_m_e_n_t  is  a
           smarter  space allocator and should be used when-
           ever possible.

(shell)

     RETURNS: the exit code of the shell when it dies.

     SIDE EFFECT: this forks a new shell  and  returns  when
                  the shell dies.

(showstack)

     RETURNS: nil

     SIDE EFFECT: all  forms  currently  in  evaluation  are
                  printed,  beginning  with the most recent.
                  For compiled code the most that  showstack
                  will  show is the function name and it may
                  miss some functions.








9

9                                   Printed: January 31, 1984







System Functions                                        6-11


(signal 'x_signum 's_name)

     RETURNS: nil if no previous call  to  signal  has  been
              made, or the previously installed s_name.

     SIDE EFFECT: this  declares  that  the  function  named
                  s_name   will  handle  the  signal  number
                  x_signum.  If s_name is nil, the signal is
                  ignored.  Presently only four UNIX signals
                  are caught.  They and their  numbers  are:
                  Interrupt(2),    Floating    exception(8),
                  Alarm(14), and Hang-up(1).

(sizeof 'g_arg)

     RETURNS: the number of  bytes  required  to  store  one
              object of type g_arg, encoded as a fixnum.

(small-segment 's_type 'x_cells)

     WHERE:   s_type is one of fixnum, flonum and value.

     RETURNS: a segment of  x_cells  data  objects  of  type
              s_type.

     SIDE EFFECT: This may  call  _s_e_g_m_e_n_t  to  allocate  new
                  space  or  it  may  be  able  to  fill the
                  request on a page already allocated.   The
                  value returned by _s_m_a_l_l-_s_e_g_m_e_n_t is usually
                  stored in the data  subpart  of  an  array
                  object.

(sstatus g_type g_val)

     RETURNS: g_val

     SIDE EFFECT: If  g_type  is  not  one  of  the  special
                  sstatus  codes  described  in the next few
                  pages this simply sets g_val as the  value
                  of status type g_type in the system status
                  property list.











9

9                                   Printed: January 31, 1984







System Functions                                        6-12


(sstatus appendmap g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_val is non-null when _f_a_s_l is told  to
                  create  a  load map, it will append to the
                  file  name  given  in  the  _f_a_s_l  command,
                  rather  than creating a new map file.  The
                  initial value is nil.

(sstatus automatic-reset g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_val is non-null when an error  occurs
                  which no one wants to handle, a _r_e_s_e_t will
                  be done instead of  entering  a  primitive
                  internal break loop.  The initial value is
                  t.

(sstatus chainatom g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_val is non nil and a _c_a_r or _c_d_r of  a
                  symbol  is done, then nil will be returned
                  instead of an error being signaled.   This
                  only affects the interpreter, not the com-
                  piler.  The initial value is nil.

(sstatus dumpcore g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_val is nil,  FRANZ  LISP  tells  UNIX
                  that a segmentation violation or bus error
                  should cause a core dump.  If g_val is non
                  nil  then  FRANZ  LISP  will  catch  those
                  errors and print a  message  advising  the
                  user to reset.

     NOTE: The initial value for this flag is nil, and  only
           those  knowledgeable  of  the innards of the lisp
           system should ever set this flag non nil.








9

9                                   Printed: January 31, 1984







System Functions                                        6-13


(sstatus dumpmode x_val)

     RETURNS: x_val

     SIDE EFFECT: All subsequent _d_u_m_p_l_i_s_p's will be done  in
                  mode  x_val.   x_val  may be either 413 or
                  410 (decimal).

     NOTE: the advantage of mode 413 is that the dumped Lisp
           can  be demand paged in when first started, which
           will make it start faster and disrupt other users
           less.  The initial value is 413.

(sstatus evalhook g_val)

     RETURNS: g_val

     SIDE EFFECT: When g_val is non nil,  this  enables  the
                  evalhook  and  funcallhook  traps  in  the
                  evaluator.  See 14.4 for more details.

(sstatus feature g_val)

     RETURNS: g_val

     SIDE EFFECT: g_val is added  to  the  (_s_t_a_t_u_s _f_e_a_t_u_r_e_s)
                  list,

(sstatus gcstrings g_val)

     RETURNS: g_val

     SIDE EFFECT: if g_val is non-null, and if  string  gar-
                  bage  collection was enabled when the lisp
                  system was compiled, string space will  be
                  garbage collected.

     NOTE: the default value for this is nil since  in  most
           applications  garbage  collecting  strings  is  a
           waste of time.

(sstatus ignoreeof g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_val is non-null when an end  of  file
                  (CNTL-D  on UNIX) is typed to the standard
                  top-level interpreter, it will be  ignored
                  rather then cause the lisp system to exit.
                  If the the standard input  is  a  file  or
                  pipe  then this has no effect, an EOF will
                  always cause lisp to  exit.   The  initial
                  value is nil.


                                   Printed: January 31, 1984







System Functions                                        6-14


(sstatus nofeature g_val)

     RETURNS: g_val

     SIDE EFFECT: g_val is removed from the status  features
                  list if it was present.

(sstatus translink g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_val is nil then all  transfer  tables
                  are  cleared and further calls through the
                  transfer table will  not  cause  the  fast
                  links  to be set up.  If g_val is the sym-
                  bol _o_n then all  possible  transfer  table
                  entries  will  be linked and the flag will
                  be set to cause fast links to  be  set  up
                  dynamically.   Otherwise  all that is done
                  is to set the flag to cause fast links  to
                  be  set up dynamically.  The initial value
                  is nil.

     NOTE: For a discussion of transfer tables, see 12.8.

(sstatus uctolc g_val)

     RETURNS: g_val

     SIDE EFFECT: If g_val is not  nil  then  all  unescaped
                  capital  letters  in  symbols  read by the
                  reader will be converted to lower case.

     NOTE: This allows FRANZ LISP to be compatible with sin-
           gle  case  lisp  systems (e.g. Maclisp, Interlisp
           and UCILisp).

(status g_code)

     RETURNS: the value  associated  with  the  status  code
              g_code  if  g_code  is  not one of the special
              cases given below










9

9                                   Printed: January 31, 1984







System Functions                                        6-15


(status ctime)

     RETURNS: a symbol whose print name is the current  time
              and date.

     EXAMPLE: (_s_t_a_t_u_s _c_t_i_m_e) = |Sun Jun 29 16:51:26 1980|

     NOTE: This  has  been  made  obsolete  by  _t_i_m_e-_s_t_r_i_n_g,
           described below.

(status feature g_val)

     RETURNS: t iff g_val is in the status features list.

(status features)

     RETURNS: the value of the features  code,  which  is  a
              list  of  features  which  are present in this
              system.    You   add   to   this   list   with
              (_s_s_t_a_t_u_s _f_e_a_t_u_r_e '_g__v_a_l)  and  test if feature
              g_feat         is         present         with
              (_s_t_a_t_u_s _f_e_a_t_u_r_e '_g__f_e_a_t).

(status isatty)

     RETURNS: t iff the standard input is a terminal.

(status localtime)

     RETURNS: a list of  fixnums  representing  the  current
              time.

     EXAMPLE: (_s_t_a_t_u_s _l_o_c_a_l_t_i_m_e) =  (3 51 13 31 6 81  5  211
              1)
              means 3_r_d second, 51_s_t minute,  13_t_h  hour  (1
              p.m), 31_s_t day, month 6 (0 = January), year 81
              (0 = 1900), day of the  week  5  (0 = Sunday),
              211_t_h  day  of  the  year and daylight savings
              time is in effect.

(status syntax s_char)

     NOTE: This  function  should  not  be  used.   See  the
           description  of  _g_e_t_s_y_n_t_a_x  (in  Chapter 7) for a
           replacement.







9

9                                   Printed: January 31, 1984







System Functions                                        6-16


(status undeffunc)

     RETURNS: a list of all functions which  transfer  table
              entries  point to but which are not defined at
              this point.

     NOTE: Some of the undefined functions listed  could  be
           arrays which have yet to be created.

(status version)

     RETURNS: a string which is  the  current  lisp  version
              name.

     EXAMPLE: (_s_t_a_t_u_s _v_e_r_s_i_o_n) = "Franz Lisp, Opus 38.61"

(syscall 'x_index ['xst_arg1 ...])

     RETURNS: the result of issuing  the  UNIX  system  call
              number x_index with arguments xst_arg_i.

     NOTE: The UNIX system calls are described in section  2
           of the UNIX Programmer's manual. If xst_arg_i is a
           fixnum, then its value is passed as an  argument,
           if  it  is  a symbol then its pname is passed and
           finally if it is a string then the string  itself
           is  passed  as an argument.  Some useful syscalls
           are:
           (_s_y_s_c_a_l_l _2_0) returns process id.
           (_s_y_s_c_a_l_l _1_3) returns the number of seconds  since
           Jan 1, 1970.
           (_s_y_s_c_a_l_l _1_0 '_f_o_o) will unlink (delete)  the  file
           foo.

(sys:access 'st_filename 'x_mode)
(sys:chmod 'st_filename 'x_mode)
(sys:gethostname)
(sys:getpid)
(sys:getpwnam 'st_username)
(sys:link 'st_oldfilename 'st_newfilename)
(sys:time)
(sys:unlink 'st_filename)

     NOTE: We have been warned that the actual  system  call
           numbers  may  vary  among different UNIX systems.
           Users concerned about portability may wish to use
           this  group  of  functions.  Another advantage is
           that tilde-expansion is performed on all filename
           arguments.   These functions do what is described
           in the system call section of your UNIX manual.

           _s_y_s:_g_e_t_p_w_n_a_m returns a  vector  of  four  entries
           from the password file, being the user name, user


                                   Printed: January 31, 1984







System Functions                                        6-17


           id, group id, and home directory.

(time-string ['x_seconds])

     RETURNS: an ascii string giving the time and date which
              was  x_seconds  after  UNIX's idea of creation
              (Midnight, Jan 1, 1970 GMT).  If  no  argument
              is  given,  time-string  returns  the  current
              date.  This supplants (_s_t_a_t_u_s _c_t_i_m_e), and  may
              be  used  to make the results of _f_i_l_e_s_t_a_t more
              intelligible.

(top-level)

     RETURNS: nothing (it never returns)

     NOTE: This function is  the  top-level  read-eval-print
           loop.   It  never  returns  any  value.  Its main
           utility is that if you  redefine  it,  and  do  a
           (reset)  then  the  redefined (top-level) is then
           invoked.  The default top-level for Franz,  allow
           one  to  specify  his  own  printer or reader, by
           binding the symbols  top-level-printer  and  top-
           level-reader.   One can let the default top-level
           do most of the drudgery in catching _r_e_s_e_t's,  and
           reading  in  .lisprc files, by binding the symbol
           user-top-level, to a routine that concerns itself
           only with the read-eval-print loop.

(wait)

     RETURNS: a dotted pair (_p_r_o_c_e_s_s_i_d .  _s_t_a_t_u_s)  when  the
              next child process dies.



















9

9                                   Printed: January 31, 1984



