






                         CHAPTER  5


                        Input/Output




     The following functions are used to read from and write
to  external  devices  (e.g.  files)  and  programs (through
pipes).  All I/O goes through the lisp data type called  the
port.  A port may be open for either reading or writing, but
usually not both simultaneously (see _f_i_l_e_o_p_e_n ).  There  are
only  a  limited  number  of ports (20) and they will not be
reclaimed unless they are _c_l_o_s_ed.  All ports  are  reclaimed
by  a _r_e_s_e_t_i_o call, but this drastic step won't be necessary
if the program closes what it uses.

     If a port argument is not supplied to a function  which
requires  one,  or  if  a bad port argument (such as nil) is
given, then FRANZ LISP will use the default  port  according
to this scheme: If input is being done then the default port
is the value of the symbol piport and  if  output  is  being
done  then  the  default  port  is  the  value of the symbol
poport.  Furthermore, if the value of piport  or  poport  is
not a valid port, then the standard input or standard output
will be used, respectively.

     The standard input and standard output are usually  the
keyboard  and terminal display unless your job is running in
the background and its input or output  is  connected  to  a
pipe.   All  output  which  goes to the standard output will
also go to the port ptport if it is a  valid  port.   Output
destined for the standard output will not reach the standard
output if the symbol ^w is non nil (although it  will  still
go to ptport if ptport is a valid port).

     Some of the  functions  listed  below  reference  files
directly.   FRANZ  LISP  has borrowed a convenient shorthand
notation from /_b_i_n/_c_s_h, concerning naming files.  If a  file
name begins with ~ (tilde), and the symbol tilde-expansion

is bound to  something  other  than  nil,  then  FRANZ  LISP
expands  the  file  name.  It takes the string of characters
between the leading tilde, and the first slash  as  a  user-
name.   Then,  that  initial  segment  of  the  filename  is
replaced by the home directory of the user.  The null  user-
name is taken to be the current user.

     FRANZ LISP keeps a cache of user home directory  infor-
mation,  to  minimize  searching  the password file.  Tilde-
expansion is performed in the  following  functions:  _c_f_a_s_l,
_c_h_d_i_r, _f_a_s_l, _f_f_a_s_l, _f_i_l_e_o_p_e_n, _i_n_f_i_l_e, _l_o_a_d, _o_u_t_f_i_l_e, _p_r_o_b_e_f,


Input/Output                                             5-1







Input/Output                                             5-2


_s_y_s:_a_c_c_e_s_s, _s_y_s:_u_n_l_i_n_k.

(cfasl    'st_file    'st_entry    'st_funcname    ['st_disc
['st_library]])

     RETURNS: t

     SIDE EFFECT: This is used to load in a foreign function
                  (see  8.4).   The  object  file st_file is
                  loaded into  the  lisp  system.   St_entry
                  should  be an entry point in the file just
                  loaded.  The function binding of the  sym-
                  bol  s_funcname  will  be  set to point to
                  st_entry, so that when the  lisp  function
                  s_funcname  is  called,  st_entry  will be
                  run.  st_disc  is  the  discipline  to  be
                  given  to  s_funcname. st_disc defaults to
                  "subroutine" if it is not given or  if  it
                  is  given  as  nil.  If st_library is non-
                  null, then after st_file  is  loaded,  the
                  libraries  given  in  st_library  will  be
                  searched to resolve  external  references.
                  The form of st_library should be something
                  like "-lm".  The C library  (" -lc " )  is
                  always  searched  so  when  loading in a C
                  file you probably won't need to specify  a
                  library.   For  Fortran  files, you should
                  specify "-lF77" and if you are  doing  any
                  I/O,  the  library  entry should be "-lI77
                  -lF77".   For  Pascal  files   "-lpc"   is
                  required.

     NOTE: This function may be used to load the  output  of
           the  assembler, C compiler, Fortran compiler, and
           Pascal compiler  but NOT the lisp  compiler  (use
           _f_a_s_l  for  that).   If  a  file has more than one
           entry point, then use _g_e_t_a_d_d_r_e_s_s  to  locate  and
           setup other foreign functions.
           It is an error to load in a file which has a glo-
           bal  entry  point  of  the  same name as a global
           entry point in the running lisp.  As soon as  you
           load  in  a  file  with  _c_f_a_s_l,  its global entry
           points become part of the  lisp's  entry  points.
           Thus  you  cannot  _c_f_a_s_l  in  the same file twice
           unless you use _r_e_m_o_v_e_a_d_d_r_e_s_s  to  change  certain
           global entry points to local entry points.






9

9                                   Printed: January 31, 1984







Input/Output                                             5-3


(close 'p_port)

     RETURNS: t

     SIDE EFFECT: the specified port is drained and  closed,
                  releasing the port.

     NOTE: The standard defaults are not used in  this  case
           since  you probably never want to close the stan-
           dard output or standard input.

(cprintf 'st_format 'xfst_val ['p_port])

     RETURNS: xfst_val

     SIDE EFFECT: The UNIX formatted output function  printf
                  is  called  with  arguments  st_format and
                  xfst_val.  If xfst_val is  a  symbol  then
                  its  print  name is passed to printf.  The
                  format string may contain characters which
                  are just printed literally and it may con-
                  tain special formatting commands  preceded
                  by  a  percent  sign.  The complete set of
                  formatting characters is described in  the
                  UNIX  manual.  Some useful ones are %d for
                  printing a fixnum in decimal, %f or %e for
                  printing  a  flonum, and %s for printing a
                  character string (or print name of a  sym-
                  bol).

     EXAMPLE: (_c_p_r_i_n_t_f "_P_i _e_q_u_a_l_s %_f"  _3._1_4_1_5_9)  prints  `Pi
              equals 3.14159'

(drain ['p_port])

     RETURNS: nil

     SIDE EFFECT: If this is an output port then the charac-
                  ters  in the output buffer are all sent to
                  the device.  If this is an input port then
                  all  pending  characters are flushed.  The
                  default port  for  this  function  is  the
                  default output port.









9

9                                   Printed: January 31, 1984







Input/Output                                             5-4


(ex [s_filename])
(vi [s_filename])
(exl [s_filename])
(vil [s_filename])

     RETURNS: nil

     SIDE EFFECT: The lisp system starts up an editor on the
                  file  named  as the argument.  It will try
                  appending .l to the file if it can't  find
                  it.   The  functions _e_x_l and _v_i_l will load
                  the file  after  you  finish  editing  it.
                  These  functions  will  also  remember the
                  name of the file  so  that  on  subsequent
                  invocations, you don't need to provide the
                  argument.

     NOTE: These functions do not evaluate their argument.

(fasl 'st_name ['st_mapf ['g_warn]])

     WHERE:   st_mapf and g_warn default to nil.

     RETURNS: t if the function succeeded, nil otherwise.

     SIDE EFFECT: this function is designed to  load  in  an
                  object file generated by the lisp compiler
                  Liszt.  File names for object  files  usu-
                  ally end in `.o', so _f_a_s_l will append `.o'
                  to st_name (if it is not already present).
                  If st_mapf is non nil, then it is the name
                  of the map file to create.  _F_a_s_l writes in
                  the  map  file  the names and addresses of
                  the functions it loads and defines.   Nor-
                  mally  the map file is created (i.e. trun-
                  cated    if    it    exists),    but    if
                  (_s_s_t_a_t_u_s _a_p_p_e_n_d_m_a_p _t) is done then the map
                  file will be appended.  If g_warn  is  non
                  nil  and if a function is loaded from  the
                  file which  is  already  defined,  then  a
                  warning message will be printed.

     NOTE: _f_a_s_l only looks in the current directory for  the
           file  to  load. The function _l_o_a_d looks through a
           user-supplied search path and will call  _f_a_s_l  if
           it  finds  a  file  with the same root name and a
           `.o' extension.  In  most cases the user would be
           better  off  using  the function _l_o_a_d rather than
           calling _f_a_s_l directly.



9

9                                   Printed: January 31, 1984







Input/Output                                             5-5


(ffasl  'st_file  'st_entry   'st_funcname   ['st_discipline
['st_library]])

     RETURNS: the binary object created.

     SIDE EFFECT: the Fortran object file st_file is  loaded
                  into  the lisp system.  St_entry should be
                  an entry point in the file just loaded.  A
                  binary  object  will  be  created  and its
                  entry  field  will  be  set  to  point  to
                  st_entry.   The  discipline  field  of the
                  binary will be  set  to  st_discipline  or
                  "subroutine" by default.  If st_library is
                  present and non-null, then  after  st_file
                  is   loaded,   the   libraries   given  in
                  st_library will  be  searched  to  resolve
                  external    references.    The   form   of
                  st_library should be something  like  "-lS
                  -ltermcap".   In  any  case,  the standard
                  Fortran libraries will be searched also to
                  resolve external references.

     NOTE: in F77 on Unix, the entry point for  the  fortran
           function foo is named `_foo_'.

(filepos 'p_port ['x_pos])

     RETURNS: the current position in the file if  x_pos  is
              not given or else x_pos if x_pos is given.

     SIDE EFFECT: If x_pos is given, the  next  byte  to  be
                  read  or  written  to  the port will be at
                  position x_pos.

(filestat 'st_filename)

     RETURNS: a vector containing various numbers which  the
              UNIX  operating  system  assigns to files.  if
              the file doesn't exist, an error  is  invoked.
              Use _p_r_o_b_e_f to determine if the file exists.

     NOTE: The individual entries can be accesed by mnemonic
           functions of the form filestat:_f_i_e_l_d, where field
           may be  any  of  atime,  ctime,  dev,  gid,  ino,
           mode,mtime,  nlink,  rdev,  size, type, uid.  See
           the UNIX programmers manual for a  more  detailed
           description of these quantities.





9

9                                   Printed: January 31, 1984







Input/Output                                             5-6


(flatc 'g_form ['x_max])

     RETURNS: the number of  characters  required  to  print
              g_form  using _p_a_t_o_m.  If x_max is given and if
              _f_l_a_t_c determines that it will return  a  value
              greater  than  x_max,  then  it  gives  up and
              returns the current  value  it  has  computed.
              This  is  useful if you just want to see if an
              expression is larger than a certain size.

(flatsize 'g_form ['x_max])

     RETURNS: the number of  characters  required  to  print
              g_form  using  _p_r_i_n_t.  The meaning of x_max is
              the same as for flatc.

     NOTE: Currently this just _e_x_p_l_o_d_e's g_form  and  checks
           its length.

(fileopen 'st_filename 'st_mode)

     RETURNS: a port for reading or  writing  (depending  on
              st_mode) the file st_name.

     SIDE EFFECT: the given file is opened  (or  created  if
                  opened  for  writing  and  it  doesn't yet
                  exist).

     NOTE: this function call provides a direct interface to
           the  operating system's fopen function.  The mode
           may be more than just "r" for read, "w" for write
           or "a" for append.  The modes "r+", "w+" and "a+"
           permit both reading and writing on  a  port  pro-
           vided  that  _f_s_e_e_k  is  done  between  changes in
           direction.  See the UNIX  manual  description  of
           fopen  for  more  details.  This routine does not
           look through a search path for a given file.

(fseek 'p_port 'x_offset 'x_flag)

     RETURNS: the position in the file after the function is
              performed.

     SIDE EFFECT: this  function  positions  the  read/write
                  pointer before a certain byte in the file.
                  If x_flag is 0 then the pointer is set  to
                  x_offset  bytes  from the beginning of the
                  file.  If x_flag is 1 then the pointer  is
                  set  to  x_offset  bytes  from the current
                  location in the file.  If x_flag is 2 then
                  the  pointer is set to x_offset bytes from
                  the end of the file.
9

9                                   Printed: January 31, 1984







Input/Output                                             5-7


(infile 's_filename)

     RETURNS: a port ready to read s_filename.

     SIDE EFFECT: this tries to open s_filename  and  if  it
                  cannot  or if there are no ports available
                  it gives an error message.

     NOTE: to allow your program to continue on a  file-not-
           found error, you can use something like:
           (_c_o_n_d ((_n_u_l_l (_s_e_t_q _m_y_p_o_r_t  (_c_a_r  (_e_r_r_s_e_t  (_i_n_f_i_l_e
           _n_a_m_e) _n_i_l))))
                       (_p_a_t_o_m '"_c_o_u_l_d_n'_t _o_p_e_n _t_h_e _f_i_l_e")))
           which will set myport to the port to read from if
           the  file  exists  or  will print a message if it
           couldn't open it and also set myport to nil.   To
           simply determine if a file exists, use _p_r_o_b_e_f.

(load 's_filename ['st_map ['g_warn]])

     RETURNS: t

     NOTE: The function of _l_o_a_d has changed  since  previous
           releases of FRANZ LISP and the following descrip-
           tion should be read carefully.

     SIDE EFFECT: _l_o_a_d now serves the function of both  _f_a_s_l
                  and the old _l_o_a_d.  _L_o_a_d will search a user
                  defined search path for a lisp  source  or
                  object  file  with the filename s_filename
                  (with the extension  .l  or  .o  added  as
                  appropriate).   The search path which _l_o_a_d
                  uses is the value of  (_s_t_a_t_u_s _l_o_a_d-_s_e_a_r_c_h-
                  _p_a_t_h).  The default is (|.| /usr/lib/lisp)
                  which means look in the current  directory
                  first  and  then  /usr/lib/lisp.  The file
                  which _l_o_a_d looks for depends on  the  last
                  two    characters   of   s_filename.    If
                  s_filename ends with ".l" then  _l_o_a_d  will
                  only  look  for a file name s_filename and
                  will assume that  this  is  a  FRANZ  LISP
                  source file.  If s_filename ends with ".o"
                  then _l_o_a_d will only look for a file  named
                  s_filename  and will assume that this is a
                  FRANZ LISP object file to  be  _f_a_s_led  in.
                  Otherwise,   _l_o_a_d   will  first  look  for
                  s_filename.o,   then   s_filename.l    and
                  finally  s_filename  itself.   If it finds
                  s_filename.o it will assume that  this  is
                  an  object  file, otherwise it will assume
                  that it is a source file.  An object  file
                  is  loaded using _f_a_s_l and a source file is
                  loaded by reading and evaluating each form


                                   Printed: January 31, 1984







Input/Output                                             5-8


                  in   the  file.   The  optional  arguments
                  st_map  and  g_warn  are  passed  to  _f_a_s_l
                  should _f_a_s_l be called.

     NOTE: _l_o_a_d requires a port to open the file s_filename.
           It  then  lambda  binds the symbol piport to this
           port and reads and evaluates the forms.

(makereadtable ['s_flag])

     WHERE:   if s_flag is not present it is assumed  to  be
              nil.

     RETURNS: a readtable equal to the original readtable if
              s_flag  is  non-null,  or  else  equal  to the
              current  readtable.   See  chapter  7  for   a
              description of readtables and their uses.

(msg [l_option ...] ['g_msg ...])

     NOTE: This function is intended for printing short mes-
           sages.  Any of the arguments or options presented
           can be used any number of times,  in  any  order.
           The  messages  themselves  (g_msg) are evaluated,
           and then they are transmitted  to  _p_a_t_o_m.   Typi-
           cally,  they are strings, which evaluate to them-
           selves.  The options are interpreted specially:


    ____________________________________________________

    _m_s_g _O_p_t_i_o_n _S_u_m_m_a_r_y

    (_P _p__p_o_r_t_n_a_m_e)          causes subsequent output to go to the port p_portname
                                    (port should be opened previously)

    _B                       print a single blank.

    (_B '_n__b)                evaluate n_b and print that many blanks.

    _N                       print a single by calling _t_e_r_p_r.

    (_N '_n__n)                evaluate n_n and transmit
                                    that many newlines to the stream.

    _D                       _d_r_a_i_n the current port.
    ____________________________________________________





9

9                                   Printed: January 31, 1984







Input/Output                                             5-9


(nwritn ['p_port])

     RETURNS: the number of characters in the buffer of  the
              given port but not yet written out to the file
              or device.  The buffer  is  flushed  automati-
              cally when filled, or when _t_e_r_p_r is called.

(outfile 's_filename ['st_type])

     RETURNS: a port or nil

     SIDE EFFECT: this opens a port to write s_filename.  If
                  st_type is given and if it is  a symbol or
                  string whose name begins  with  `a',  then
                  the  file  will  be opened in append mode,
                  that is the current contents will  not  be
                  lost  and the next data will be written at
                  the end of the file.  Otherwise, the  file
                  opened  is  truncated  by  _o_u_t_f_i_l_e  if  it
                  existed beforehand.  If there are no  free
                  ports, outfile returns nil.  If one cannot
                  write on  s_filename,  an  error  is  sig-
                  nalled.

(patom 'g_exp ['p_port])

     RETURNS: g_exp

     SIDE EFFECT: g_exp is printed to the given port or  the
                  default  port.   If  g_exp  is a symbol or
                  string, the print name is printed  without
                  any escape characters around special char-
                  acters in the print name.  If g_exp  is  a
                  list  then  _p_a_t_o_m  has  the same effect as
                  _p_r_i_n_t.

(pntlen 'xfs_arg)

     RETURNS: the  number  of  characters  needed  to  print
              xfs_arg.

(portp 'g_arg)

     RETURNS: t iff g_arg is a port.








9

9                                   Printed: January 31, 1984







Input/Output                                            5-10


(pp [l_option] s_name1 ...)

     RETURNS: t

     SIDE EFFECT: If s_name_i has a function binding,  it  is
                  pretty-printed, otherwise if s_name_i has a
                  value then that is  pretty-printed.   Nor-
                  mally  the  output  of  the pretty-printer
                  goes to the standard output  port  poport.
                  The options allow you to redirect it.


    ____________________________________________________

    _P_P _O_p_t_i_o_n _S_u_m_m_a_r_y

    (_F _s__f_i_l_e_n_a_m_e)          direct future printing to s_filename

    (_P _p__p_o_r_t_n_a_m_e)          causes output to go to the port p_portname
                                    (port should be opened previously)

    (_E _g__e_x_p_r_e_s_s_i_o_n)                evaluate g_expression and don't print
    ____________________________________________________




(princ 'g_arg ['p_port])

     EQUIVALENT TO: patom.

(print 'g_arg ['p_port])

     RETURNS: nil

     SIDE EFFECT: prints g_arg on the  port  p_port  or  the
                  default port.

(probef 'st_file)

     RETURNS: t iff the file st_file exists.

     NOTE: Just because it exists doesn't mean you can  read
           it.








9

9                                   Printed: January 31, 1984







Input/Output                                            5-11


(pp-form 'g_form ['p_port])

     RETURNS: t

     SIDE EFFECT: g_form  is  pretty-printed  to  the   port
                  p_port (or poport if p_port is not given).
                  This is the  function which _p_p  uses.  _p_p-
                  _f_o_r_m  does  not  look for function defini-
                  tions or  values  of  variables,  it  just
                  prints out the form it is given.

     NOTE: This is useful as a top-level-printer, c.f.  _t_o_p-
           _l_e_v_e_l in Chapter 6.

(ratom ['p_port ['g_eof]])

     RETURNS: the next atom read from the given  or  default
              port.   On end of file, g_eof (default nil) is
              returned.

(read ['p_port ['g_eof]])

     RETURNS: the next lisp expression read from  the  given
              or  default  port.   On  end  of  file,  g_eof
              (default nil) is returned.

     NOTE: An error will occur if the reader is given an ill
           formed  expression.  The most common error is too
           many right parentheses (note  that  this  is  not
           considered an error in Maclisp).

(readc ['p_port ['g_eof]])

     RETURNS: the next character  read  from  the  given  or
              default  port.  On end of file, g_eof (default
              nil) is returned.

(readlist 'l_arg)

     RETURNS: the lisp expression  read  from  the  list  of
              characters in l_arg.











9

9                                   Printed: January 31, 1984







Input/Output                                            5-12


(removeaddress 's_name1 ['s_name2 ...])

     RETURNS: nil

     SIDE EFFECT: the entries for the s_name_i  in  the  Lisp
                  symbol  table are removed.  This is useful
                  if you wish to _c_f_a_s_l or _f_f_a_s_l  in  a  file
                  twice, since it is illegal for a symbol in
                  the file you are loading to already  exist
                  in the lisp symbol table.

(resetio)

     RETURNS: nil

     SIDE EFFECT: all ports except the standard input,  out-
                  put and error are closed.

(setsyntax 's_symbol 's_synclass ['ls_func])

     RETURNS: t

     SIDE EFFECT: this sets the code for s_symbol to sx_code
                  in  the  current readtable.  If s_synclass
                  is _m_a_c_r_o or _s_p_l_i_c_i_n_g then ls_func  is  the
                  associated  function.   See  Chapter  7 on
                  the reader for more details.

(sload 's_file)

     SIDE EFFECT: the file s_file (in the current directory)
                  is  opened  for  reading  and each form is
                  read, printed and evaluated.  If the  form
                  is  recognizable as a function definition,
                  only its name will be  printed,  otherwise
                  the whole form is printed.

     NOTE: This function is useful when a  file  refuses  to
           load because of a syntax error and you would like
           to narrow down where the error is.

(tab 'x_col ['p_port])

     SIDE EFFECT: enough spaces are printed to put the  cur-
                  sor  on  column  x_col.   If the cursor is
                  beyond x_col to start  with,  a  _t_e_r_p_r  is
                  done first.





9

9                                   Printed: January 31, 1984







Input/Output                                            5-13


(terpr ['p_port])

     RETURNS: nil

     SIDE EFFECT: a terminate line   character  sequence  is
                  sent  to  the  given  port  or the default
                  port.  This will also drain the port.

(terpri ['p_port])

     EQUIVALENT TO: terpr.

(tilde-expand 'st_filename)

     RETURNS: a symbol whose pname is the tilde-expansion of
              the  argument,  (as discussed at the beginning
              of this chapter).  If the  argument  does  not
              begin  with  a  tilde,  the argument itself is
              returned.

(tyi ['p_port])

     RETURNS: the fixnum representation of the next  charac-
              ter read.  On end of file, -1 is returned.

(tyipeek ['p_port])

     RETURNS: the fixnum representation of the next  charac-
              ter to be read.

     NOTE: This does not actually  read  the  character,  it
           just peeks at it.

(tyo 'x_char ['p_port])

     RETURNS: x_char.

     SIDE EFFECT: the character whose fixnum  representation
                  is  x_code,  is  printed as a on the given
                  output port or the default output port.

(untyi 'x_char ['p_port])

     SIDE EFFECT: x_char is put back in the input buffer  so
                  a  subsequent  _t_y_i  or  _r_e_a_d  will read it
                  first.

     NOTE: a maximum of one character may be put back.




9

9                                   Printed: January 31, 1984







Input/Output                                            5-14


(username-to-dir 'st_name)

     RETURNS: the home directory of  the  given  user.   The
              result   is  stored,  to  avoid  unnecessarily
              searching the password file.

(zapline)

     RETURNS: nil

     SIDE EFFECT: all characters up  to  and  including  the
                  line  termination  character  are read and
                  discarded from  the  last  port  used  for
                  input.

     NOTE: this is used as the macro function for the  semi-
           colon character when it acts as a comment charac-
           ter.


































9

9                                   Printed: January 31, 1984



