






                                CHAPTER  5


                               Input/Output



The following functions are used to read and write  to  and  from  external
devices  and  programs (through pipes).  All I/O goes through the lisp data
type called the port.  A port may be open for either reading or writing but
not both simultaneously.  There are only a limited number of ports (20) and
they will not be reclaimed unless you _c_l_o_s_e them.  All ports are  reclaimed
by  a  _r_e_s_e_t_i_o call but this drastic step won't be necessary if the program
closes what it uses.  If you don't supply a port  argument  to  a  function
which  requires one or if you supply a bad port argument (such as nil) then
FRANZ LISP will use the default port according to this scheme.  If you  are
reading  then the default port is the value of the symbol _p_i_p_o_r_t and if you
are writing it is the value of the symbol _p_o_p_o_r_t.  Furthermore if the value
of piport or poport is not a valid port then the standard input or standard
output will be used, respectively.  The standard input and standard  output
are usually the keyboard and terminal display unless your job is running in
the background and its input or output is connected to a pipe.  All  output
which  goes to the standard output will also go to the port _p_t_p_o_r_t if it is
a valid port.  Output destined for the standard output will not reach it if
the  symbol ^_w is non nil (although it will still go to _p_t_p_o_r_t if _p_t_p_o_r_t is
a valid port).


(cfasl 'st_file 'st_entry 's_funcname ['st_disc ['st_library]])

     RETURNS: t

     SIDE EFFECT: This is use to load in a foreign function (see 8.4).  The
                  object  file  st_file  is  loaded  into  the lisp system.
                  St_entry should be  an  entry  point  in  the  file  just
                  loaded.   The  function  binding of the symbol s_funcname
                  will be set to point to st_entry, so that when  the  lisp
                  function  s_funcname  is  called,  st_entry  will be run.
                  st_disc is the discipline  to  be  given  to  s_funcname.
                  st_disc defaults to "subroutine" if it is not given or if
                  it is given as nil.  If st_library is non nil, then after
                  st_file is loaded, the libraries given in st_library will
                  be searched to resolve external references.  The form  of
                  st_library  should  be  something  like "-lS -lm".  The c
                  library  (" -lc " ) is always searched so when loading in
                  a  C  file  you probably won't need to specify a library.
                  For Fortran files, you should specify "-lF77" and if  you
                  are  doing  any  I/O  that should be "-lF77 -lIf77".  For
                  Pascal files "-lpc" is required.

     NOTE: This function may be used to load the output of the assembler, C
           compiler,  Fortran  compiler,  and  Pascal compiler  but NOT the
           lisp compiler (use _f_a_s_l for that).  If a file has more than  one


Input/Output                                                            5-1







Input/Output                                                            5-2


           entry  point,  then  use  _g_e_t_a_d_d_r_e_s_s  to  locate and setup other
           foreign functions.


(close 'p_port)

     RETURNS:

     SIDE EFFECT: the specified port is drained and closed,  releasing  the
                  port.

     NOTE: The standard defaults are not used in this case since you  prob-
           ably never want to close the standard output or standard input.


(cprintf 'st_format 'xfst_val ['p_port])

     RETURNS: xfst_val

     SIDE EFFECT: The UNIX formatted output function printf is called  with
                  arguments  st_format and xfst_val.  If xfst_val is a sym-
                  bol then its print name is passed to printf.  The  format
                  string  may  contain  characters  which  are just printed
                  literally and it may contain special formatting  commands
                  preceded  by a percent sign.  The complete set of format-
                  ting characters is described in the  UNIX  manual.   Some
                  useful  ones  are %d for printing a fixnum in decimal, %f
                  or %e for printing a flonum, and %s for printing a  char-
                  acter string (or print name of a symbol).

     EXAMPLE: (_c_p_r_i_n_t_f "_P_i _e_q_u_a_l_s %_f" _3._1_4_1_5_9) prints `Pi equals 3.14159'


(drain ['p_port])

     RETURNS: nil

     SIDE EFFECT: If this is an output port then the characters in the out-
                  put  buffer  are  all  sent to the device.  If this is an
                  input port then all pending characters are flushed.   The
                  default  port  for  this  function  is the default output
                  port.










9

9                                                  Printed: October 22, 1980







Input/Output                                                            5-3


(fasl 'st_name ['st_mapf ['g_warn]])

     WHERE:   st_mapf and g_warn default to nil.

     RETURNS: t if the function succeeded, nil otherwise.

     SIDE EFFECT: this function is designed to load in an object file  gen-
                  erated by the lisp compiler Liszt.  File names for object
                  files usually end in `.o', so _f_a_s_l will append   `.o'  to
                  st_name  (if  it  is not already present).  If st_mapf is
                  non nil, then it is the name of the map file  to  create.
                  _F_a_s_l  writes  in  the map file the names and addresses of
                  the functions it loads and  defines.   Normally  the  map
                  file  is  created  (i.e.  truncated if it exists), but if
                  (_s_s_t_a_t_u_s _a_p_p_e_n_d_m_a_p _t) is done then the map file  will  be
                  appended.   If  g_warn  is  non nil and if  a function is
                  loaded from  the file which is already  defined,  then  a
                  warning message will be printed.


(ffasl 'st_file 'st_entry 'st_funcname ['st_discipline])

     RETURNS: the binary object created.

     SIDE EFFECT: the fortran object file st_file is loaded into  the  lisp
                  system.   St_entry  should  be an entry point in the file
                  just loaded. A binary object  will  be  created  and  its
                  entry  field  will be set to point to st_entry.  The dis-
                  cipline field of the binary will be set to  st_discipline
                  or "subroutine" by default.  After st_file is loaded, the
                  standard fortran libraries will be  searched  to  resolve
                  external references.

     NOTE: in F77 on Unix, the entry point for the fortran function foo  is
           named `_foo_'.


(flatc 'g_form ['x_max])

     RETURNS: the number of  characters  required  to  print  g_form  using
              _p_a_t_o_m.   If  x_max is given, and the _f_l_a_t_c determines that it
              will return a value greater than x_max, then it gives up  and
              returns the current value it has computed.  This is useful if
              you just want to see if an expression is larger than  a  cer-
              tain size.







9

9                                                  Printed: October 22, 1980







Input/Output                                                            5-4


(flatsize 'g_form ['x_max])

     RETURNS: the number of  characters  required  to  print  g_form  using
              _p_r_i_n_t.  The meaning of x_max is the same as for flatc.

     NOTE: Currently this just _e_x_p_l_o_d_e's g_form and checks its length.


(fseek 'p_port 'x_offset 'x_flag)

     RETURNS: the position in the file after the function is performed.

     SIDE EFFECT: this function position the read/write  pointer  before  a
                  certain  byte  in  the  file.   If  x_flag  is 0 then the
                  pointer is set to x_offset bytes from  the  beginning  of
                  the  file.   If  x_flag  is  1 then the pointer is set to
                  x_offset bytes from the current location in the file.  If
                  x_flag  is  2  then  the pointer is set to x_offset bytes
                  from the end of the file (the bytes between  the  end  of
                  the  file  and  the  new  position  will  be  filled with
                  zeroes).


(infile 's_filename)

     RETURNS: a port ready to read s_filename.

     SIDE EFFECT: this tries to open s_filename and  if  it  cannot  or  if
                  there are no ports available it gives an error message.

     NOTE: to allow your program to continue on a file not found error  you
           can use something like:
           (_c_o_n_d ((_n_u_l_l (_s_e_t_q _m_y_p_o_r_t (_c_a_r (_e_r_r_s_e_t (_i_n_f_i_l_e _n_a_m_e) _n_i_l))))
                       (_p_a_t_o_m '"_c_o_u_l_d_n'_t _o_p_e_n _t_h_e _f_i_l_e")))
           which will set myport to the port  to  read  from  if  the  file
           exists  or  will print a message if it couldn't open it and also
           set myport to nil.


(load 's_filename ['st_map ['g_warn]])

     RETURNS: t

     NOTE: The function of _l_o_a_d has  changed  since  previous  releases  of
           FRANZ  LISP  and  the following description should be read care-
           fully.

     SIDE EFFECT: _l_o_a_d now serves the function of both  _f_a_s_l  and  the  old
                  _l_o_a_d.   _L_o_a_d will search a user defined search path for a
                  lisp source or object file with the  filename  s_filename
                  (with  the extension .l or .o added as appropriate).  The
                  search  path  which   _l_o_a_d   uses   is   the   value   of
                  (_s_t_a_t_u_s _l_o_a_d-_s_e_a_r_c_h-_p_a_t_h).       The      default      is


                                                  Printed: October 22, 1980







Input/Output                                                            5-5


                  (|.| /usr/lib/lisp)  which  means  look  in  the  current
                  directory  first  and then /usr/lib/lisp.  The file which
                  _l_o_a_d looks for depends on  the  last  two  characters  of
                  s_filename.   If s_filename ends with ".l" then _l_o_a_d will
                  only look for a file name s_filename and will assume that
                  this  is  a  FRANZ  LISP source file.  If s_filename ends
                  with ".o" then _l_o_a_d will  only  look  for  a  file  named
                  s_filename  and  will  assume  that  this is a FRANZ LISP
                  object file to be _f_a_s_led in.  Otherwise, _l_o_a_d will  first
                  look  for  s_filename.o,  then  s_filename.l  and finally
                  s_filename itself.  If  it  finds  s_filename.o  it  will
                  assume  that  this  is  an object file, otherwise it will
                  assume that it is a  source  file.   An  object  file  is
                  loaded  using _f_a_s_l and a source file is loaded by reading
                  and evaluating each form in the file.  The optional argu-
                  ments st_map and g_warn are passed to _f_a_s_l should _f_a_s_l be
                  called.

     NOTE: _l_o_a_d requires a port to  open  the  file  s_filename.   It  then
           lambda binds the symbol piport to this port and reads and evalu-
           ates the forms.


(makereadtable ['s_flag])

     WHERE:   if s_flag is not present it is assumed to be nil.

     RETURNS: a readtable equal to the original readtable if s_flag is  non
              nil,  or  else equal to the current readtable.  See chapter 7
              for a description of readtables and their uses.


(nwritn ['p_port])

     RETURNS: the number of characters in the buffer of the given port  but
              not  yet  written  out  to the file or device.  The buffer is
              flushed automatically after the buffer (of 512 characters) is
              filled or when _t_e_r_p_r is called.


(outfile 's_filename)

     RETURNS: a port or nil

     SIDE EFFECT: this opens a port to write s_filename.  The  file  opened
                  is truncated by the _o_u_t_f_i_l_e if it existed beforehand.  If
                  there are no free ports, outfile returns nil.





9

9                                                  Printed: October 22, 1980







Input/Output                                                            5-6


(patom 'g_exp ['p_port])

     RETURNS: g_exp

     SIDE EFFECT: g_exp is printed to the given port or the  default  port.
                  If  g_exp  is  a  symbol  then  the print name is printed
                  without any escape characters around  special  characters
                  in the print name.  If g_exp is a list then _p_a_t_o_m has the
                  same effect as _p_r_i_n_t.


(pntlen 'xfs_arg)

     WHERE:   xfs_arg is a fixnum, flonum or symbol.

     RETURNS: the number of characters needed to print  xfs_arg.


(portp 'g_arg)

     RETURNS: t iff g_arg is a port.


(pp [l_option] s_name1 ...)

     RETURNS: t

     SIDE EFFECT: If s_name_i has a function binding, it is pretty  printed,
                  otherwise  if  s_name_i  has  a  value then that is pretty
                  printed.  Normally the output of the pretty printer  goes
                  to  the  standard  output port poport.  The options allow
                  you to redirect it.  The option (_F _f_i_l_e_n_a_m_e) causes  out-
                  put  to go to the file filename.  The option (_P _p_o_r_t_n_a_m_e)
                  causes output to go to the  port  portname  which  should
                  have been opened previously.


(princ 'g_arg ['p_port])

     EQUIVALENT TO: patom.












9

9                                                  Printed: October 22, 1980







Input/Output                                                            5-7


(print 'g_arg ['p_port])

     RETURNS: nil

     SIDE EFFECT: prints g_arg on the port p_port or the default port.


(probef 'st_file)

     RETURNS: t iff the file st_file exists.

     NOTE: Just because it exists doesn't mean you can read it.


(ratom ['p_port ['g_eof]])

     RETURNS: the next atom read from the given or default port.  On end of
              file, g_eof (default nil) is returned.


(read ['p_port ['g_eof]])

     RETURNS: the next lisp expression read from the given or default port.
              On end of file, g_eof (default nil) is returned.


(readc ['p_port ['g_eof]])

     RETURNS: the next character read from the given or default  port.   On
              end of file, g_eof (default nil) is returned.


(readlist 'l_arg)

     RETURNS: the lisp expression read  from  the  list  of  characters  in
              l_arg.


(resetio)

     RETURNS: nil

     SIDE EFFECT: all ports except the standard input, output and error are
                  closed.








9

9                                                  Printed: October 22, 1980







Input/Output                                                            5-8


(setsyntax 's_symbol 'sx_code ['ls_func])

     RETURNS: t

     SIDE EFFECT: this sets the code for s_symbol to sx_code in the current
                  readtable.   If sx_code is _m_a_c_r_o or _s_p_l_i_c_i_n_g then ls_func
                  is the associated function.  See section 7 on the  reader
                  for more details.


(terpr ['p_port])

     RETURNS: nil

     SIDE EFFECT: a terminate line  character sequence is sent to the given
                  port  or  the  default  port.   This  will also flush the
                  buffer.


(terpri ['p_port])

     EQUIVALENT TO: terpr.


(tyi ['p_port])

     RETURNS: the fixnum representation of the next character read.  On end
              of file, -1 is returned.


(tyipeek ['p_port])

     RETURNS: the fixnum representation of the next character to be read.

     NOTE: This does not actually read the character, it just peeks at it.


(tyo 'x_char ['p_port])

     RETURNS: x_char.

     SIDE EFFECT: the fixnum representation  of  a  character,  x_code,  is
                  printed  as  a  character on the given output port or the
                  default output port.








9

9                                                  Printed: October 22, 1980







Input/Output                                                            5-9


(zapline)

     RETURNS: nil

     SIDE EFFECT: all characters up to and including the  line  termination
                  character  are read and discarded from the last port used
                  for input.

     NOTE: this is used as the macro function for the  semicolon  character
           when it acts as a comment character.










































9

9                                                  Printed: October 22, 1980



