






                        CHAPTER  13


         The CMU User Toplevel and the File Package



This documentation was written by Don Cohen, and  the  func-
tions described below were imported from PDP-10 CMULisp.

_N_o_n _C_M_U _u_s_e_r_s _n_o_t_e: this is not the default  top  level  for
your  Lisp system.  In order to start up this top level, you
should type (_l_o_a_d '_c_m_u_e_n_v).





_1_3._1.  _U_s_e_r _C_o_m_m_a_n_d _I_n_p_u_t _T_o_p _L_e_v_e_l

The top-level is the function  that  reads  what  you  type,
evaluates  it  and prints the result.  The _n_e_w_l_i_s_p top-level
was inspired by the CMULisp top-level (which was inspired by
interlisp) but is much simpler.  The top-level is a function
(of zero arguments) that can be called by your program.   If
you  prefer  another  top-level, just redefine the top-level
function and  type  "(reset)"  to  start  running  it.   The
current  top-level simply calls the functions tlread, tleval
and tlprint to read, evaluate and print.  These are supposed
to be replaceable by the user.  The only one that would make
sense to replace is tlprint, which currently uses a function
that  refuses  to go below a certain level and prints "...]"
when it finds itself printing a circular  list.   One  might
want  to  prettyprint the results instead.  The current top-
level numbers the lines that you type to it,  and  remembers
the  last n "events" (where n can be set but is defaulted to
25).  One can refer to these events in the  following  "top-
level commands":














9

9The CMU User Toplevel and the File Package              13-1







The CMU User Toplevel and the File Package              13-2



    ____________________________________________________

    _T_O_P_L_E_V_E_L _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

    ??      prints events - both the input and the result.  If you just type
            "??" you will see all of the recorded events.  "?? 3" will show
            only event 3, and "?? 3 6" will show events 3 through 6.

    redo    pretends that you typed the same thing that was typed before.  If
            you type "redo 3" event number 3 is redone.  "redo -3" redoes the
            thing 3 events ago.  "redo" is the same as "redo -1".

    ed      calls the editor and then does whatever the editor returns.  Thus
            if you want to do event 5 again except for some small change, you
            can type "ed 5", make the change and leave the editor.  "ed -3"
            and "ed" are analogous to redo.
    ____________________________________________________



Finally, you can get the value of event 7 with the  function
(valueof 7).  The other interesting feature of the top-level
is that it makes outermost parentheses superfluous  for  the
most  part.   This  works the same way as in CMULisp, so you
can use the help for an explanation.  If you're not sure and
don't  want  to  risk  it  you  can  always just include the
parentheses.


(top-level)

     SIDE EFFECT: _t_o_p-_l_e_v_e_l is the  LISP top level function.
                  As well as  being the  top level  function
                  with  which  the user interacts, it can be
                  called  recursively  by  the   user or any
                  function.  Thus, the   top  level  can  be
                  invoked  from  inside  the  editor,  break
                  package, or a user  function to  make  its
                  commands available to the user.

     NOTE: The  CMU FRANZ  LISP  top-level    uses  _l_i_n_e_r_e_a_d
           rather   than _r_e_a_d.  The difference will not usu-
           ally be noticeable.  The principal  thing  to  be
           careful   about is that input  to the function or
           system being called cannot appear   on  the  same
           line as the  top-level call.  For example, typing
           (_e_d_i_t_f _f_o_o)_f_P _o_n _o_n_e   _l_i_n_e  _w_i_l_l  _e_d_i_t  _f_o_o  _a_n_d
           _e_v_a_l_u_a_t_e  _P, _n_o_t _e_d_i_t _f_o_o _a_n_d _e_x_e_c_u_t_e _t_h_e  _p _c_o_m_-
           _m_a_n_d _i_n _t_h_e _e_d_i_t_o_r.  _t_o_p-_l_e_v_e_l  _s_p_e_c_i_a_l_l_y  _r_e_c_o_g_-
           _n_i_z_e_s _t_h_e _f_o_l_l_o_w_i_n_g _c_o_m_m_a_n_d_s:

9

9                                     Printed: March 23, 1982







The CMU User Toplevel and the File Package              13-3


(valueof '_g__e_v_e_n_t_s_p_e_c)

     RETURNS: the value(s)  of  the  event(s)  specified  by
              g_eventspec.   If a single event is specified,
              its value will be returned.  If more than  one
              event  is specified, or an event has more than
              one subevent (as for _r_e_d_o,  etc),  a  list  of
              vlaues will be returned.





     _1_3._2.  _T_h_e _F_i_l_e _P_a_c_k_a_g_e

     Users typically define functions in lisp and then  want
     to   save  them  for  the  next  session.   If  you  do
     (_c_h_a_n_g_e_s), a list  of  the  functions  that  are  newly
     defined  or  changed  will  be  printed.  When you type
     (_d_s_k_o_u_t_s), the functions associated with files will  be
     saved  in the new versions of those files.  In order to
     associate functions with files you can either add  them
     to the _f_i_l_e_f_n_s list of an existing file or create a new
     file to hold them.  This is done with  the  _f_i_l_e  func-
     tion.   If you type (_f_i_l_e _n_e_w) the system will create a
     variable called _n_e_w_f_n_s.  You may add the names  of  the
     functions to go into that file to _n_e_w_f_n_s.  After you do
     (_c_h_a_n_g_e_s), the functions which are in no other file are
     stored  in the value of the atom _c_h_a_n_g_e_s.  To put these
     all in  the  new  file,  (_s_e_t_q  _n_e_w_f_n_s  (_a_p_p_e_n_d  _n_e_w_f_n_s
     _c_h_a_n_g_e_s)).  Now if you do (_c_h_a_n_g_e_s), all of the changed
     functions should be associated with files.  In order to
     save  the  changes  on the files, do (_d_s_k_o_u_t_s).  All of
     the changed files (such as NEW) will  be  written.   To
     recover  the  new functions the next time you run FRANZ
     LISP, do (_d_s_k_i_n _n_e_w).
















9

9                                     Printed: March 23, 1982







The CMU User Toplevel and the File Package              13-4



    ____________________________________________________

    Script started on Sat Mar 14 11:50:32 1981
    $ newlisp
    Welcome to newlisp...
    1.(defun square (x) (* x x))            ; define a new function
    square
    2.(changes)                             ; See, this function is associated
                                            ; with no file.
    <no-file>      (square)nil
    3.(file 'new)                           ; So let's declare file NEW.
    new
    4.newfns                                ; It doesn't have anything on it yet.
    nil
    5.(setq newfns '(square))               ; Add the function associated
    (square)                                ; with no file to file NEW.
    6.(changes)                             ; CHANGES magically notices this fact.

    new            (square)nil
    7.(dskouts)                             ; We write the file.
    creating new
    (new)
    8.(dskin new)                           ; We read it in!
    (new)
    14.Bye
    $
    script done on Sat Mar 14 11:51:48 1981

    ____________________________________________________





(changes s_flag)

     RETURNS: Changes computes a list  containing  an  entry
              for  each  file  which defines atoms that have
              been marked changed.  The entry  contains  the
              file   name  and  the  changed  atoms  defined
              therein.  There is also a  special  entry  for
              changes  to atoms which are not defined in any
              known file.  The global variable _f_i_l_e_l_s_t  con-
              tains  the  list of "known" files.  If no flag
              is passed this  result  is  printed  in  human
              readable  form  and the value returned is t if
              there were any changes and nil if not.  Other-
              wise  nothing is printed and the computer list
              is returned.  The global variable _c_h_a_n_g_e_s con-
              tains  the  atoms which are marked changed but
              not yet associated with any file.  The _c_h_a_n_g_e_s
              function  attempts  to  associate  these names


                                     Printed: March 23, 1982







The CMU User Toplevel and the File Package              13-5


              with files, and any that  are  not  found  are
              considered  to belong to no file.  The _c_h_a_n_g_e_s
              property is the means by which  changed  func-
              tions  are associated with files.  When a file
              is read in or written out its _c_h_a_n_g_e_s property
              is removed.

(dc s_word s_id [ g_descriptor1  ... ] <text> <esc>)

     RETURNS: _d_c defines comments.   It  is  exceptional  in
              that  its  behavior is very context dependent.
              When _d_c  is  executed  from  _d_s_k_i_n  it  simply
              records  the fact that the comment exists.  It
              is expected that in interactive mode  comments
              will  be  found via _g_e_t_d_e_f - this allows large
              comments which do not take up  space  in  your
              core image.  When _d_c is executed from the ter-
              minal it expects you to type a comment. _d_s_k_o_u_t
              will  write  out  the comments that you define
              and also copy the comments on the old  version
              of the file, so that the new version will keep
              the old comments even though they  were  never
              actually  brought  into core.  The optional id
              is  a  mechanism  for   distinguishing   among
              several  comments  associated  with  the  same
              word.  It defaults to  nil.   However  if  you
              define  two  comments  with  the  same id, the
              second is considered to be a  replacement  for
              the first. The behavior of _d_c is determined by
              the value of the global variable  _d_e_f-_c_o_m_m_e_n_t.
              _d_e_f-_c_o_m_m_e_n_t  contains  the  name of a function
              that is run.  Its arguments are the  word,  id
              and  attribute  list. _d_e_f-_c_o_m_m_e_n_t is initially
              _d_c-_d_e_f_i_n_e.  Other functions rebind it  to  _d_c-
              _h_e_l_p,  _d_c-_u_s_e_r_h_e_l_p,  and  the  value of _d_s_k_i_n-
              _c_o_m_m_e_n_t.  The comment property of an atom is a
              list  of  entries,  each representing one com-
              ment.  Atomic entries are assumed to be  iden-
              tifiers of comments on a file but not in core.
              In-core comments are represented by a list  of
              the  id,  the  attribute  list and the comment
              text.  The comment text is an uninterned atom.
              Comments  may be deleted or reordered by edit-
              ing the comment property.








9

9                                     Printed: March 23, 1982







The CMU User Toplevel and the File Package              13-6


(dskin l_filenames)

     SIDE EFFECT: READ-EVAL-PRINTs  the  contents  of    the
                  given  files.  This is the function to use
                  to read files created  by  _d_s_k_o_u_t.   _d_s_k_i_n
                  also  declares the files that it reads (if
                  a _f_i_l_e-_f_n_s list is defined and the file is
                  otherwise  declarable  by  _f_i_l_e ), so that
                  changes to it can be recorded.


(dskout s_file1 ...)

     SIDE EFFECT: For each file  specified,  _d_s_k_o_u_t  assumes
                  the   list  named  filenameFNS (i.e.,  the
                  file  name,   excluding  extension,   con-
                  catenated   with  _f_n_s )  contains  a  list
                  of function names, etc., to be loaded  Any
                  previous  version  of  the  file  will  be
                  renamed  to have extension ".back".

(dskouts s_file1 ...)

     SIDE EFFECT: applies _d_s_k_o_u_t to and  prints the name  of
                  each  s_filei    (with    no    additional
                  arguments,   assuming filenameFNS to be  a
                  list  to  be loaded) for  which s_file_i is
                  either not in _f_i_l_e_l_s_t  (meaning  it  is  a
                  new   file not previously declared by _f_i_l_e
                  or  given  as  an   argument   to   _d_s_k_i_n,
                  _d_s_k_o_u_t_s,  or _d_s_k_o_u_t_s) or is in _f_i_l_e_l_s_t and
                  has some recorded changes  to  definitions
                  of  atoms  in  filenameFNS, as recorded by
                  _m_a_r_k!_c_h_a_n_g_e_d and  noted  by  changes.   If
                  _f_i_l_ei  is not  specified, _f_i_l_e_l_s_t  will be
                  used.  This  is the  most common   way  of
                  using  dskouts.   Typing   (_d_s_k_o_u_t_s)  will
                  save  every  file  reported  by  (_c_h_a_n_g_e_s)
                  to have changed definitions.


(dv s_atom g_value)

     EQUIVALENT TO: (_s_e_t_q   _a_t_o_m    '_v_a_l_u_e).     _d_v    calls
                    _m_a_r_k!_c_h_a_n_g_e_d.







9

9                                     Printed: March 23, 1982







The CMU User Toplevel and the File Package              13-7


(file 's_file)

     SIDE EFFECT: declares its argument to be a file  to  be
                  used  for  reporting and saving changes to
                  functions by adding the  file  name  to  a
                  list  of  files,  _f_i_l_e_l_s_t.  _f_i_l_e is called
                  for each file argument of  _d_s_k_i_n,  _d_s_k_o_u_t,
                  and _d_s_k_o_u_t_s.

(file-fns 's_file)

     RETURNS: the name of the  fileFNS  list  for  its  file
              argument s_file.

(getdef 's_file ['s_i1 ...])

     SIDE EFFECT: selectively executes definitions for atoms
                  s_i1  ... from the specified file.  Any of
                  the words to be defined which end with "@"
                  will be treated as patterns in which the @
                  matchs any suffix (just like the  editor).
                  _g_e_t_d_e_f  is driven by _g_e_t_d_e_f_t_a_b_l_e (and thus
                  may be programmed).  It looks for lines in
                  the  file  that  start  with a word in the
                  table.  The first character must be a  "("
                  or "[" followed by the word, followed by a
                  space, return or something else that  will
                  not  be considered as part of the identif-
                  ier by _r_e_a_d, e.g.,  "("  is  unacceptable.
                  When  one  is found the next word is read.
                  If it matches one of  the  identifiers  in
                  the call to _g_e_t_d_e_f then the table entry is
                  executed.  The table entry is  a  function
                  of  the  expression starting in this line.
                  Output from _d_s_k_o_u_t is in acceptable format
                  for _g_e_t_d_e_f.  _g_e_t_d_e_f

     RETURNS: a list of the words which match  the  ones  it
              looked for, for which it found (but, depending
              on the table, perhaps did not execute) in  the
              file.

     NOTE: _g_e_t_d_e_f_t_a_b_l_e is the table that drives _g_e_t_d_e_f.   It
           is in the form of an association list.  Each ele-
           ment is a dotted pair consisting of the name of a
           function for which _g_e_t_d_e_f searches and a function
           of one argument to be executed when it is found.





9

9                                     Printed: March 23, 1982







The CMU User Toplevel and the File Package              13-8


(mark!changed 's_f)

     SIDE EFFECT: records the fact that  the  definition  of
                  s_f has been changed.  It is automatically
                  called by _d_e_f, _d_e_f_u_n, _d_e, _d_f, _d_e_f_p_r_o_p, _d_m,
                  _d_v,  and  the  editor when a definition is
                  altered.













































9

9                                     Printed: March 23, 1982



