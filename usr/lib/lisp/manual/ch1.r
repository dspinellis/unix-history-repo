






                         CHAPTER  1


                         FRANZ LISP






   1.1.  FRANZ LISP[|-] was created  as  a  tool  to  further
      research   in  symbolic  and  algebraic  manipulation,
      artificial intelligence, and programming languages  at
      the University of California at Berkeley.   Its  roots
      are in a PDP-11 Lisp system which originally came from
      Harvard.  As it grew it adopted  features  of  Maclisp
      and Lisp Machine Lisp.  Substantial compatibility with
      other Lisp dialects (Interlisp, UCILisp,  CMULisp)  is
      achieved  by  means  of  support packages and compiler
      switches.  The heart of FRANZ LISP is  written  almost
      entirely in the programming language C.  Of course, it
      has been greatly  extended  by  additions  written  in
      Lisp.   A  small  part  is  written  in  the  assembly
      language for the current host machines,  VAXen  and  a
      couple  of  flavors  of  68000.  Because FRANZ LISP is
      written in C, it is relatively portable  and  easy  to
      comprehend.

           FRANZ LISP is capable of running large lisp  pro-
      grams in a timesharing environment, has facilities for
      arrays and user defined structures, has  a  user  con-
      trolled reader with character and word macro  capabil-
      ities, and can interact directly with  compiled  Lisp,
      C, Fortran, and Pascal code.

           This document is a reference manual for the FRANZ
      LISP  system.  It is not a Lisp primer or introduction
      to the language.  Some parts will be of interest  pri-
      marily  to  those maintaining FRANZ LISP at their com-
      puter site.  There is an additional document  entitled
      _T_h_e  _F_r_a_n_z  _L_i_s_p  _S_y_s_t_e_m, _b_y _J_o_h_n _F_o_d_e_r_a_r_o, _w_h_i_c_h _p_a_r_-
      _t_i_a_l_l_y _d_e_s_c_r_i_b_e_s  _t_h_e  _s_y_s_t_e_m  _i_m_p_l_e_m_e_n_t_a_t_i_o_n.   _F_R_A_N_Z
      _L_I_S_P,  _a_s  _d_e_l_i_v_e_r_e_d  _b_y _B_e_r_k_e_l_e_y, _i_n_c_l_u_d_e_s _a_l_l _s_o_u_r_c_e
      _c_o_d_e _a_n_d _m_a_c_h_i_n_e _r_e_a_d_a_b_l_e _v_e_r_s_i_o_n _o_f _t_h_i_s  _m_a_n_u_a_l  _a_n_d
      _s_y_s_t_e_m  _d_o_c_u_m_e_n_t.   _T_h_e _s_y_s_t_e_m _d_o_c_u_m_e_n_t _i_s _i_n _a _s_i_n_g_l_e
      _f_i_l_e _n_a_m_e_d "_f_r_a_n_z._n" _i_n _t_h_e "_d_o_c" _s_u_b_d_i_r_e_c_t_o_r_y.
____________________
9   [|-]It is rumored that this name has something to do  with
Franz  Liszt  [F_rants List] (1811-1886) a Hungarian composer
and keyboard virtuoso. These  allegations  have  never  been
proven.



9FRANZ LISP                                               1-1







FRANZ LISP                                               1-2


           This document is divided into four Movements.  In
      the first one we will attempt to describe the language
      of FRANZ LISP  precisely  and  completely  as  it  now
      stands  (Opus  38.69, June 1983).  In the second Move-
      ment we will  look  at  the  reader,  function  types,
      arrays  and exception handling.  In the third Movement
      we will look at several large support packages written
      to help the FRANZ LISP user, namely the trace package,
      compiler, fixit and  stepping  package.   Finally  the
      fourth  movement  contains   an  index  into the other
      movements.  In the rest of this chapter we shall exam-
      ine  the  data  types  of FRANZ LISP.  The conventions
      used in the description of the  FRANZ  LISP  functions
      will  be  given  in  1.3  -- it is very important that
      these conventions are  understood.



   1.2.  Data Types   FRANZ LISP has  fourteen  data  types.
      In  this  section we shall look in detail at each type
      and if a type is divisible we shall  look  inside  it.
      There  is  a  Lisp function _t_y_p_e which will return the
      type name of a lisp  object.   This  is  the  official
      FRANZ  LISP  name  for  that type and we will use this
      name and this name only in the manual to avoid confus-
      ing  the  reader.   The  types  are listed in terms of
      importance rather than alphabetically.



      1.2.0.  lispval   This is the name we use to  describe
         any  Lisp  object.   The  function  _t_y_p_e will never
         return `lispval'.



      1.2.1.  symbol   This object corresponds to a variable
         in most other programming languages.  It may have a
         value or may be `unbound'.  A symbol may be  _l_a_m_b_d_a
         _b_o_u_n_d meaning that its current value is stored away
         somewhere and the symbol  is given a new value  for
         the  duration  of a certain context.  When the Lisp
         processor   leaves  that  context,   the   symbol's
         current  value  is thrown away and its old value is
         restored.
9         A symbol may also have a  _f_u_n_c_t_i_o_n  _b_i_n_d_i_n_g.   This
         function  binding  is  static;  it cannot be lambda
         bound.  Whenever the symbol is used  in  the  func-
         tional  position  of a Lisp expression the function
         binding of the symbol is examined  (see  Chapter  4
         for more details on  evaluation).
9         A symbol may also have  a  _p_r_o_p_e_r_t_y  _l_i_s_t,  another


                                   Printed: January 31, 1984







FRANZ LISP                                               1-3


         static  data structure.  The property list consists
         of a list of an even number of elements, considered
         to  be  grouped  as pairs. The first element of the
         pair is the _i_n_d_i_c_a_t_o_r the second the _v_a_l_u_e of  that
         indicator.
9         Each symbol has a print name (_p_n_a_m_e) which  is  how
         this  symbol is accessed from input and referred to
         on  (printed) output.
9         A symbol also has a hashlink used to  link  symbols
         together  in the oblist -- this field is inaccessi-
         ble to the lisp user.
9         Symbols are created by the reader and by the  func-
         tions  _c_o_n_c_a_t,  _m_a_k_n_a_m and their derivatives.  Most
         symbols live  on  FRANZ  LISP's  sole  _o_b_l_i_s_t,  and
         therefore  two symbols with the same print name are
         usually the  exact same object (they are _e_q).  Sym-
         bols  which  are  not  on the oblist are said to be
         _u_n_i_n_t_e_r_n_e_d.  The function _m_a_k_n_a_m creates uninterned
         symbols while _c_o_n_c_a_t creates _i_n_t_e_r_n_e_d ones.


8    ____________________________________________________________
     Subpart name   Get value   Set value          Type

8    ________________________________________________________________________________________________________________________
        value         eval         set            lispval
                                  setq
8    ____________________________________________________________
       property       plist     setplist        list or nil
         list          get       putprop
                                 defprop
8    ____________________________________________________________
       function       getd        putd      array, binary, list
       binding                     def            or nil
8    ____________________________________________________________
      print name    get_pname                     string
8    ____________________________________________________________
      hash link
8    ____________________________________________________________
7   |7|7|7|7|7|7|7|7|7|7|7|7|7|












                 |7|7|7|7|7|7|7|7|7|7|7|7|7|












                             |7|7|7|7|7|7|7|7|7|7|7|7|7|












                                         |7|7|7|7|7|7|7|7|7|7|7|7|7|












                                                               |7|7|7|7|7|7|7|7|7|7|7|7|7|

















      1.2.2.  list   A list cell has two parts,  called  the
         car  and  cdr.  List cells are created by the func-
         tion _c_o_n_s.


8          ________________________________________________
           Subpart name   Get value   Set value    Type

8          ________________________________________________________________________________________________
               car           car       rplaca     lispval
8          ________________________________________________
               cdr           cdr       rplacd     lispval



9                                   Printed: January 31, 1984







FRANZ LISP                                               1-4


8          ________________________________________________
799         |99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|
7777777777777777777777777777777777777777777777799                       |99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|
7777777777777777777777777777777777777777777777799                                   |99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|
7777777777777777777777777777777777777777777777799                                               |99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|
7777777777777777777777777777777777777777777777799                                                         |99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|
777777777777777777777777777777777777777777777


      1.2.3.  binary   This type acts as a  function  header
         for  machine  coded functions.  It has two parts, a
         pointer to the start of the function and  a  symbol
         whose print name describes the argument _d_i_s_c_i_p_l_i_n_e.
         The discipline (if _l_a_m_b_d_a, _m_a_c_r_o or _n_l_a_m_b_d_a) deter-
         mines  whether  the arguments to this function will
         be evaluated by the caller before this function  is
         called.   If  the  discipline is a string (specifi-
         cally "_s_u_b_r_o_u_t_i_n_e", "_f_u_n_c_t_i_o_n", "_i_n_t_e_g_e_r-_f_u_n_c_t_i_o_n",
         "_r_e_a_l-_f_u_n_c_t_i_o_n", "_c-_f_u_n_c_t_i_o_n", "_d_o_u_b_l_e-_c-_f_u_n_c_t_i_o_n",
         or "_v_e_c_t_o_r-_c-_f_u_n_c_t_i_o_n" ) then this  function  is  a
         foreign  subroutine  or  function (see 8.5 for more
         details on this).  Although the type of  the  _e_n_t_r_y
         field  of a binary type object is usually string or
         other, the object pointed to is actually a sequence
         of machine instructions.
         Objects of type binary are  created  by  _m_f_u_n_c_t_i_o_n,
         _c_f_a_s_l, and _g_e_t_a_d_d_r_e_s_s.


8      _________________________________________________________
       Subpart name   Get value   Set value         Type

8      __________________________________________________________________________________________________________________
          entry       getentry                string or fixnum
8      _________________________________________________________
        discipline     getdisc     putdisc    symbol or fixnum
8      _________________________________________________________
7     |8|7|7|7|7|



9                   |8|7|7|7|7|



9                               |8|7|7|7|7|



9                                           |8|7|7|7|7|



9                                                              |8|7|7|7|7|





9


      1.2.4.  fixnum   A fixnum is an  integer  constant  in
         the  range -2[31] to 2[31]-1.  Small fixnums (-1024
         to 1023) are stored in  a  special  table  so  they
         needn't  be  allocated each time one is needed.  In
         principle, the range for fixnums is machine  depen-
         dent,  although  all  current  implementations  for
         franz have this range.



      1.2.5.  flonum   A flonum is a double  precision  real
         number.  On  the  VAX, the range is +_2.9x10[-37] to
         +_1.7x10[38].   There  are   approximately   sixteen
         decimal  digits  of  precision.  Other machines may
         have other ranges.





9                                   Printed: January 31, 1984







FRANZ LISP                                               1-5


      1.2.6.  bignum   A bignum is an integer of potentially
         unbounded  size.   When  integer arithmetic exceeds
         the limits of fixnums mentioned above, the calcula-
         tion  is  automatically  done with bignums.  Should
         calculation with bignums give a result which can be
         represented  as a fixnum, then the fixnum represen-
         tation will be used[|-].  This contraction is  known
         as  _i_n_t_e_g_e_r  _n_o_r_m_a_l_i_z_a_t_i_o_n.   Many  Lisp  functions
         assume that integers are normalized.   Bignums  are
         composed  of  a  sequence  of list cells and a cell
         known as an sdot.  The user should consider a  big-
         num structure indivisible and use functions such as
         _h_a_i_p_a_r_t, and _b_i_g_n_u_m-_l_e_f_t_s_h_i_f_t to extract  parts  of
         it.



      1.2.7.   string    A  string  is  a  null   terminated
         sequence  of characters.  Most functions of symbols
         which operate on the symbol's print name will  also
         work  on strings.  The default reader syntax is set
         so that a sequence of characters surrounded by dou-
         ble quotes is a string.



      1.2.8.  port   A port is a structure which the  system
         I/O routines can reference to transfer data between
         the Lisp system and external media.   Unlike  other
         Lisp  objects  there  are  a very limited number of
         ports (20).  Ports are allocated by _i_n_f_i_l_e and _o_u_t_-
         _f_i_l_e  and  deallocated  by  _c_l_o_s_e and _r_e_s_e_t_i_o.  The
         _p_r_i_n_t function prints a port as a percent sign fol-
         lowed  by  the  name of the file it is connected to
         (if the port was opened  by  _f_i_l_e_o_p_e_n,  _i_n_f_i_l_e,  _o_r
         _o_u_t_f_i_l_e).   During initialization, FRANZ LISP binds
         the symbol piport to a port attached to  the  stan-
         dard  input  stream.   This port prints as %$stdin.
         There are ports connected to  the  standard  output
         and  error  streams,  which  print  as %$stdout and
         %$stderr.  This is discussed in more detail at  the
         beginning of Chapter 5.




____________________
9   [|-]The current algorithms for integer  arithmetic  opera-
tions will return (in certain cases) a result between +_2[30]
and 2[31] as a bignum although this could be represented  as
a fixnum.



9                                   Printed: January 31, 1984







FRANZ LISP                                               1-6


      1.2.9.  vector    Vectors  are  indexed  sequences  of
         data.   They  can  be used to implement a notion of
         user-defined types via  their  associated  property
         list.    They  make  hunks  (see  below)  logically
         unnecessary, although hunks  are  very  efficiently
         garbage  collected.  There is a second kind of vec-
         tor,  called  an  immediate-vector,  which   stores
         binary  data.   The  name  that  the  function _t_y_p_e
         returns   for   immediate-vectors    is    vectori.
         Immediate-vectors   could   be  used  to  implement
         strings and block-flonum arrays, for example.  Vec-
         tors  are  discussed  in  chapter 9.  The functions
         _n_e_w-_v_e_c_t_o_r, and _v_e_c_t_o_r, can be used to create  vec-
         tors.


8          ________________________________________________
           Subpart name   Get value   Set value    Type

8          ________________________________________________________________________________________________
             datum[_i]       vref        vset      lispval
8          ________________________________________________
             property       vprop     vsetprop    lispval
                                      vputprop
8          ________________________________________________
               size         vsize         -       fixnum
8          ________________________________________________
7         |7|7|7|7|7|7|7|






                       |7|7|7|7|7|7|7|






                                   |7|7|7|7|7|7|7|






                                               |7|7|7|7|7|7|7|






                                                         |7|7|7|7|7|7|7|











      1.2.10.  array   Arrays are rather  complicated  types
         and  are  fully  described  in Chapter 9.  An array
         consists of a block of contiguous data, a  function
         to  access  that data, and auxiliary fields for use
         by  the  accessing  function.   Since  an   array's
         accessing function is created by the user, an array
         can  have  any  form  the  user  chooses  (e.g.  n-
         dimensional, triangular, or hash table).
         Arrays are created by the function _m_a_r_r_a_y.


8   _______________________________________________________________
     Subpart name     Get value   Set value          Type

8   ______________________________________________________________________________________________________________________________
    access function   getaccess   putaccess      binary, list
                                                   or symbol
8   _______________________________________________________________
       auxiliary       getaux      putaux           lispval
8   _______________________________________________________________
         data         arrayref     replace    block of contiguous
                                     set            lispval
8   _______________________________________________________________
        length        getlength   putlength         fixnum
8   _______________________________________________________________
         delta        getdelta    putdelta          fixnum



9                                   Printed: January 31, 1984







FRANZ LISP                                               1-7


8   _______________________________________________________________
799  |9|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|
7777777777777777777777777777777777777777899                   |9|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|
7777777777777777777777777777777777777777899                               |9|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|
7777777777777777777777777777777777777777899                                           |9|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|
7777777777777777777777777777777777777777899                                                                 |9|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|99|
777777777777777777777777777777777777778


      1.2.11.  value   A value cell contains a pointer to  a
         lispval.   This  type  is  used mainly by arrays of
         general lisp objects.  Value cells are created with
         the  _p_t_r  function.   A  value  cell  containing  a
         pointer  to  the  symbol  `foo'   is   printed   as
         `(ptr to)foo'



      1.2.12.  hunk   A hunk is a vector of from  1  to  128
         lispvals.   Once  a  hunk  is  created  (by _h_u_n_k or
         _m_a_k_h_u_n_k) it cannot grow or shrink.  The access time
         for an element of a hunk is slower than a list cell
         element but faster than an array.  Hunks are really
         only  allocated  in  sizes which are powers of two,
         but can appear to the user to be any size in the  1
         to  128  range.   Users  of hunks must realize that
         (_n_o_t (_a_t_o_m '_l_i_s_p_v_a_l)) will return true  if  _l_i_s_p_v_a_l
         is  a hunk.  Most lisp systems do not have a direct
         test for a list cell and instead use the above test
         and  assume  that  a true result means _l_i_s_p_v_a_l is a
         list cell.  In FRANZ LISP you can use _d_t_p_r to check
         for  a  list  cell.   Although  hunks  are not list
         cells, you can still access the first two hunk ele-
         ments  with _c_d_r and _c_a_r and you can access any hunk
         element with _c_x_r[|-].  You can set the value of  the
         first two elements of a hunk with _r_p_l_a_c_d and _r_p_l_a_c_a
         and you can set the value of  any  element  of  the
         hunk  with  _r_p_l_a_c_x.   A hunk is printed by printing
         its contents surrounded by { and }.  However a hunk
         cannot  be read in in this way in the standard lisp
         system.  It is easy to write a reader macro  to  do
         this if desired.



      1.2.13.   other    Occasionally,  you  can  obtain   a
         pointer  to  storage not allocated by the lisp sys-
         tem.  One example of this is  the  entry  field  of
         those  FRANZ  LISP  functions  written  in C.  Such
         objects are classified as of type  other.   Foreign
         functions  which  call malloc to allocate their own
         space, may also inadvertantly create such  objects.
         The  garbage  collector  is supposed to ignore such
____________________
9   [|-]In a hunk, the function _c_d_r references the first  ele-
ment and _c_a_r the second.



                                   Printed: January 31, 1984







FRANZ LISP                                               1-8


         objects.



   1.3.  Documentation   The conventions used in the follow-
      ing  chapters  were  designed  to give a great deal of
      information in a brief space.  The  first  line  of  a
      function  description  contains  the  function name in
      bold face and then lists the arguments, if  any.   The
      arguments all have names which begin with a letter  or
      letters and an underscore.  The  letter(s)  gives  the
      allowable  type(s) for that argument according to this
      table.


8     _______________________________________________________
      Letter                Allowable type(s)

8     ______________________________________________________________________________________________________________
      g        any type
8     _______________________________________________________
      s        symbol (although nil may not be allowed)
8     _______________________________________________________
      t        string
8     _______________________________________________________
      l        list (although nil may be allowed)
8     _______________________________________________________
      n        number (fixnum, flonum, bignum)
8     _______________________________________________________
      i        integer (fixnum, bignum)
8     _______________________________________________________
      x        fixnum
8     _______________________________________________________
      b        bignum
8     _______________________________________________________
      f        flonum
8     _______________________________________________________
      u        function type (either binary or lambda body)
8     _______________________________________________________
      y        binary
8     _______________________________________________________
      v        vector
8     _______________________________________________________
      V        vectori
8     _______________________________________________________
      a        array
8     _______________________________________________________
      e        value
8     _______________________________________________________
      p        port (or nil)
8     _______________________________________________________
      h        hunk
8     _______________________________________________________
7    |7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|


























            |7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|


























                                                           |7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|





























      In the first line of  a  function  description,  those
      arguments preceded by a quote mark are evaluated (usu-
      ally before the function is called).  The quoting con-
      vention  is  used  so  that  we can give a name to the
      result of evaluating the argument and we can  describe
      the  allowable types.  If an argument is not quoted it
      does  not  mean  that  that  argument  will   not   be


9                                   Printed: January 31, 1984







FRANZ LISP                                               1-9


      evaluated,  but  rather  that  if it is evaluated, the
      time at which it is  evaluated  will  be  specifically
      mentioned in the function description.  Optional argu-
      ments are surrounded by square brackets.  An  ellipsis
      (...) means zero or more occurrences of an argument of
      the directly preceding type.














































9

9                                   Printed: January 31, 1984



