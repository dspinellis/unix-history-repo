






                        CHAPTER  12


                 Liszt - the lisp compiler






   12.1.  General strategy of the compiler

           The purpose of the lisp compiler,  Liszt,  is  to
      create  an  object  module which when brought into the
      lisp system using _f_a_s_l will have the  same  effect  as
      bringing in the corresponding lisp coded source module
      with _l_o_a_d with one important exception, functions will
      be  defined  as sequences of machine language instruc-
      tions, instead of lisp S-expressions.  Liszt is not  a
      function compiler, it is a _f_i_l_e compiler.  Such a file
      can contain more than  function  definitions;  it  can
      contain  other  lisp S-expressions which are evaluated
      at load time.  These other S-expressions will also  be
      stored in the object module produced by Liszt and will
      be evaluated at fasl time.

           As is almost universally true of Lisp  compilers,
      the  main  pass of Liszt is written in Lisp.  A subse-
      quent pass is the assembler,  for  which  we  use  the
      standard UNIX assembler.



   12.2.  Running the compiler

           The compiler is normally run in this manner:
      % liszt foo
      will compile the file foo.l or foo (the preferred  way
      to indicate a lisp source file is to end the file name
      with `.l').  The result of  the  compilation  will  be
      placed  in  the  file  foo.o   if no fatal errors were
      detected.  All messages which Liszt  generates  go  to
      the  standard  output.  Normally each function name is
      printed  before  it  is  compiled   (the   -q   option
      suppresses this).



   12.3.  Special forms

           Liszt makes one pass over  the  source  file.  It
      processes each form in this way:
9

9Liszt - the lisp compiler                               12-1







Liszt - the lisp compiler                               12-2


      12.3.1.  macro expansion

              If the form is a macro invocation (i.e it is a
         list  whose  car is a symbol whose function binding
         is  a  macro),  then  that  macro   invocation   is
         expanded.   This  is  repeated  until the top level
         form is not a macro invocation.  When Liszt begins,
         there are already some macros defined, in fact some
         functions (such as defun) are actually macros.  The
         user  may  define  his  own  macros as well.  For a
         macro to be used it must be  defined  in  the  Lisp
         system in which Liszt runs.



      12.3.2.  classification

              After all macro expansion is done, the form is
         classified according to its _c_a_r (if the form is not
         a list, then it is classified as an _o_t_h_e_r).



         12.3.2.1.  eval-when

                 The   form   of   eval-when    is    (_e_v_a_l-
            _w_h_e_n (_t_i_m_e_1 _t_i_m_e_2 ...) _f_o_r_m_1 _f_o_r_m_2 ...)    where
            the time_i are one of  _e_v_a_l,  _c_o_m_p_i_l_e,  or  _l_o_a_d.
            The  compiler examines the form_i in sequence and
            the action taken depends on what is in the  time
            list.   If  _c_o_m_p_i_l_e is in the list then the com-
            piler will invoke _e_v_a_l on each form_i as it exam-
            ines  it.   If _l_o_a_d is in the list then the com-
            pile will recursively  call  itself  to  compile
            each form_i as it examines it.  Note that if _c_o_m_-
            _p_i_l_e and _l_o_a_d are in the  time  list,  then  the
            compiler  will  both  evaluate  and compile each
            form.  This is useful if you need a function  to
            be  defined in the compiler at both compile time
            (perhaps to aid macro expansion) and at run time
            (after the file is _f_a_s_led in).



         12.3.2.2.  declare

                 Declare  is  used  to  provide  information
            about  functions  and variables to the compiler.
            It   is   (almost)    equivalent    to    (_e_v_a_l-
            _w_h_e_n (_c_o_m_p_i_l_e) ...).   You may declare functions
            to  be  one  of  three  types:  lambda  (*expr),
            nlambda  (*fexpr), lexpr (*lexpr).  The names in
            parenthesis  are  the  Maclisp  names  and   are


                                     Printed: August 5, 1983







Liszt - the lisp compiler                               12-3


            accepted  by  the compiler as well (and not just
            when the compiler is in  Maclisp  mode).   Func-
            tions  are  assumed to be lambdas until they are
            declared otherwise or are  defined  differently.
            The  compiler treats calls to lambdas and lexprs
            equivalently, so you needn't worry about declar-
            ing  lexprs  either.  It is important to declare
            nlambdas or define  them  before  calling  them.
            Another attribute you can declare for a function
            is localf which makes the function  `local'.   A
            local function's name is known only to the func-
            tions  defined  within  the  file  itself.   The
            advantage  of a local function is that is can be
            entered and exited very quickly and it can  have
            the  same name as a function in another file and
            there will be no name conflict.

                 Variables may be declared special or unspe-
            cial.   When  a special variable is lambda bound
            (either in a lambda, prog or do expression), its
            old  value  is  stored  away  on a stack for the
            duration of the lambda, prog or  do  expression.
            This  takes  time  and  is  often not necessary.
            Therefore the default classification  for  vari-
            ables  is  unspecial.  Space for unspecial vari-
            ables is dynamically allocated on a  stack.   An
            unspecial  variable  can  only  be accessed from
            within the function where it is created  by  its
            presence  in  a  lambda,  prog  or do expression
            variable list.  It is possible to  declare  that
            all  variables  are  special  as  will  be shown
            below.

                 You may declare any  number  of  things  in
            each declare statement.  A sample declaration is
            (_d_e_c_l_a_r_e
                 (_l_a_m_b_d_a _f_u_n_c_1 _f_u_n_c_2)
                 (*_f_e_x_p_r _f_u_n_c_3)
                 (*_l_e_x_p_r _f_u_n_c_4)
                 (_l_o_c_a_l_f _f_u_n_c_5)
                 (_s_p_e_c_i_a_l _v_a_r_1 _v_a_r_2 _v_a_r_3)
                 (_u_n_s_p_e_c_i_a_l _v_a_r_4))

                 You may also declare all  variables  to  be
            special  with  (_d_e_c_l_a_r_e (_s_p_e_c_i_a_l_s _t)).   You may
            declare that macro definitions  should  be  com-
            piled  as  well  as evaluated at compile time by
            (_d_e_c_l_a_r_e (_m_a_c_r_o_s _t)).  In fact, as was mentioned
            above,    declare    is    much    like   (_e_v_a_l-
            _w_h_e_n (_c_o_m_p_i_l_e) ...).  Thus if the compiler  sees
            (_d_e_c_l_a_r_e (_f_o_o _b_a_r))  and foo is defined, then it
            will evaluate (_f_o_o _b_a_r).  If foo is not  defined
            then an undefined declare attribute warning will


                                     Printed: August 5, 1983







Liszt - the lisp compiler                               12-4


            be issued.



         12.3.2.3.  (progn 'compile form1 form2 ... formn)

                 When the compiler sees this it simply  com-
            piles  form1  through  formn as if they too were
            seen at top level.  One use for this is to allow
            a  macro  at  top-level to expand into more than
            one function definition for the compiler to com-
            pile.



         12.3.2.4.  include/includef

                 _I_n_c_l_u_d_e and _i_n_c_l_u_d_e_f cause another file  to
            be  read  and  compiled  by  the  compiler.  The
            result is the same as if the included file  were
            textually  inserted into the original file.  The
            only difference between _i_n_c_l_u_d_e and _i_n_c_l_u_d_e_f  is
            that  include  doesn't evaluate its argument and
            includef does.  Nested includes are allowed.



         12.3.2.5.  def

                 A def form is used to  define  a  function.
            The  macros  _d_e_f_u_n  and _d_e_f_m_a_c_r_o expand to a def
            form.   If  the  function  being  defined  is  a
            lambda,  nlambda or lexpr then the compiler con-
            verts the  lisp  definition  to  a  sequence  of
            machine  language instructions.  If the function
            being defined is a macro, then the compiler will
            evaluate the definition, thus defining the macro
            withing the running Lisp compiler.  Furthermore,
            if  the  variable  _m_a_c_r_o_s  is  set  to a non nil
            value, then the macro definition  will  also  be
            translated  to machine language and thus will be
            defined when the object file is fasled in.   The
            variable    _m_a_c_r_o_s    is    set    to    t    by
            (_d_e_c_l_a_r_e (_m_a_c_r_o_s _t)).

                 When a function or macro definition is com-
            piled,  macro  expansion is done whenever possi-
            ble.  If the compiler can determine that a  form
            would  be evaluated if this function were inter-
            preted then it will macro expand  it.   It  will
            not  macro  expand arguments to a nlambda unless
            the characteristics of the nlambda is known  (as
            is  the  case  with  _c_o_n_d).  The map functions (


                                     Printed: August 5, 1983







Liszt - the lisp compiler                               12-5


            _m_a_p, _m_a_p_c, _m_a_p_c_a_r, and so on) are expanded to  a
            _d_o statement.  This allows the first argument to
            the map function to be a lambda expression which
            references local variables of the function being
            defined.



         12.3.2.6.  other forms

                 All other forms are simply  stored  in  the
            object  file  and are evaluated when the file is
            _f_a_s_led in.



   12.4.  Using the compiler

           The previous section describes exactly  what  the
      compiler  does  with  its  input.  Generally you won't
      have to worry about all that  detail  as  files  which
      work  interpreted  will work compiled.  Following is a
      list of steps you should follow to insure that a  file
      will compile correctly.

      [1]  Make sure all macro definitions precede their use
           in  functions or other macro definitions.  If you
           want the macros to be around when you _f_a_s_l in the
           object  file you should include this statement at
           the beginning of the file: (_d_e_c_l_a_r_e (_m_a_c_r_o_s _t))

      [2]  Make sure all nlambdas are  defined  or  declared
           before  they  are  used.   If  the compiler comes
           across a call to a function which  has  not  been
           defined  in  the  current  file,  which  does not
           currently have a function binding, and whose type
           has  not  been  declared then it will assume that
           the function needs  its arguments evaluated (i.e.
           it  is  a lambda or lexpr) and will generate code
           accordingly.  This means that you do not have  to
           declare  nlambda functions like _s_t_a_t_u_s since they
           have an nlambda function binding.

      [3]  Locate all variables which are used for  communi-
           cating values between functions.  These variables
           must be declared special at the  beginning  of  a
           file.   In most cases there won't be many special
           declarations but if you fail to declare  a  vari-
           able  special  that  should be, the compiled code
           could fail in mysterious ways.  Let's look  at  a
           common  problem, assume that a file contains just
           these three lines:
9

9                                     Printed: August 5, 1983







Liszt - the lisp compiler                               12-6


           (_d_e_f _a_a_a (_l_a_m_b_d_a (_g_l_o_b _l_o_c) (_b_b_b _l_o_c)))
           (_d_e_f _b_b_b (_l_a_m_b_d_a (_m_y_l_o_c) (_a_d_d _g_l_o_b _m_y_l_o_c)))
           (_d_e_f _c_c_c (_l_a_m_b_d_a (_g_l_o_b _l_o_c) (_b_b_b _l_o_c)))


           We can see that if we load in these  two  defini-
           tions then (aaa 3 4) is the same as (add 3 4) and
           will give us 7.  Suppose we compile the file con-
           taining  these  definitions.  When Liszt compiles
           aaa, it will assume that both glob  and  loc  are
           local  variables  and  will allocate space on the
           temporary stack for  their  values  when  aaa  is
           called.   Thus  the values of the local variables
           glob and loc will not affect the  values  of  the
           symbols  glob  and  loc  in the Lisp system.  Now
           Liszt moves on to function bbb.  Myloc is assumed
           to  be local.  When it sees the add statement, it
           find a reference to a variable called glob.  This
           variable is not a local variable to this function
           and therefore glob must refer to the value of the
           symbol  glob.   Liszt  will automatically declare
           glob to be special and it will print a warning to
           that  effect.   Thus subsequent uses of glob will
           always refer to the symbol glob.  Next Liszt com-
           piles ccc and treats glob as a special and loc as
           a local.  When the object file is _f_a_s_l'ed in, and
           (ccc  3  4) is evaluated, the symbol glob will be
           lambda bound to 3 bbb will  be  called  and  will
           return 7.  However (aaa 3 4) will fail since when
           bbb is called, glob will be unbound.  What should
           be done here is to put (_d_e_c_l_a_r_e (_s_p_e_c_i_a_l _g_l_o_b) at
           the beginning of the file.

      [4]  Make sure that all calls to _a_r_g  are  within  the
           lexpr  whose arguments they reference.  If _f_o_o is
           a compiled lexpr and it calls _b_a_r then _b_a_r cannot
           use  _a_r_g  to get at _f_o_o's arguments.  If both _f_o_o
           and _b_a_r are interpreted this will  work  however.
           The  macro _l_i_s_t_i_f_y can be used to put all of some
           of a lexprs arguments in a list which then can be
           passed to other functions.



   12.5.  Compiler options

           The compiler recognizes a number of options which
      are  described  below.  The options are typed anywhere
      on the command line preceded by  a  minus  sign.   The
      entire   command  line  is  scanned  and  all  options
      recorded before any action is taken.  Thus
      % liszt -mx foo
      % liszt -m -x foo


                                     Printed: August 5, 1983







Liszt - the lisp compiler                               12-7


      % liszt foo -mx
      are all equivalent. Before scanning the  command  line
      for  options,  liszt  looks for in the environment for
      the variable LISZT, and if found scans its value as if
      it  was  a  string  of  options.   The  meaning of the
      options are:

      C    The assembler language output of the compiler  is
           commented.   This  is  useful  when debugging the
           compiler and is not normally done since it  slows
           down compilation.

      I    The next command line  argument  is  taken  as  a
           filename, and loaded prior to compilation.

      e    Evaluate the next argument on  the  command  line
           before starting compilation.  For example
           % liszt -e '(setq foobar "foo string")' foo
           will evaluate the above s-expression.  Note  that
           the  shell  requires  that  the arguments be sur-
           rounded by single quotes.

      i    Compile this program in  interlisp  compatibility
           mode. This is not implemented yet.

      m    Compile this program in Maclisp mode.  The reader
           syntax  will be changed to the Maclisp syntax and
           a file of macro definitions  will  be  loaded  in
           (usually   named  /usr/lib/lisp/machacks).   This
           switch brings us sufficiently close to Maclisp to
           allow us to compile Macsyma, a large Maclisp pro-
           gram.  However Maclisp is a moving target and  we
           can't  guarantee  that this switch will allow you
           to compile any given program.

      o    Select a different object or  assembler  language
           file name.  For example
           % liszt foo -o xxx.o
           will compile foo and into xxx.o  instead  of  the
           default foo.o, and
           % liszt bar -S -o xxx.s
           will compile to  assembler  language  into  xxx.s
           instead of bar.s.

      p    place profiling code at  the  beginning  of  each
           non-local  function.   If the lisp system is also
           created with profiling in it, this  allows  func-
           tion  calling  frequency  to  be  determined (see
           _p_r_o_f(_1))

      q    Run in quiet mode. The names of  functions  being
           compiled and various "Note"'s are not printed.
9

9                                     Printed: August 5, 1983







Liszt - the lisp compiler                               12-8


      Q    print compilation statistics and warn of  strange
           constructs.  This  is the inverse of the q switch
           and is the default.

      r    place bootstrap code  at  the  beginning  of  the
           object  file,  which when the object file is exe-
           cuted will cause a lisp system to be invoked  and
           the  object  file  _f_a_s_led  in.  This  is known as
           `autorun' and is described below.

      S    Create an assembler language file only.
           % liszt -S foo
           will create  the  file  assembler  language  file
           foo.s  and  will  not attempt to assemble it.  If
           this  option  is  not  specified,  the  assembler
           language  file  will be put in the temporary disk
           area under a automatically generated  name  based
           on the lisp compiler's process id.  Then if there
           are no compilation errors, the assembler will  be
           invoked to assemble the file.

      T    Print the assembler language output on the  stan-
           dard  output file.  This is useful when debugging
           the compiler.

      u    Run in UCI-Lisp mode.  The  character  syntax  is
           changed to that of UCI-Lisp and a UCI-Lisp compa-
           tibility package of macros is read in.

      w    Suppress warning messages.

      x    Create an cross reference file.
           % liszt -x foo
           not only compiles foo into foo.o  but  also  gen-
           erates  the file foo.x .  The file foo.x  is lisp
           readable and lists for each  function  all  func-
           tions  which  that function could call.  The pro-
           gram lxref reads one or more of these ".x"  files
           and  produces  a  human  readable cross reference
           listing.



   12.6.  autorun

           The object  file which liszt writes does not con-
      tain  all the functions necessary to run the lisp pro-
      gram which was compiled.  In order to use  the  object
      file,  a  lisp  system  must be started and the object
      file _f_a_s_led in.  When the -r switch is given to liszt,
      the  object file created will contain a small piece of
      bootstrap code at the beginning, and the  object  file
      will  be  made  executable.  Now, when the name of the


                                     Printed: August 5, 1983







Liszt - the lisp compiler                               12-9


      object file is given to the UNIX  command  interpreter
      (shell) to run, the bootstrap code at the beginning of
      the object file will cause a lisp system to be started
      and  the first action the lisp system will  take is to
      _f_a_s_l in the object file which started it.   In  effect
      the object file has created an environment in which it
      can run.

           Autorun  is  an  alternative  to  _d_u_m_p_l_i_s_p.   The
      advantage  of  autorun  is  that the object file which
      starts the whole process is typically  small,  whereas
      the  minimum  _d_u_m_p_l_i_s_ped  file is very large (one half
      megabyte).  The disadvantage of autorun  is  that  the
      file  must  be _f_a_s_led into a lisp each time it is used
      whereas the file which _d_u_m_p_l_i_s_p creates can be run  as
      is.   liszt  itself  is  a _d_u_m_p_l_i_s_ped file since it is
      used so often and is large enough that too  much  time
      would  be  wasted _f_a_s_ling it in each time it was used.
      The lisp cross reference program, lxref, uses  _a_u_t_o_r_u_n
      since it is a small and rarely used program.

           In order to have the program _f_a_s_led in begin exe-
      cution  (rather  than  starting a lisp top level), the
      value of the symbol user-top-level should  be  set  to
      the  name  of the function to get control.  An example
      of this is shown next.


























9

9                                     Printed: August 5, 1983







Liszt - the lisp compiler                              12-10



    ____________________________________________________

    _w_e _w_a_n_t _t_o _r_e_p_l_a_c_e _t_h_e _u_n_i_x _d_a_t_e _p_r_o_g_r_a_m _w_i_t_h _o_n_e _w_r_i_t_t_e_n _i_n _l_i_s_p.

    % cat lispdate.l
    (defun mydate nil
       (patom "The date is ")
       (patom (status ctime))
       (terpr)
       (exit 0))
    (setq user-top-level 'mydate)

    % liszt -r lispdate
    Compilation begins with Lisp Compiler 5.2
    source: lispdate.l, result: lispdate.o
    mydate
    %Note: lispdate.l: Compilation complete
    %Note: lispdate.l:  Time: Real: 0:3, CPU: 0:0.28, GC: 0:0.00 for 0 gcs
    %Note: lispdate.l: Assembly begins
    %Note: lispdate.l: Assembly completed successfully
    3.0u 2.0s 0:17 29%

     _W_e _c_h_a_n_g_e _t_h_e _n_a_m_e _t_o _r_e_m_o_v_e _t_h_e "._o", (_t_h_i_s _i_s_n'_t _n_e_c_e_s_s_a_r_y)
    % mv lispdate.o lispdate

     _N_o_w _w_e _t_e_s_t _i_t _o_u_t
    % lispdate
    The date is Sat Aug  1 16:58:33 1981
    %
    ____________________________________________________






   12.7.  pure literals

           Normally the quoted lisp objects (literals) which
      appear in functions are treated as constants. Consider
      this function:

      (_d_e_f _f_o_o
         (_l_a_m_b_d_a _n_i_l (_c_o_n_d ((_n_o_t (_e_q '_a  (_c_a_r  (_s_e_t_q  _x  '(_a
      _b)))))
                            (_p_r_i_n_t '_i_m_p_o_s_s_i_b_l_e!!))
                           (_t (_r_p_l_a_c_a _x '_d)))))

      At first glance it seems that the  first  cond  clause
      will  never  be  true,  since  the _c_a_r of (_a _b) should
      always be _a.  However if you run this function  twice,
      it will print 'impossible!!' the second time.  This is


                                     Printed: August 5, 1983







Liszt - the lisp compiler                              12-11


      because the following clause modifies  the  'constant'
      list  (_a _b)  with the _r_p_l_a_c_a function.  Such modifica-
      tion of literal lisp objects  can  cause  programs  to
      behave  strangely as the above example shows, but more
      importantly it can cause garbage  collection  problems
      if  done  to compiled code.  When a file is _f_a_s_led in,
      if the symbol $purcopylits is  non  nil,  the  literal
      lisp  data  is  put in 'pure' space, that is it put in
      space which needn't be looked at by the garabage  col-
      lector.   This  reduces the work the garbage collector
      must do but it is dangerous since if the literals  are
      modified  to point to non pure objects, the marker may
      not mark the non pure objects.  If  the  symbol  $pur-
      copylits  is  nil then the literal lisp data is put in
      impure space and the compiled code will act  like  the
      interpreted  code  when literal data is modified.  The
      default value for $purcopylits is t.



   12.8.  transfer tables

           A transfer table is setup by _f_a_s_l when the object
      file is loaded in.  There is one entry in the transfer
      table for each function which is called in that object
      file.   The  entry  for a call to the function _f_o_o has
      two parts whose contents are:

      [1]  function address - This will initially  point  to
           the internal  function _q_l_i_n_k_e_r.  It may some time
           in the future point to the function _f_o_o  if  cer-
           tain  conditions  are  satisfied  (more  on  this
           below).

      [2]  function name - This is a pointer to  the  symbol
           _f_o_o.  This will be used by _q_l_i_n_k_e_r.



      When a call is made to the function _f_o_o the call  will
      actually  be made to the address in the transfer table
      entry  and  will  end  up  in  the  _q_l_i_n_k_e_r  function.
      _Q_l_i_n_k_e_r will determine that _f_o_o was the function being
      called by locating the  function  name  entry  in  the
      transfer table[].  If the function being called is not
      compiled  then  _q_l_i_n_k_e_r  just calls _f_u_n_c_a_l_l to perform
____________________
9   []_Q_l_i_n_k_e_r does this by tracing back the call stack  until
it finds the _c_a_l_l_s machine instruction which called it.  The
address field of the  _c_a_l_l_s  contains  the  address  of  the
transfer table entry.



9                                     Printed: August 5, 1983







Liszt - the lisp compiler                              12-12


      the  function  call.   If  _f_o_o  is  compiled  and   if
      (_s_t_a_t_u_s _t_r_a_n_s_l_i_n_k)  is  non  nil,  then  _q_l_i_n_k_e_r  will
      modify the function address part of the transfer table
      to  point  directly  to  the  function  _f_o_o.   Finally
      _q_l_i_n_k_e_r will call _f_o_o directly .  The next time a call
      is  made  to  _f_o_o the call will go directly to _f_o_o and
      not through _q_l_i_n_k_e_r.  This will result in  a  substan-
      tial   speedup  in  compiled  code  to  compiled  code
      transfers.  A disadvantage is that no debugging infor-
      mation is left on the stack, so _s_h_o_w_s_t_a_c_k and _b_a_k_t_r_a_c_e
      are useless.  Another  disadvantage  is  that  if  you
      redefine a compiled function either through loading in
      a new version or interactively defining it,  then  the
      old  version may still be called from compiled code if
      the fast linking  described  above  has  already  been
      done.   The  solution  to  these  problems  is  to use
      (_s_s_t_a_t_u_s _t_r_a_n_s_l_i_n_k _v_a_l_u_e).  If value is

      _n_i_l  All transfer tables will  be  cleared,  i.e.  all
           function  addresses  will  be  set  to  point  to
           _q_l_i_n_k_e_r.  This means that the next time  a  func-
           tion  is  called  _q_l_i_n_k_e_r will be called and will
           look at the current definition.   Also,  no  fast
           links  will  be  set  up since (_s_t_a_t_u_s _t_r_a_n_s_l_i_n_k)
           will be nil.  The end result  is  that  _s_h_o_w_s_t_a_c_k
           and  _b_a_k_t_r_a_c_e  will work and the function defini-
           tion at the time of call will always be used.

      _o_n   This causes the lisp system  to  go  through  all
           transfer  tables  and  set up fast links wherever
           possible.  This is normally used after  you  have
           _f_a_s_led  in  all  of your files. Furthermore since
           (_s_t_a_t_u_s _t_r_a_n_s_l_i_n_k) is not nil, _q_l_i_n_k_e_r will  make
           new  fast  links  if  the situation arises (which
           isn't likely unless you _f_a_s_l in another file).

      _t    This or any other value not previously  mentioned
           will just make (_s_t_a_t_u_s _t_r_a_n_s_l_i_n_k) be non nil, and
           as a result fast links will be made   by  _q_l_i_n_k_e_r
           if the called function is compiled.



   12.9.  Fixnum functions

           The compiler will generate inline arithmetic code
      for  fixnum only functions.  Such functions include +,
      -, *,  /, \, 1+ and 1-.  The code  generated  will  be
      much  faster than using _a_d_d, _d_i_f_f_e_r_e_n_c_e, etc.  However
      it will only work if the arguments to and  results  of
      the functions are fixnums.  No type checking is done.

9

9                                     Printed: August 5, 1983



