






                         CHAPTER  8


              Functions, Fclosures, and Macros






   8.1.  valid function objects

           There are many different objects which can occupy
      the  function field of a symbol object.  Table 8.1, on
      the following page, shows all  of  the  possibilities,
      how  to recognize them, and where to look for documen-
      tation.



   8.2.  functions

           The basic Lisp function is the  lambda  function.
      When a lambda function is called, the actual arguments
      are evaluated from left to right and are  lambda-bound
      to the formal parameters of the lambda function.

           An nlambda function is usually used for functions
      which  are  invoked  by  the  user at top level.  Some
      built-in functions which evaluate their  arguments  in
      special  ways  are  also  nlambdas (e.g _c_o_n_d, _d_o, _o_r).
      When an  nlambda  function  is  called,  the  list  of
      unevaluated  arguments  is  lambda bound to the single
      formal parameter of the nlambda function.

           Some programmers will  use  an  nlambda  function
      when  they  are  not  sure  how many arguments will be
      passed.  Then, the first thing  the  nlambda  function
      does  is  map  _e_v_a_l over the list of unevaluated argu-
      ments it has been passed.  This is usually  the  wrong
      thing  to  do,  as it will not work compiled if any of
      the arguments are local variables. The solution is  to
      use  a  lexpr.   When  a lexpr function is called, the
      arguments are evaluated and a fixnum  whose  value  is
      the  number of arguments is lambda-bound to the single
      formal parameter of the lexpr function.  The lexpr can
      then access the arguments using the _a_r_g function.

           When a function is compiled, _s_p_e_c_i_a_l declarations
      may  be  needed to preserve its behavior.  An argument
      is not lambda-bound to the name of  the  corresponding
      formal parameter unless that formal parameter has been
      declared _s_p_e_c_i_a_l (see 12.3.2.2).


Functions, Fclosures, and Macros                         8-1







Functions, Fclosures, and Macros                         8-2





8_________________________________________________________________
   informal name             object type          documentation
8__________________________________________________________________________________________________________________________________
    interpreted             list with _c_a_r              8.2
  lambda function           _e_q to lambda
8_________________________________________________________________
    interpreted             list with _c_a_r              8.2
  nlambda function          _e_q to nlambda
8_________________________________________________________________
    interpreted             list with _c_a_r              8.2
   lexpr function            _e_q to lexpr
8_________________________________________________________________
    interpreted             list with _c_a_r              8.3
       macro                 _e_q to macro
8_________________________________________________________________
      fclosure            vector with _v_p_r_o_p            8.4
                           _e_q to fclosure
8_________________________________________________________________
      compiled         binary with discipline          8.2
  lambda or lexpr           _e_q to lambda
      function
8_________________________________________________________________
      compiled         binary with discipline          8.2
  nlambda function          _e_q to nlambda
8_________________________________________________________________
      compiled         binary with discipline          8.3
       macro                 _e_q to macro
8_________________________________________________________________
      foreign          binary with discipline          8.5
     subroutine          of "subroutine"[|-]
8_________________________________________________________________
      foreign          binary with discipline          8.5
      function            of "function"[|-]
8_________________________________________________________________
      foreign          binary with discipline          8.5
  integer function    of "integer-function"[|-]
8_________________________________________________________________
      foreign          binary with discipline          8.5
   real function        of "real-function"[|-]
8_________________________________________________________________
      foreign          binary with discipline          8.5
     C function          of "c-function"[|-]
8_________________________________________________________________
      foreign          binary with discipline          8.5
  double function     of "double-c-function"[|-]
8_________________________________________________________________
      foreign          binary with discipline          8.5
 structure function   of "vector-c-function"[|-]
8_________________________________________________________________
       array                array object                9
8_________________________________________________________________
7|8|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|







































9                   |8|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|







































9                                               |8|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|







































9                                                                |8|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|









































9                         Table 8.1

____________________
9   [|-]Only the first character of the string is significant (i.e "s"
is ok for "subroutine")



9                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                         8-3


           Lambda and lexpr functions both  compile  into  a
      binary object with a discipline of lambda.  However, a
      compiled lexpr still acts like an interpreted lexpr.



   8.3.  macros

           An important feature of Lisp is  its  ability  to
      manipulate  programs  as  data.   As a result of this,
      most Lisp implementations  have  very  powerful  macro
      facilities.  The Lisp language's macro facility can be
      used to incorporate  popular  features  of  the  other
      languages  into  Lisp.   For  example, there are macro
      packages which allow one to create records  (ala  Pas-
      cal)  and  refer  to  elements of those records by the
      field names.  The _s_t_r_u_c_t package imported from Maclisp
      does  this.   Another  popular  use  for  macros is to
      create more readable control structures  which  expand
      into  _c_o_n_d,  _o_r  and  _a_n_d.  One such example is the If
      macro.  It allows you to write

      (_I_f (_e_q_u_a_l _n_u_m_b _0) _t_h_e_n (_p_r_i_n_t '_z_e_r_o) (_t_e_r_p_r)
       _e_l_s_e_i_f (_e_q_u_a_l _n_u_m_b _1) _t_h_e_n (_p_r_i_n_t '_o_n_e) (_t_e_r_p_r)
       _e_l_s_e (_p_r_i_n_t '|_I _g_i_v_e _u_p|))

      which expands to

      (_c_o_n_d
          ((_e_q_u_a_l _n_u_m_b _0) (_p_r_i_n_t '_z_e_r_o) (_t_e_r_p_r))
          ((_e_q_u_a_l _n_u_m_b _1) (_p_r_i_n_t '_o_n_e) (_t_e_r_p_r))
          (_t (_p_r_i_n_t '|_I _g_i_v_e _u_p|)))




      8.3.1.  macro forms

              A macro is a function  which  accepts  a  Lisp
         expression   as  input  and  returns  another  Lisp
         expression.  The action the macro takes  is  called
         macro expansion.  Here is a simple example:

         -> (_d_e_f _f_i_r_s_t (_m_a_c_r_o (_x) (_c_o_n_s '_c_a_r (_c_d_r _x))))
         first
         -> (_f_i_r_s_t '(_a _b _c))
         a
         -> (_a_p_p_l_y '_f_i_r_s_t '(_f_i_r_s_t '(_a _b _c)))
         (car '(a b c))

         The first input line defines a macro called  _f_i_r_s_t.
         Notice  that the macro has one formal parameter, _x.
         On the second input line, we ask the interpreter to


                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                         8-4


         evaluate  (_f_i_r_s_t '(_a _b _c)).   _E_v_a_l  sees that _f_i_r_s_t
         has a function definition  of  type  macro,  so  it
         evaluates  _f_i_r_s_t's definition, passing to _f_i_r_s_t, as
         an argument, the form _e_v_a_l  itself  was  trying  to
         evaluate:  (_f_i_r_s_t '(_a _b _c)).  The _f_i_r_s_t macro chops
         off the car of the argument with _c_d_r, cons'  a  _c_a_r
         at   the   beginning   of   the  list  and  returns
         (_c_a_r '(_a _b _c)), which _e_v_a_l evaluates.  The value  _a
         is returned as the value of (_f_i_r_s_t '(_a _b _c)).  Thus
         whenever _e_v_a_l tries to evaluate a  list  whose  car
         has  a macro definition it ends up doing (at least)
         two operations, the first of which is a call to the
         macro  to  let  it  macro  expand the form, and the
         other is the evaluation of the result of the macro.
         The  result of the macro may be yet another call to
         a macro, so _e_v_a_l may have to do even  more  evalua-
         tions  until it can finally determine the  value of
         an expression.  One way to see  how  a  macro  will
         expand  is to use _a_p_p_l_y as shown on the third input
         line above.



      8.3.2.  defmacro

              The macro _d_e_f_m_a_c_r_o makes it easier  to  define
         macros  because it allows you to name the arguments
         to the macro call.  For example,  suppose  we  find
         ourselves      often      writing     code     like
         (_s_e_t_q _s_t_a_c_k (_c_o_n_s _n_e_w_e_l_t _s_t_a_c_k).  We could define a
         macro  named  _p_u_s_h  to  do this for us.  One way to
         define it is:

         -> (_d_e_f _p_u_s_h
               (_m_a_c_r_o (_x) (_l_i_s_t '_s_e_t_q (_c_a_d_d_r _x) (_l_i_s_t '_c_o_n_s (_c_a_d_r _x) (_c_a_d_d_r _x)))))
         push

         then (_p_u_s_h _n_e_w_e_l_t _s_t_a_c_k) will expand  to  the  form
         mentioned above.  The same macro written using def-
         macro would be:

         -> (_d_e_f_m_a_c_r_o _p_u_s_h (_v_a_l_u_e _s_t_a_c_k)
           (_l_i_s_t '_s_e_t_q ,_s_t_a_c_k (_l_i_s_t '_c_o_n_s ,_v_a_l_u_e ,_s_t_a_c_k)))
         push

         Defmacro allows you to name the  arguments  of  the
         macro  call,  and  makes  the macro definition look
         more like a function definition.




9

9                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                         8-5


      8.3.3.  the backquote character macro

              The default syntax for  FRANZ  LISP  has  four
         characters  with  associated character macros.  One
         is semicolon for  comments.   Two  others  are  the
         backquote and comma which are used by the backquote
         character macro.  The  fourth  is  the  sharp  sign
         macro described in the next section.

              The backquote macro is used  to  create  lists
         where many of the elements are fixed (quoted). This
         makes it very useful  for  creating  macro  defini-
         tions.  In the simplest case, a backquote acts just
         like a single quote:

         ->`(_a _b _c _d _e)
         (a b c d e)

         If a comma precedes an element of a backquoted list
         then that element is evaluated and its value is put
         in the list.

         ->(_s_e_t_q _d '(_x _y _z))
         (x y z)
         ->`(_a _b _c ,_d _e)
         (a b c (x y z) e)

         If a comma followed by an at sign precedes an  ele-
         ment  in  a  backquoted  list, then that element is
         evaluated and spliced into the list with _a_p_p_e_n_d.

         ->`(_a _b _c ,@_d _e)
         (a b c x y z e)

         Once a list begins with a backquote, the commas may
         appear anywhere in the list as this example shows:

         ->`(_a _b (_c _d ,(_c_d_r _d)) (_e _f (_g _h ,@(_c_d_d_r _d) ,@_d)))
         (a b (c d (y z)) (e f (g h z x y z)))

         It is also possible and sometimes  even  useful  to
         use  the backquote macro within itself.  As a final
         demonstration of  the  backquote  macro,  we  shall
         define  the  first  and  push  macros using all the
         power at our disposal: defmacro and  the  backquote
         macro.

         ->(_d_e_f_m_a_c_r_o _f_i_r_s_t (_l_i_s_t) `(_c_a_r ,_l_i_s_t))
         first
         ->(_d_e_f_m_a_c_r_o _p_u_s_h (_v_a_l_u_e _s_t_a_c_k) `(_s_e_t_q ,_s_t_a_c_k (_c_o_n_s ,_v_a_l_u_e ,_s_t_a_c_k)))
         stack

9

9                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                         8-6


      8.3.4.  sharp sign character macro

              The sharp sign macro can perform a  number  of
         different  functions   at read time.  The character
         directly following the sharp sign determines  which
         function  will  be  done,  and  following  Lisp  s-
         expressions may serve as arguments.



         8.3.4.1.  conditional inclusion

            If you plan to run one source file in more  than
            one environment then you may want to some pieces
            of code to be included  or not included  depend-
            ing  on  the  environment.  The  C language uses
            "#ifdef" and "#ifndef"  for  this  purpose,  and
            Lisp  uses  "#+" and "#-".  The environment that
            the   sharp   sign   macro   checks    is    the
            (_s_t_a_t_u_s _f_e_a_t_u_r_e_s) list which is initialized when
            the Lisp system  is  built   and  which  may  be
            altered     by     (_s_s_t_a_t_u_s _f_e_a_t_u_r_e _f_o_o)     and
            (_s_s_t_a_t_u_s _n_o_f_e_a_t_u_r_e _b_a_r) The form  of conditional
            inclusion is
                        _#_+_w_h_e_n _w_h_a_t
            where _w_h_e_n is either a symbol or  an  expression
            involving symbols and the functions _a_n_d, _o_r, and
            _n_o_t.  The meaning is that _w_h_a_t will only be read
            in  if  _w_h_e_n  is true.  A symbol in _w_h_e_n is true
            only if  it  appears  in  the  (_s_t_a_t_u_s _f_e_a_t_u_r_e_s)
            list.


    ____________________________________________________

    ; suppose we want to write a program which references a file
    ; and which can run at ucb, ucsd and cmu where the file naming conventions
    ; are different.
    ;
    -> (_d_e_f_u_n _h_o_w_o_l_d (_n_a_m_e)
          (_t_e_r_p_r)
          (_l_o_a_d #+(_o_r _u_c_b _u_c_s_d) "/_u_s_r/_l_i_b/_l_i_s_p/_a_g_e_s._l"
                 #+_c_m_u "/_u_s_r/_l_i_s_p/_d_o_c/_a_g_e_s._l")
          (_p_a_t_o_m _n_a_m_e)
          (_p_a_t_o_m " _i_s ")
          (_p_r_i_n_t (_c_d_r (_a_s_s_o_c _n_a_m_e _a_g_e_f_i_l_e)))
          (_p_a_t_o_m "_y_e_a_r_s _o_l_d")
          (_t_e_r_p_r))
    ____________________________________________________



The form


                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                         8-7


                        _#_-_w_h_e_n _w_h_a_t
is equivalent to
                     _#_+_(_n_o_t _w_h_e_n_) _w_h_a_t



         8.3.4.2.  fixnum character equivalents

            When working with fixnum equivalents of  charac-
            ters,  it  is  often hard to remember the number
            corresponding to a character.  The form
                            _#_/_c
            is equivalent to the  fixnum  representation  of
            character c.


    ____________________________________________________

    ; a function which returns t if the user types y else it returns nil.
    ;
    -> (_d_e_f_u_n _y_e_s_o_r_n_o _n_i_l
          (_p_r_o_g_n (_a_n_s)
                 (_s_e_t_q _a_n_s (_t_y_i))
                 (_c_o_n_d ((_e_q_u_a_l _a_n_s #/_y) _t)
                       (_t _n_i_l))))
    ____________________________________________________






         8.3.4.3.  read time evaluation

            Occasionally you want to express a constant as a
            Lisp  expression,  yet you don't want to pay the
            penalty of evaluating this expression each  time
            it is referenced.  The form
                        _#_._e_x_p_r_e_s_s_i_o_n
            evaluates  the  expression  at  read  time   and
            returns its value.











9

9                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                         8-8



    ____________________________________________________

    ; a function to test if any of bits 1 3 or 12 are set in a fixnum.
    ;
    -> (_d_e_f_u_n _t_e_s_t_i_t (_n_u_m)
          (_c_o_n_d ((_z_e_r_o_p (_b_o_o_l_e _1 _n_u_m #.(+ (_l_s_h _1 _1) (_l_s_h _1 _3) (_l_s_h _1 _1_2))))
                 _n_i_l)
                (_t _t)))
    ____________________________________________________






   8.4.  fclosures

           Fclosures are a type of functional  object.   The
      purpose  is  to  remember the values of some variables
      between invocations of the functional  object  and  to
      protect this data from being inadvertently overwritten
      by other Lisp  functions.   Fortran  programs  usually
      exhibit  this behavior for their variables.  (In fact,
      some versions of Fortran would require  the  variables
      to  be  in COMMON).  Thus it is easy to write a linear
      congruent random number generator in  Fortran,  merely
      by keeping the seed as a variable in the function.  It
      is much more risky to do so in Lisp, since any special
      variable you picked, might be used by some other func-
      tion.  Fclosures are an attempt to provide most of the
      same  functionality  as closures in Lisp Machine Lisp,
      to users of FRANZ LISP.  Fclosures are related to clo-
      sures in this way:
      (fclosure '(a b) 'foo) <==>
              (let ((a a) (b b)) (closure '(a b) 'foo))



      8.4.1.  an example

____________________________________________________________

% lisp
Franz Lisp, Opus 38.60
->(defun code (me count)
  (print (list 'in x))
  (setq x (+ 1 x))
  (cond ((greaterp count 1) (funcall me me (sub1 count))))
  (print (list 'out x)))
code
->(defun tester (object count)
  (funcall object object count) (terpri))


                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                         8-9


tester
->(setq x 0)
0
->(setq z (fclosure '(x) 'code))
fclosure[8]
-> (tester z 3)
(in 0)(in 1)(in 2)(out 3)(out 3)(out 3)
nil
->x
0
____________________________________________________________





              The function _f_c_l_o_s_u_r_e  creates  a  new  object
         that  we  will  call  an  fclosure, (although it is
         actually a vector).  The fclosure contains a  func-
         tional  object, and a set of symbols and values for
         the symbols.  In the above  example,  the  fclosure
         functional object is the function code.  The set of
         symbols and values just contains the symbol `x' and
         zero,  the  value  of  `x'  when  the  fclosure was
         created.

         When an fclosure is funcall'ed:

         1)   The Lisp system lambda binds  the  symbols  in
              the fclosure to their values in the fclosure.

         2)   It continues the  funcall  on  the  functional
              object of the fclosure.

         3)   Finally, it un-lambda binds the symbols in the
              fclosure  and  at  the  same  time  stores the
              current values of the symbols in the fclosure.


              Notice that the fclosure is saving  the  value
         of  the  symbol  `x'.   Each  time  a  fclosure  is
         created, new space  is  allocated  for  saving  the
         values  of the symbols. Thus if we execute fclosure
         again, over the same  function,  we  can  have  two
         independent counters:

____________________________________________________________

-> (setq zz (fclosure '(x) 'code))
fclosure[1]
-> (tester zz 2)
(in 0)(in 1)(out 2)(out 2)
-> (tester zz 2)


                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                        8-10


(in 2)(in 3)(out 4)(out 4)
-> (tester z 3)
(in 3)(in 4)(in 5)(out 6)(out 6)(out 6)
____________________________________________________________







      8.4.2.  useful functions

              Here are some quick some  summaries  of  func-
         tions  dealing  with  closures.  They are more for-
         mally defined in 2.8.4.  To  recap,  fclosures  are
         made by (_f_c_l_o_s_u_r_e '_l__v_a_r_s '_g__f_u_n_c_o_b_j).  l_vars is a
         list of symbols (not containing nil), g_funcobj  is
         any  object  that can be funcalled.  (Objects which
         can be funcalled, include compiled Lisp  functions,
         lambda  expressions,  symbols,  foreign  functions,
         etc.) In general, if you want a  compiled  function
         to  be closed over a variable, you must declare the
         variable  to  be  special  within   the   function.
         Another example would be:

                     (fclosure '(a b) #'(lambda (x) (plus x a)))

         Here, the #' construction will  make  the  compiler
         compile the lambda expression.

              There are times when you want to  share  vari-
         ables  between  fclosures.  This can be done if the
         fclosures  are  created  at  the  same  time  using
         _f_c_l_o_s_u_r_e-_l_i_s_t.  The function _f_c_l_o_s_u_r_e-_a_l_i_s_t returns
         an assoc list giving the symbols and values in  the
         fclosure.   The  predicate  _f_c_l_o_s_u_r_e_p returns t iff
         its  argument  is  a  fclosure.   Other   functions
         imported  from  Lisp  Machine  Lisp are _s_y_m_e_v_a_l-_i_n-
         _f_c_l_o_s_u_r_e,   _l_e_t-_f_c_l_o_s_e_d,    and    _s_e_t-_i_n-_f_c_l_o_s_u_r_e.
         Lastly,  the function _f_c_l_o_s_u_r_e-_f_u_n_c_t_i_o_n returns the
         function argument.



      8.4.3.  internal structure

              Currently, closures are  implemented  as  vec-
         tors, with property being the symbol fclosure.  The
         functional object is the first entry.  The  remain-
         ing  entries are structures which point to the sym-
         bols and values for the closure, (with a  reference
         count  to  determine  if  a  recursive  closure  is


                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                        8-11


         active).



   8.5.  foreign subroutines and functions

           FRANZ LISP has the ability  to  dynamically  load
      object  files  produced by other compilers and to call
      functions defined in those files.  These functions are
      called _f_o_r_e_i_g_n functions.* There are  seven  types  of
      foreign functions.  They are characterized by the type
      of result they  return,  and  by  differences  in  the
      interpretation of their arguments.  They come from two
      families: a group  suited  for  languages  which  pass
      arguments  by  reference  (e.g.  Fortran), and a group
      suited for languages which  pass  arguments  by  value
      (e.g. C).


      There are four types in the first group:

      subroutine
           This does not return anything.  The  Lisp  system
           always returns t after calling a subroutine.

      function
           This returns whatever the function returns.  This
           must  be  a valid Lisp object or it may cause the
           Lisp system to fail.

      integer-function
           This returns an integer  which  the  Lisp  system
           makes into a fixnum and returns.

      real-function
           This returns a double precision real number which
           the Lisp system makes into a flonum and returns.


      There are three types in the second group:

      c-function
           This is like an integer function, except for  its
           different interpretation of arguments.


____________________
9   *This topic is also discussed in Report  PAM-124  of  the
Center  for  Pure  and  Applied  Mathematics,  UCB, entitled
``Parlez-Vous Franz?  An Informal Introduction to  Interfac-
ing Foreign Functions to Franz LISP'', by James R. Larus



9                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                        8-12


      double-c-function
           This is like a real-function.

      vector-c-function
           This is for C functions which return a structure.
           The  first  argument  to such functions must be a
           vector (of type vectori), into which  the  result
           is  stored.  The second Lisp argument becomes the
           first argument to the C function, and so on

      A foreign function is accessed through a binary object
      just like a compiled Lisp function.  The difference is
      that the discipline field of a  binary  object  for  a
      foreign  function is a string whose first character is
      given in the following table:


8                   ____________________________
                    letter         type
8                   ________________________________________________________
                      s         subroutine
8                   ____________________________
                      f          function
8                   ____________________________
                      i      integer-function
8                   ____________________________
                      r       real-function.
8                   ____________________________
                      c         c-function
8                   ____________________________
                      v      vector-c-function
8                   ____________________________
                      d      double-c-function
8                   ____________________________
7                  |7|7|7|7|7|7|7|7|7|7|7|










                          |7|7|7|7|7|7|7|7|7|7|7|










                                              |7|7|7|7|7|7|7|7|7|7|7|












      Two functions  are  provided  for  setting-up  foreign
      functions.   _C_f_a_s_l  loads an object file into the Lisp
      system and sets up one foreign function binary object.
      If there are more than one function in an object file,
      _g_e_t_a_d_d_r_e_s_s can be used to set  up  additional  foreign
      function objects.

           Foreign  functions are  called  just  like  other
      functions,  e.g  (_f_u_n_n_a_m_e _a_r_g_1 _a_r_g_2).  When a function
      in the Fortran group  is  called,  the  arguments  are
      evaluated  and  then  examined.  List, hunk and symbol
      arguments are passed unchanged to  the  foreign  func-
      tion.   Fixnum  and flonum arguments are copied into a
      temporary location and  a  pointer  to  the  value  is
      passed (this is because Fortran uses call by reference
      and it is dangerous to modify the contents of a fixnum
      or  flonum  which  something else might point to).  If
      the argument is an array object, the data field of the
      array  object  is passed to the foreign function (This
      is the easiest way to send large amounts  of  data  to
      and receive large amounts of data from a foreign func-
      tion).  If a binary object is an argument,  the  entry


9                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                        8-13


      field of that object is passed to the foreign function
      (the entry field is the address of a function, so this
      amounts to passing a function as an argument).

           When a function in the C group is called,  fixnum
      and flownum arguments are passed by value.  For almost
      all other arguments, the address is merely provided to
      the  C  routine.   The  only exception arises when you
      want to invoke a C routine which  expects  a  ``struc-
      ture''  argument.  Recall that a (rarely used) feature
      of the C language is the ability to pass structures by
      value.   This  copies  the  structure  onto the stack.
      Since the Franz's nearest equivalent to a C  structure
      is  a  vector, we provide an escape clause to copy the
      contents of an immediate-type vector by value.  If the
      property  field  of  a vectori argument, is the symbol
      "value-structure-argument", then the  binary  data  of
      this immediate-type vector is copied into the argument
      list of the C routine.

           The method a foreign function uses to access  the
      arguments   provided  by  Lisp  is  dependent  on  the
      language  of  the  foreign  function.   The  following
      scripts  demonstrate  how  how  Lisp can interact with
      three languages: C, Pascal and Fortran.  C and  Pascal
      have  pointer  types and the first script shows how to
      use pointers to extract information from Lisp objects.
      There  are  two  functions  defined for each language.
      The first (cfoo in C, pfoo in Pascal)  is  given  four
      arguments,  a  fixnum, a flonum-block array, a hunk of
      at least two fixnums and a list of at least  two  fix-
      nums.   To  demonstrate  that  the values were passed,
      each ?foo function prints its arguments (or  parts  of
      them).   The  ?foo  function  then modifies the second
      element of the flonum-block array and returns a  3  to
      Lisp.   The second function (cmemq in C, pmemq in Pas-
      cal) acts just like the Lisp _m_e_m_q function (except  it
      won't work for fixnums whereas the lisp _m_e_m_q will work
      for small fixnums).  In the script, typed input is  in
      bold,  computer output is in roman and comments are in
      _i_t_a_l_i_c.


____________________________________________________________

_T_h_e_s_e _a_r_e _t_h_e _C _c_o_d_e_d _f_u_n_c_t_i_o_n_s
% cat ch8auxc.c
/* demonstration of c coded foreign integer-function */

/* the following will be used to extract fixnums out of a list of fixnums */
struct listoffixnumscell
{    struct listoffixnumscell *cdr;
     int *fixnum;


                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                        8-14


};

struct listcell
{       struct listcell *cdr;
        int car;
};

cfoo(a,b,c,d)
int *a;
double b[];
int *c[];
struct listoffixnumscell *d;
{
    printf("a: %d, b[0]: %f, b[1]: %f0, *a, b[0], b[1]);
    printf(" c (first): %d   c (second): %d0,
               *c[0],*c[1]);
    printf(" ( %d %d ... ) ", *(d->fixnum), *(d->cdr->fixnum));
    b[1] = 3.1415926;
    return(3);
}

struct listcell *
cmemq(element,list)
int element;
struct listcell *list;
{
   for( ; list && element != list->car ;  list = list->cdr);
   return(list);
}


_T_h_e_s_e _a_r_e _t_h_e _P_a_s_c_a_l _c_o_d_e_d _f_u_n_c_t_i_o_n_s
% cat ch8auxp.p
type    pinteger = ^integer;
        realarray = array[0..10] of real;
        pintarray = array[0..10] of pinteger;
        listoffixnumscell = record
                                cdr  : ^listoffixnumscell;
                                fixnum : pinteger;
                            end;
        plistcell = ^listcell;
        listcell = record
                      cdr : plistcell;
                      car : integer;
                   end;

function pfoo ( var a : integer ;
                var b : realarray;
                var c : pintarray;
                var d : listoffixnumscell) : integer;
begin
   writeln(' a:',a, ' b[0]:', b[0], ' b[1]:', b[1]);
   writeln(' c (first):', c[0]^,' c (second):', c[1]^);


                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                        8-15


   writeln(' ( ', d.fixnum^, d.cdr^.fixnum^, ' ...) ');
   b[1] := 3.1415926;
   pfoo := 3
end ;

{ the function pmemq looks for the Lisp pointer given as the first argument
  in the list pointed to by the second argument.
  Note that we declare " a : integer " instead of " var a : integer " since
  we are interested in the pointer value instead of what it points to (which
  could be any Lisp object)
}
function pmemq( a : integer; list : plistcell) : plistcell;
begin
 while (list <> nil) and (list^.car <> a) do list := list^.cdr;
 pmemq := list;
end ;


_T_h_e _f_i_l_e_s _a_r_e _c_o_m_p_i_l_e_d
% cc -c ch8auxc.c
1.0u 1.2s 0:15 14% 30+39k 33+20io 147pf+0w
% pc -c ch8auxp.p
3.0u 1.7s 0:37 12% 27+32k 53+32io 143pf+0w


% lisp
Franz Lisp, Opus 38.60
_F_i_r_s_t _t_h_e _f_i_l_e_s _a_r_e _l_o_a_d_e_d _a_n_d _w_e _s_e_t _u_p _o_n_e  _f_o_r_e_i_g_n  _f_u_n_c_-
_t_i_o_n  _b_i_n_a_r_y.  _W_e _h_a_v_e _t_w_o _f_u_n_c_t_i_o_n_s _i_n _e_a_c_h _f_i_l_e _s_o _w_e _m_u_s_t
_c_h_o_o_s_e _o_n_e _t_o _t_e_l_l _c_f_a_s_l _a_b_o_u_t.  _T_h_e _c_h_o_i_c_e _i_s _a_r_b_i_t_r_a_r_y.
-> (cfasl 'ch8auxc.o '_cfoo 'cfoo "integer-function")
/usr/lib/lisp/nld -N -A /usr/local/lisp -T 63000 ch8auxc.o -e _cfoo -o /tmp/Li7055.0  -lc
#63000-"integer-function"
-> (cfasl 'ch8auxp.o '_pfoo 'pfoo "integer-function" "-lpc")
/usr/lib/lisp/nld -N -A /tmp/Li7055.0 -T 63200 ch8auxp.o -e _pfoo -o /tmp/Li7055.1 -lpc -lc
#63200-"integer-function"
_H_e_r_e _w_e _s_e_t _u_p _t_h_e _o_t_h_e_r _f_o_r_e_i_g_n _f_u_n_c_t_i_o_n _b_i_n_a_r_y _o_b_j_e_c_t_s
-> (getaddress '_cmemq 'cmemq "function" '_pmemq 'pmemq "function")
#6306c-"function"
_W_e _w_a_n_t _t_o _c_r_e_a_t_e _a_n_d _i_n_i_t_i_a_l_i_z_e _a_n _a_r_r_a_y  _t_o  _p_a_s_s  _t_o  _t_h_e
_c_f_o_o  _f_u_n_c_t_i_o_n.  _I_n _t_h_i_s _c_a_s_e _w_e _c_r_e_a_t_e _a_n _u_n_n_a_m_e_d _a_r_r_a_y _a_n_d
_s_t_o_r_e _i_t _i_n _t_h_e _v_a_l_u_e _c_e_l_l _o_f _t_e_s_t_a_r_r.  _W_h_e_n  _w_e  _c_r_e_a_t_e  _a_n
_a_r_r_a_y  _t_o  _p_a_s_s  _t_o  _t_h_e  _P_a_s_c_a_l _p_r_o_g_r_a_m _w_e _w_i_l_l _u_s_e _a _n_a_m_e_d
_a_r_r_a_y _j_u_s_t _t_o _d_e_m_o_n_s_t_r_a_t_e _t_h_e _d_i_f_f_e_r_e_n_t _w_a_y _t_h_a_t  _n_a_m_e_d  _a_n_d
_u_n_n_a_m_e_d _a_r_r_a_y_s _a_r_e _c_r_e_a_t_e_d _a_n_d _a_c_c_e_s_s_e_d.
-> (setq testarr (array nil flonum-block 2))
array[2]
-> (store (funcall testarr 0) 1.234)
1.234
-> (store (funcall testarr 1) 5.678)
5.678
-> (cfoo 385 testarr (hunk 10 11 13 14) '(15 16 17))
a: 385, b[0]: 1.234000, b[1]: 5.678000


                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                        8-16


 c (first): 10   c (second): 11
 ( 15 16 ... )
 3
_N_o_t_e _t_h_a_t _c_f_o_o _h_a_s _r_e_t_u_r_n_e_d _3 _a_s _i_t _s_h_o_u_l_d.  _I_t _a_l_s_o _h_a_d _t_h_e
_s_i_d_e  _e_f_f_e_c_t  _o_f  _c_h_a_n_g_i_n_g  _t_h_e _s_e_c_o_n_d _v_a_l_u_e _o_f _t_h_e _a_r_r_a_y _t_o
_3._1_4_1_5_9_2_6  _w_h_i_c_h _c_h_e_c_k _n_e_x_t.
-> (funcall testarr 1)
3.1415926


_I_n _p_r_e_p_a_r_a_t_i_o_n _f_o_r _c_a_l_l_i_n_g _p_f_o_o _w_e _c_r_e_a_t_e _a_n _a_r_r_a_y.
-> (array test flonum-block 2)
array[2]
-> (store (test 0) 1.234)
1.234
-> (store (test 1) 5.678)
5.678
-> (pfoo 385 (getd 'test) (hunk 10 11 13 14) '(15 16 17))
 a:       385 b[0]:  1.23400000000000E+00 b[1]:  5.67800000000000E+00
 c (first):        10 c (second):        11
 (         15        16 ...)
3
-> (test 1)
3.1415926

 _N_o_w _t_o _t_e_s_t _o_u_t _t_h_e _m_e_m_q'_s
-> (cmemq 'a '(b c a d e f))
(_a _d _e _f)
-> (pmemq 'e '(a d f g a x))
_n_i_l
____________________________________________________________





           The Fortran example will be much shorter since in
      Fortran  you can't follow pointers as you can in other
      languages.  The Fortran function ffoo is  given  three
      arguments:  a  fixnum, a fixnum-block array and a flo-
      num.  These arguments are printed out to  verify  that
      they  made it and then the first value of the array is
      modified.  The function  returns  a  double  precision
      value  which  is  converted  to  a  flonum by lisp and
      printed.  Note that the entry point  corresponding  to
      the  Fortran function ffoo is _ffoo_ as opposed to the
      C and Pascal convention of preceding the name with  an
      underscore.

____________________________________________________________


% cat ch8auxf.f


                                   Printed: January 31, 1984







Functions, Fclosures, and Macros                        8-17


        double precision function ffoo(a,b,c)
        integer a,b(10)
        double precision c
        print 2,a,b(1),b(2),c
2       format(' a=',i4,', b(1)=',i5,', b(2)=',i5,' c=',f6.4)
        b(1) = 22
        ffoo = 1.23456
        return
        end
% f77 -c ch8auxf.f
ch8auxf.f:
   ffoo:
0.9u 1.8s 0:12 22% 20+22k 54+48io 158pf+0w
% lisp
Franz Lisp, Opus 38.60
-> (cfasl 'ch8auxf.o '_ffoo_ 'ffoo "real-function" "-lF77 -lI77")
/usr/lib/lisp/nld -N -A /usr/local/lisp -T 63000 ch8auxf.o -e _ffoo_
-o /tmp/Li11066.0 -lF77 -lI77 -lc
#6307c-"real-function"

-> (array test fixnum-block 2)
array[2]
-> (store (test 0) 10)
10
-> (store (test 1) 11)
11
-> (ffoo 385 (getd 'test) 5.678)
 a= 385, b(1)=   10, b(2)=   11 c=5.6780
1.234559893608093
-> (test 0)
22

____________________________________________________________



















9

9                                   Printed: January 31, 1984



