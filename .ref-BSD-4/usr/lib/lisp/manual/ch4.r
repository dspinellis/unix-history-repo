






                                CHAPTER  4


                                 Special Functions





(and [g_arg1 ...])

     RETURNS: the value of the last argument if all arguments evaluate to a
              non  nil  value,  otherwise _a_n_d returns nil.  It returns t if
              there are no arguments.

     NOTE: the arguments are evaluated left to right  and  evaluation  will
           cease with the first nil encountered


(apply 'u_func 'l_args)

     RETURNS: the result of applying function u_func to  the  arguments  in
              the list l_args.

     NOTE: If u_func is a lambda, then the (_l_e_n_g_t_h _l__a_r_g_s) should equal the
           number  of  formal  parameters  for  the u_func.  If u_func is a
           nlambda or macro, then l_args is  bound  to  the  single  formal
           parameter.
























9

9   Special Functions                                                    4-1







   Special Functions                                                    4-2



    ___________________________________________________________________

    ; _a_d_d_1 is a lambda of 1 argument
    -> (_a_p_p_l_y '_a_d_d_1 '(_3))
    4

    ; we will define _p_l_u_s_1 as a macro which will be equivalent to _a_d_d_1
    -> (_d_e_f _p_l_u_s_1 (_m_a_c_r_o (_a_r_g) (_l_i_s_t '_a_d_d_1 (_c_a_d_r _a_r_g))))
    plus1
    -> (_p_l_u_s_1 _3)
    4

    ; now if we _a_p_p_l_y a macro we obtain the form it changes to.
    -> (_a_p_p_l_y '_p_l_u_s_1 '(_p_l_u_s_1 _3))
    (add1 3)

    ; if we _f_u_n_c_a_l_l a macro however, the result of the macro is _e_v_a_led
    ; before it is returned.
    -> (_f_u_n_c_a_l_l '_p_l_u_s_1 '(_p_l_u_s_1 _3))
    4
    ___________________________________________________________________





(arg ['x_numb])

     RETURNS: if x_numb is specified then the  x_numb'_t_h  argument  to  the
              enclosing  lexpr If x_numb is not specified then this returns
              the number of arguments to the enclosing lexpr.

     NOTE: it is an error to the interpreter if x_numb is given and out  of
           range.


(break [g_message ['g_pred]])

     WHERE:   if g_message is not given  it  is  assumed  to  be  the  null
              string, and if g_pred is not given it is assumed to be t.

     RETURNS: the value of (*break 'g_pred 'g_message)









9

9                                                  Printed: October 22, 1980







   Special Functions                                                    4-3


(*break 'g_pred 'g_message)

     RETURNS: nil immediately if g_pred is nil, else the value of the  next
              (return 'value) expression typed in at top level.

     SIDE EFFECT: If the predicate, g_pred, evaluates to non nil, the  lisp
                  system   stops  and  prints  out  `Break  '  followed  by
                  g_message. It then enters a break loop which  allows  one
                  to  interactively debug a program.  To continue execution
                  from a break you can use the _r_e_t_u_r_n function.  to  return
                  to  top  level or another break level, you can use _r_e_t_b_r_k
                  or _r_e_s_e_t.


(catch g_exp [ls_tag])

     WHERE:   if ls_tag is not given, it is assumed to be nil.

     RETURNS: the result of (*catch 'ls_tag g_exp)

     NOTE: catch is defined as a macro.


(*catch 'ls_tag g_exp)

     WHERE:   ls_tag is either a symbol or a list of symbols.

     RETURNS: the result of evaluating g_exp or the value thrown during the
              evaluation of g_exp.

     SIDE EFFECT: this first sets up a `catch frame' on  the  lisp  runtime
                  stack.   Then  it  begins  to  evaluate  g_exp.  If g_exp
                  evaluates normally, its value is returned.  If, however a
                  _t_h_r_o_w  is  done  during  the  evaluation of g_exp we will
                  catch the value thrown iff one of these cases is true:

     (1)  the tag thrown to is ls_tag

     (2)  ls_tag is a list and the tag thrown to is a member of this list

     (3)  ls_tag is nil.

     NOTE: Errors are implemented as a special kind of throw.  A catch with
           no  tag  will  not  catch  an error but a catch whose tag is the
           error type will catch that type  of  error.   See  10  for  more
           information.






9

9                                                  Printed: October 22, 1980







   Special Functions                                                    4-4


(comment [g_arg ...])

     RETURNS: the symbol comment.

     NOTE: This does absolutely nothing.


(cond [l_clause1 ...])

     RETURNS: the last value evaluated in the first clause  satisfied.   If
              no clauses are satisfied then nil is returned.

     NOTE: This is the basic conditional `statement' in lisp.  The  clauses
           are processed from left to right.  The first element of a clause
           is evaluated.  If it evaluated to a  non  nil  value  then  that
           clause  is  satisfied  and all following elements of that clause
           are evaluated.  The last value computed is returned as the value
           of  the  cond.   If there is just one element in the clause then
           its value is returned.  If the first element of a clause  evalu-
           ates  to  nil,  then  the  other elements of that clause are not
           evaluated and the system moves to the next clause.


(declare [g_arg ...])

     RETURNS: nil

     NOTE: this is a no-op to the evaluator.  It has special meaning to the
           compiler.


(def s_name (s_type l_argl g_exp1 ...))

     WHERE:   s_type is one of lambda, nlambda, macro or lexpr.

     RETURNS: s_name

     SIDE EFFECT: This defines the function s_name to the lisp system.   If
                  s_type  is nlambda or macro then the argument list l_argl
                  must contain exactly one non-nil symbol.












9

9                                                  Printed: October 22, 1980







   Special Functions                                                    4-5


(defun s_name [s_mtype] ls_argl g_exp1 ... )

     WHERE:   s_mtype is one of fexpr, expr, args or macro.

     RETURNS: s_name

     SIDE EFFECT: This defines the function s_name.

     NOTE: this exists for MAClisp compatibility, it is just a macro  which
           changes  the defun form to the def form.  An s_mtype of fexpr is
           converted to nlambda and of expr to lambda.  Macro  remains  the
           same.   If ls_arg1 is a non-nil symbol, then the type is assumed
           to be lexpr and ls_arg1 is the symbol  which  is  bound  to  the
           number of args when the function is entered.


    ___________________________________________________________________

    ; _d_e_f and _d_e_f_u_n here are used to define identical functions
    ; you can decide for yourself which is easier to use.
    -> (_d_e_f _a_p_p_e_n_d_1 (_l_a_m_b_d_a (_l_i_s _e_x_t_r_a) (_a_p_p_e_n_d _l_i_s (_l_i_s_t _e_x_t_r_a))))
    append1

    -> (_d_e_f_u_n _a_p_p_e_n_d_1 (_l_i_s _e_x_t_r_a) (_a_p_p_e_n_d _l_i_s (_l_i_s_t _e_x_t_r_a)))
    append1
    ___________________________________________________________________





(do l_vrbs l_test g_exp1 ...)

     RETURNS: the last form in the cdr of  l_test  evaluated,  or  a  value
              explicitly given by a return evaluated within the do body.

     NOTE: This is the basic iteration form for FRANZ LISP.   l_vrbs  is  a
           list  of  zero or more var-init-repeat forms.  A var-init-repeat
           form looks like:
                       (s_name [g_init [g_repeat]])
           There are three cases depending on what is present in the  form.
           If  just  s_name  is  present,  this  means  that when the do is
           entered, s_name is lambda-bound to nil and is never modified  by
           the  system  (though the program is certainly free to modify its
           value).  If the form is (s_name 'g_init) then the  only  differ-
           ence  is  that  s_name  is  lambda-bound  to the value of g_init
           instead of nil.  If g_repeat is  also  present  then  s_name  is
           lambda-bound  to  g_init when the loop is entered and after each
           pass through the do body  s_name  is   bound  to  the  value  of
           g_repeat.
           l_test is either nil or has the form of a cond clause.  If it is
           nil then the do body will be evaluated only once and the do will
           return nil.  Otherwise, before the do body is evaluated the  car


                                                  Printed: October 22, 1980







   Special Functions                                                    4-6


           of l_test is evaluated and if the result is non nil this signals
           an end to the looping.  Then the rest of the forms in l_test are
           evaluated and the value of the last one is returned as the value
           of the do.  If the cdr of l_test is nil, then nil is returned --
           thus this is not exactly like a cond clause.
           g_exp1 and those forms which follow constitute the do  body.   A
           do body is like a prog body and thus may have labels and one may
           use the functions go and return.
           The sequence of evaluations is this:

     (1)  the init forms are evaluated left to right and   stored  in  tem-
          porary locations.

     (2)  Simultaneously all do variables are lambda bound to the value  of
          their init forms or nil.

     (3)  If l_test is non nil then the car is evaluated and if it  is  non
          nil  the  rest  of the forms in l_test are evaluated and the last
          value is returned as the value of the do.

     (4)  The forms in the do body are evaluated left to right.

     (5)  If l_test is nil the do function returns with the value nil.

     (6)  The repeat forms are evaluated and saved in temporary locations.

     (7)  The variables with repeat forms are simultaneously bound  to  the
          values of those forms.

     (8)  Go to step 3.

     NOTE: there is an alternate form of do which can be used when there is
           only one do variable.  It is described next.


(do s_name g_init g_repeat g_test g_exp1 ...)

     NOTE: this is another, less general,  form of do.  It is evaluated by:

     (1)  evaluating g_init

     (2)  lambda binding s_name to value of g_init

     (3)  g_test is evaluated and if it is not nil the do function  returns
          with nil.

     (4)  the do body is evaluated beginning at g_exp1.

     (5)  the repeat form is evaluated and stored in s_name.

     (6)  go to step 3.

9

9                                                  Printed: October 22, 1980







   Special Functions                                                    4-7


(err ['s_value [nil]])

     RETURNS: nothing (it never returns).

     SIDE EFFECT: This causes an error and if this error is  caught  by  an
                  _e_r_r_s_e_t  then  that  _e_r_r_s_e_t will return s_value instead of
                  nil.  If the second arg is given, then  it  must  be  nil
                  (MAClisp compatibility).


(error ['s_message1 ['s_message2]])

     RETURNS: nothing (it never returns).

     SIDE EFFECT: s_message1 and s_message2 are _p_a_t_o_med if they  are  given
                  and then _e_r_r is called which causes an error.


(errset g_expr [s_flag])

     RETURNS: a list of one element, which  is  the  value  resulting  from
              evaluating  g_expr.  If an error occurs during the evaluation
              of g_expr, then the locus  of  control  will  return  to  the
              _e_r_r_s_e_t  which  will  then  return  nil  (unless the error was
              caused by a call to _e_r_r).

     SIDE EFFECT: S_flag is evaluated before g_expr is evaluated. If s_flag
                  is  not  given,  then it is assumed to be t.  If an error
                  occurs  during  the  evaluation  of  g_expr,  and  s_flag
                  evaluated  to  a  non  nil  value, then the error message
                  associated with  the  error  is  printed  before  control
                  returns to the errset.


(eval 'g_val)

     RETURNS: the result of evaluating g_val.

     NOTE: The evaluator evaluates g_val in this way:
           If g_val is a symbol, then the evaluator returns its value.   If
           g_val  had  never  been  assigned  a  value, then this causes an
           'Unbound Variable' error.  If g_val is of type value,  then  its
           value  is  returned.   If  g_val  is a list object then g_val is
           either a function call or array reference.   Let  g_car  be  the
           first  element of g_val.  We continually evaluate g_car until we
           end up with a symbol with a non nil function binding or  a  non-
           symbol.   Call  what we end up with: g_func.  g_func must be one
           of three types: list, binary or array.  If it is a list then the
           first  element of the list, which we shall call g_functype, must
           be either lambda, nlambda, macro  or  lexpr.   If  g_func  is  a
           binary,  then its discipline, which we shall call g_functype, is
           either lambda, nlambda, macro or a string  "subroutine",  "func-
           tion",  "integer-function"  or "real-function".  If g_func is an


                                                  Printed: October 22, 1980







   Special Functions                                                    4-8


           array then this form is evaluated specially, see  9  on  arrays.
           If  g_func  is  a list or binary, then g_functype will determine
           how the arguments to this function, the cdr of g_val,  are  pro-
           cessed.  If g_functype is a string, then this is a foreign func-
           tion call (see 8.4 for more details).  If g_functype  is  lambda
           or  lexpr,  the arguments are  evaluated (by calling _e_v_a_l recur-
           sively) and stacked.  If g_functype is nlambda then the argument
           list  is  stacked.  If g_functype is macro then the entire form,
           g_val is stacked.  Next the formal variables are  lambda  bound.
           The  formal  variables are the cadr of g_func - if g_functype is
           nlambda, lexpr or macro, there should only be one  formal  vari-
           able.   The  values  on the stack are lambda bound to the formal
           variables except in the case of a lexpr,  where  the  number  of
           actual  arguments  is  bound  to the formal variable.  After the
           binding is done, the function is invoked, either by  jumping  to
           the  entry  point  in  the case of a binary or by evaluating the
           list of forms beginning at cddr  g_func.   The  result  of  this
           function  invocation  is  returned  as  the value of the call to
           eval.


(eval-when l_times g_exp1 ... g_expn)

     WHERE:   l_times is a list containing any combination of compile, eval
              and load.

     RETURNS: nil if the symbol  eval  is  not   member  of  l_times,  else
              returns the value of g_expn.

     SIDE EFFECT: If eval is a member of l_times, then the forms g_exp_i are
                  evaluated.

     NOTE: this is used mainly  to  control  when  the  compiler  evaluates
           forms.


(exec s_arg1 ...)

     RETURNS: the result of forking and executing the command named by con-
              catenating the s_arg_i together with spaces in between.












9

9                                                  Printed: October 22, 1980







   Special Functions                                                    4-9


(exece 's_fname ['l_args ['l_envir]])

     RETURNS: the error code from the system if it was  unable  to  execute
              the  command  s_fname  with  arguments  l_args  and  with the
              environment set up as specified in l_envir.  If this function
              is  successful,  it  will not return, instead the lisp system
              will be overlaid by the new command.


(funcall 'u_func ['g_arg1 ...])

     RETURNS: the value of applying function u_func to the arguments g_arg_i
              and then evaluating that result if u_func is a macro.

     NOTE: If u_func is a macro or nlambda then there should  be  only  one
           g_arg.   _f_u_n_c_a_l_l  is  the  function  which the evaluator uses to
           evaluate lists.  If _f_o_o is a lambda  or  lexpr  or  array,  then
           (_f_u_n_c_a_l_l '_f_o_o '_a '_b '_c) is equivalent to (_f_o_o '_a '_b '_c).  If _f_o_o
           is a nlambda then (_f_u_n_c_a_l_l '_f_o_o '(_a _b _c)) is equivalent to  (_f_o_o
           _a    _b    _c).     Finally,    if    _f_o_o    is   a   macro   then
           (_f_u_n_c_a_l_l '_f_o_o '(_f_o_o _a _b _c)) is equivalent to (_f_o_o _a _b _c).


(function u_func)

     RETURNS: the function binding of u_func if it  is  an  symbol  with  a
              function binding otherwise u_func is returned.


(getdisc 't_func)

     RETURNS: the discipline of the machine coded function (either  lambda,
              nlambda or macro).


(go g_labexp)

     WHERE:   g_labexp is either a symbol or an expression.

     SIDE EFFECT: If  g_labexp  is  an  expression,  that   expression   is
                  evaluated  and  should  result in a symbol.  The locus of
                  control moves to just following the  symbol  g_labexp  in
                  the current prog or do body.

     NOTE: this is only valid in the context of a prog  or  do  body.   The
           interpreter  and compiler will allow non-local _g_o's although the
           compiler won't allow a _g_o to leave a function  body.   The  com-
           piler will not allow g_labexp to be an expression.




9

9                                                  Printed: October 22, 1980







   Special Functions                                                   4-10


(map 'u_func 'l_arg1 ...)

     RETURNS: l_arg1

     NOTE: The function u_func is applied to  successive  sublists  of  the
           l_arg_i.  All sublists should have the same length.


(mapc 'u_func 'l_arg1 ...)

     RETURNS: l_arg1.

     NOTE: The function u_func is applied to  successive  elements  of  the
           argument lists.  All of the lists should have the same length.


(mapcan 'u_func 'l_arg1 ...)

     RETURNS: nconc applied to the results of the functional evaluations.

     NOTE: The function u_func is applied to  successive  elements  of  the
           argument lists.  All sublists should have the same length.


(mapcar 'u_func 'l_arg1 ...)

     RETURNS: a list of the values returned from  the  functional  applica-
              tion.

     NOTE: the function u_func is applied to  successive  elements  of  the
           argument lists.  All sublists should have the same length.


(mapcon 'u_func 'l_arg1 ...)

     RETURNS: nconc applied to the results of the functional evaluation.

     NOTE: the function u_func is applied to  successive  sublists  of  the
           argument lists.  All sublists should have the same length.


(maplist 'u_func 'l_arg1 ...)

     RETURNS: a list of the results of the functional evaluations.

     NOTE: the function u_func is applied to  successive  sublists  of  the
           arguments lists.  All sublists should have the same length.





9

9                                                  Printed: October 22, 1980







   Special Functions                                                   4-11


(mfunction entry 's_disc)

     RETURNS: a lisp object of type binary composed of entry and s_disc.

     NOTE: entry is a pointer to the  machine  code  for  a  function,  and
           s_disc is the discipline (e.g. lambda).


(oblist)

     RETURNS: a list of all symbols on the oblist.


(or [g_arg1 ... ])

     RETURNS: the value of the first non nil argument  or nil if all  argu-
              ments evaluate to nil.

     NOTE: Evaluation proceeds left to right and stops as soon  as  one  of
           the arguments evaluates to a non nil value.


(prog l_vrbls g_exp1 ...)

     RETURNS: the value explicitly given in a return form or else nil if no
              return is done by the time the last g_exp_i is evaluated.

     NOTE: the local variables are lambda bound to nil then the  g_exp  are
           evaluated  from  left to right.  This is a prog body (obviously)
           and this means than any symbols seen are not evaluated,  instead
           they  are  treated  as labels.  This also means that returns and
           go's are allowed.


(prog2 g_exp1 g_exp2 [g_exp3 ...])

     RETURNS: the value of g_exp2.

     NOTE: the forms are evaluated from left to  right  and  the  value  of
           g_exp2 is returned.












9

9                                                  Printed: October 22, 1980







   Special Functions                                                   4-12


(progn g_exp1 [g_exp2 ...])

     RETURNS: the value of the last g_exp_i.

     NOTE: the forms are evaluated from left to right and the value of  the
           last one is returned.


(progv 'l_locv 'l_initv g_exp1 ...)

     WHERE:   l_locv is a list of symbols and l_initv is a list of  expres-
              sions.

     RETURNS: the value of the last g_exp_i evaluated.

     NOTE: The expressions in l_initv are evaluated from left to right  and
           then lambda-bound to the symbols in _locv.  If there are too few
           expressions in l_initv then the missing values are assumed to be
           nil.   If  there  are  too  many expressions in l_initv then the
           extra ones are ignored (although they are evaluated).  Then  the
           g_exp_i are evaluated left to right.  The body of a progv is like
           the body of a progn, it is _n_o_t a prog body.


(putd 's_name 'u_func)

     RETURNS: this sets the function binding of symbol s_name to u_func.


(return ['g_val])

     RETURNS: g_val (or nil if g_val is not  present)  from  the  enclosing
              prog or do body.

     NOTE: this form is only valid in the context of a prog or do body.


(setarg 'x_argnum 'g_val)

     WHERE:   x_argnum is greater than zero and less than or equal  to  the
              number of arguments to the lexpr.

     RETURNS: g_val

     SIDE EFFECT: the lexpr's x_argnum'th argument is set to g-val.

     NOTE: this can only be used within the body of a lexpr.





9

9                                                  Printed: October 22, 1980







   Special Functions                                                   4-13


(throw 'g_val [s_tag])

     WHERE:   if s_tag is not given, it is assumed to be nil.

     RETURNS: the value of (*_t_h_r_o_w '_s__t_a_g '_g__v_a_l).


(*throw 's_tag 'g_val)

     RETURNS: g_val from the first enclosing catch with the  tag  s_tag  or
              with no tag at all.

     NOTE: this is used in conjunction with *_c_a_t_c_h to cause a clean jump to
           an enclosing context.






































9

9                                                  Printed: October 22, 1980



