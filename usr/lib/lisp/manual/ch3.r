






                                CHAPTER  3


                           Arithmetic Functions







   _3._1.  This chapter describes FRANZ LISP's  functions  for  doing  arith-
      metic.   Often  the  same function is know by many names, such as _a_d_d
      which is also _p_l_u_s, _s_u_m, and +.  This is due to our desire to be com-
      patible  with  other  Lisps.  The FRANZ LISP user is advised to avoid
      using functions with names such as + and * unless their arguments are
      fixnums.   The  lisp  compiler takes advantage of the fact that their
      arguments are fixnums.

           An attempt to divide by zero will  cause  a  floating  exception
      signal  from  the UNIX operating system.  The user can catch and pro-
      cess this interrupt if he wishes (see the description of  the  _s_i_g_n_a_l
      function).



(abs 'n_arg)

     RETURNS: the absolute value of n_arg.


(absval 'n_arg)

     EQUIVALENT TO: abs.


(add ['n_arg1 ...])

     RETURNS: the sum of the arguments. If no arguments  are  given,  0  is
              returned.

     NOTE: if the size of the partial sum exceeds the limit  of  a  fixnum,
           the  partial  sum  will be converted to a bignum.  If any of the
           arguments are flonums, the partial sum will be  converted  to  a
           flonum  when that argument is processed and the result will thus
           be a flonum.  Currently, if in the process of doing the addition
           a  bignum  must be converted into a flonum an error message will
           result.




9

9Arithmetic Functions                                                    3-1







Arithmetic Functions                                                    3-2


(add1 'n-arg)

     RETURNS: n_arg plus 1.


(acos 'fx_arg)

     RETURNS: the arc cosine of fx_arg in the range 0 to Ji.


(asin 'fx_arg)

     RETURNS: the arc sine of fx_arg in the range -J/2 to J/2.


(atan 'fx_arg1 'fx_arg2)

     RETURNS: the arc tangent of fx_arg1/fx_arg2 in the range -J to J.


(boole 'x_key 'x_v1 'x_v2 ...)

     RETURNS: the result of the bitwise boolean operation as  described  in
              the following table.

     NOTE: If there are more than 3  arguments,  then  evaluation  proceeds
           left to right with each partial result becoming the new value of
           x_v1.  That is,
                (_b_o_o_l_e '_k_e_y '_v_1 '_v_2 '_v_3) =_ (_b_o_o_l_e '_k_e_y (_b_o_o_l_e '_k_e_y '_v_1 '_v_2) '_v_3).
           In  the  following table, * represents bitwise and, + represents
           bitwise or, O+ represents bitwise xor and  _  represents  bitwise
           negation and is the highest precedence operator.

8___________________________________________________________________________________________
                                    (boole 'key 'x 'y)

8______________________________________________________________________________________________________________________________________________________________________________________
  key         0          1          2         3         4         5          6         7
 result       0        x * y     _ x * y      y      x * _ y      x        x O+ y     x + y

8___________________________________________________________________________________________

  key         8          9         10        11        12        13         14        15
 result   _ (x + y)   _(x O+ y)     _ x     _ x + y     _ y     x + _ y   _ x + _ y    -1
8___________________________________________________________________________________________
7|8|7|7|7|7|7|7|7|7|







9                                                                                          |8|7|7|7|7|7|7|7|7|









9







9

9                                                  Printed: October 21, 1980







Arithmetic Functions                                                    3-3


(cos 'fx_angle)

     RETURNS: the cosine of fx_angle (which is assumed to be in radians).


(diff ['n_arg1 ... ])

     RETURNS: the result of subtracting from n_arg1  all  subsequent  argu-
              ments. If no arguments are given, 0 is returned.

     NOTE: See the description of add for details on data type  conversions
           and restrictions.


(difference ['n_arg1 ...])

     EQUIVALENT TO: diff.


(Divide 'i_dividend 'i_divisor)

     RETURNS: a list whose car is  the  quotient  and  whose  cadr  is  the
              remainder of the division of i_dividend by i_divisor.

     NOTE: this is restricted to integer division.


(Emuldiv 'x_fact1 'x_fact2 'x_addn 'x_divisor)

     RETURNS: a list of the  quotient  and  remainder  of  this  operation:
              ((x_fact1 * x_fact2) + (sign extended) x_addn) / x_divisor.

     NOTE: this is useful for creating a bignum arithmetic package in Lisp.


(exp 'fx_arg)

     RETURNS: _e raised to the fx_arg power.


(expt 'n_base 'n_power)

     RETURNS: n_base raised to the i_power power.

     NOTE: if either of the arguments are flonums, the calculation will  be
           done using _l_o_g and _e_x_p.






9

9                                                  Printed: October 21, 1980







Arithmetic Functions                                                    3-4


(fact 'x_arg)

     RETURNS: x_arg factorial.


(fix 'n_arg)

     RETURNS: a fixnum as close as we can get to n_arg.

     NOTE: _f_i_x will round down.  Currently, if n_arg  is  a  flonum  larger
           than the size of a fixnum, this will fail.


(fixp 'g_arg)

     RETURNS: t iff g_arg is a fixnum or bignum.


(float 'n_arg)

     RETURNS: a flonum as close as we can get to n_arg.

     NOTE: if n_arg is a bignum larger than the maximum size of  a  flonum,
           then a floating exception will occur.


(floatp 'g_arg)

     RETURNS: t iff g_arg is a flonum.


(greaterp ['n_arg1 ...])

     RETURNS: t iff the arguments are in a strictly decreasing order.

     NOTE: the function difference is used to compare adjacent  values.  If
           any  of  the  arguments  are non numbers, the error message will
           come from the difference function.


(haipart bx_number x_bits)

     RETURNS: the x_bits high bits of |bx_number| if  x_bits  is  positive,
              otherwise it returns the |x_bits| low bits of |bx_number|.








9

9                                                  Printed: October 21, 1980







Arithmetic Functions                                                    3-5


(haulong bx_number)

     RETURNS: the number of significant bits in bx_number.

     NOTE: the result is equal to the least integer greater to or equal  to
           the base two logarithm of |bx_number| + 1.


(lessp ['n_arg1 ...])

     RETURNS: t iff the arguments are in a strictly increasing order.

     NOTE: the function _d_i_f_f_e_r_e_n_c_e is used to compare adjacent  values.  If
           any  of  the  arguments  are non numbers, the error message will
           come from the _d_i_f_f_e_r_e_n_c_e function.


(log 'fx_arg)

     RETURNS: the natural logarithm of fx_arg.


(lsh 'x_val 'x_amt)

     RETURNS: x_val shifted left by x_amt if x_amt is positive.   If  x_amt
              is negative, then _l_s_h returns x_val shifted right by the mag-
              nitude if x_amt.


(max 'n_arg1 ... )

     RETURNS: the maximum value in the list of arguments.


(min 'n_arg1 ... )

     RETURNS: the minimum value in the list of arguments.


(minus 'n_arg)

     RETURNS: zero minus n_arg.










9

9                                                  Printed: October 21, 1980







Arithmetic Functions                                                    3-6


(minusp 'g_arg)

     RETURNS: t iff g_arg is a negative number.


(mod 'i_dividend 'i_divisor)

     RETURNS: the remainder when i_dividend is divided by i_divisor.


(numberp 'g_arg)

     RETURNS: t iff g_arg is a number (fixnum, flonum or bignum).


(numbp 'g_arg)

     EQUIVALENT TO: numberp.


(onep 'g_arg)

     RETURNS: t iff g_arg is a number equal to 1.


(plus ['n_arg ...])

     EQUIVALENT TO: to add.


(plusp 'n_arg)

     RETURNS: t iff n_arg is greater than zero.


(product ['n_arg1 ... ])

     RETURNS: the product of all of its arguments.  It returns 1  if  there
              are no arguments.

     NOTE: See the description of the function _a_d_d for details and restric-
           tions to the automatic data type coercion.










9

9                                                  Printed: October 21, 1980







Arithmetic Functions                                                    3-7


(quotient ['n_arg1 ...])

     RETURNS: the result of dividing the first argument by succeeding ones.

     NOTE: If there are no arguments, 1 is returned.  See  the  description
           of  the  function  _a_d_d for details and restrictions of data type
           coercion.  A divide by zero  will  cause  a  floating  exception
           interrupt -- see the description of the _s_i_g_n_a_l function.


(random ['x_limit])

     RETURNS: a fixnum between 0 and x_limit  1 if x_limit  is  given.   If
              x_limit is not given, any fixnum, positive or negative, might
              be returned.


(remainder 'i_dividend 'i_divisor)

     EQUIVALENT TO: mod.


(rot 'x_val 'x_amt)

     RETURNS: x_val rotated left by x_amt if x_amt is positive. If x_amt is
              negative,  then  x_val  is  rotated right by the magnitude of
              x_amt.


(sin 'fx_angle)

     RETURNS: the sine of fx_angle (which is assumed to be in radians).


(sqrt 'fx_arg)

     RETURNS: the square root of fx_arg.


(sub1 'n_arg)

     RETURNS: n_arg minus 1.










9

9                                                  Printed: October 21, 1980







Arithmetic Functions                                                    3-8


(sum ['n_arg1 ...])

     EQUIVALENT TO: add and plus.


(times ['n_arg1 ... ])

     EQUIVALENT TO: product.


(zerop 'g_arg)

     RETURNS: t iff g_arg is a number equal to 0.




     _3._2.  These functions are restricted to fixnum arguments  in  Maclisp.
     The  lisp compiler will assume the arguments are fixnums and open code
     most of these functions.


(1+ 'n_arg)

     EQUIVALENT TO: add1.


(1- 'n_arg)

     EQUIVALENT TO: sub1.


(+ 'n_arg)

     EQUIVALENT TO: add.


(* 'n_arg)

     EQUIVALENT TO: times.












9

9                                                  Printed: October 21, 1980







Arithmetic Functions                                                    3-9


(- 'n_arg)

     EQUIVALENT TO: difference.


(/ 'n_arg1 'n_arg2)

     EQUIVALENT TO: quotient


(< 'n_arg1 'n_arg2)

     EQUIVALENT TO: lessp.


(= 'g_arg1 'g_arg2)

     EQUIVALENT TO: equal.


(> 'n_arg1 'n_arg2)

     EQUIVALENT TO: greaterp.





























9

9                                                  Printed: October 21, 1980



