






                         CHAPTER  3


                    Arithmetic Functions




     This chapter describes FRANZ LISP's functions for doing
arithmetic.  Often the same function is known by many names.
For example, _a_d_d is also _p_l_u_s, and _s_u_m.  This is  caused  by
our  desire  to  be  compatible with other Lisps.  The FRANZ
LISP user should avoid using functions with names such as  +
and * unless their arguments are fixnums.  The Lisp compiler
takes advantage of these implicit declarations.

     An attempt to divide or to generate  a  floating  point
result  outside  of the range of floating point numbers will
cause a floating exception signal from  the  UNIX  operating
system.   The  user  can catch and process this interrupt if
desired (see the description of the _s_i_g_n_a_l function).



   3.1.  Simple Arithmetic Functions

(add ['n_arg1 ...])
(plus ['n_arg1 ...])
(sum ['n_arg1 ...])
(+ ['x_arg1 ...])

     RETURNS: the sum of the arguments. If no arguments  are
              given, 0 is returned.

     NOTE: if the size of the partial sum exceeds the  limit
           of a fixnum, the partial sum will be converted to
           a bignum.  If any of the arguments  are  flonums,
           the  partial  sum  will  be converted to a flonum
           when that argument is processed  and  the  result
           will thus be a flonum.  Currently, if in the pro-
           cess of doing the addition a bignum must be  con-
           verted  into  a  flonum  an  error  message  will
           result.









9

9Arithmetic Functions                                     3-1







Arithmetic Functions                                     3-2


(add1 'n_arg)
(1+ 'x_arg)

     RETURNS: its argument plus 1.

(diff ['n_arg1 ... ])
(difference ['n_arg1 ... ])
(- ['x_arg1 ... ])

     RETURNS: the result of subtracting from n_arg1 all sub-
              sequent  arguments. If no arguments are given,
              0 is returned.

     NOTE: See the description of add for  details  on  data
           type conversions and restrictions.

(sub1 'n_arg)
(1- 'x_arg)

     RETURNS: its argument minus 1.

(minus 'n_arg)

     RETURNS: zero minus n_arg.

(product ['n_arg1 ... ])
(times ['n_arg1 ... ])
(* ['x_arg1 ... ])

     RETURNS: the product  of  all  of  its  arguments.   It
              returns 1 if there are no arguments.

     NOTE: See the  description  of  the  function  _a_d_d  for
           details  and  restrictions  to the automatic data
           type coercion.

(quotient ['n_arg1 ...])
(/ ['x_arg1 ...])

     RETURNS: the result of dividing the first  argument  by
              succeeding ones.

     NOTE: If there are no arguments, 1  is  returned.   See
           the  description  of the function _a_d_d for details
           and restrictions of data type coercion.  A divide
           by zero will cause a floating exception interrupt
           -- see the description of the _s_i_g_n_a_l function.





9

9                                   Printed: January 31, 1984







Arithmetic Functions                                     3-3


(*quo 'i_x 'i_y)

     RETURNS: the integer part of i_x / i_y.

(Divide 'i_dividend 'i_divisor)

     RETURNS: a list whose car is  the  quotient  and  whose
              cadr  is  the  remainder  of  the  division of
              i_dividend by i_divisor.

     NOTE: this is restricted to integer division.

(Emuldiv 'x_fact1 'x_fact2 'x_addn 'x_divisor)

     RETURNS: a list of the quotient and remainder  of  this
              operation:
              ((x_fact1 * x_fact2) + (sign extended) x_addn) / x_divisor.

     NOTE: this  is  useful for creating a bignum arithmetic
           package in Lisp.



   3.2.  predicates

(numberp 'g_arg)

(numbp 'g_arg)

     RETURNS: t iff g_arg is a  number  (fixnum,  flonum  or
              bignum).

(fixp 'g_arg)

     RETURNS: t iff g_arg is a fixnum or bignum.

(floatp 'g_arg)

     RETURNS: t iff g_arg is a flonum.

(evenp 'x_arg)

     RETURNS: t iff x_arg is even.









9

9                                   Printed: January 31, 1984







Arithmetic Functions                                     3-4


(oddp 'x_arg)

     RETURNS: t iff x_arg is odd.

(zerop 'g_arg)

     RETURNS: t iff g_arg is a number equal to 0.

(onep 'g_arg)

     RETURNS: t iff g_arg is a number equal to 1.

(plusp 'n_arg)

     RETURNS: t iff n_arg is greater than zero.

(minusp 'g_arg)

     RETURNS: t iff g_arg is a negative number.

(greaterp ['n_arg1 ...])
(> 'fx_arg1 'fx_arg2)
(>& 'x_arg1 'x_arg2)

     RETURNS: t iff the arguments are in a strictly decreas-
              ing order.

     NOTE: In functions _g_r_e_a_t_e_r_p and > the function  _d_i_f_f_e_r_-
           _e_n_c_e  is  used to compare adjacent values. If any
           of the arguments are non-numbers, the error  mes-
           sage will come from the _d_i_f_f_e_r_e_n_c_e function.  The
           arguments to > must  be fixnums or both  flonums.
           The arguments to >& must both be fixnums.

(lessp ['n_arg1 ...])
(< 'fx_arg1 'fx_arg2)
(<& 'x_arg1 'x_arg2)

     RETURNS: t iff the arguments are in a strictly increas-
              ing order.

     NOTE: In functions _l_e_s_s_p and < the function  _d_i_f_f_e_r_e_n_c_e
           is used to compare adjacent values. If any of the
           arguments are non numbers, the error message will
           come from the _d_i_f_f_e_r_e_n_c_e function.  The arguments
           to < may be either fixnums or flonums but must be
           the  same type.  The arguments to <& must be fix-
           nums.




9

9                                   Printed: January 31, 1984







Arithmetic Functions                                     3-5


(= 'fx_arg1 'fx_arg2)

(=& 'x_arg1 'x_arg2)

     RETURNS: t iff the arguments have the same value.   The
              arguments to = must be the either both fixnums
              or both flonums.  The arguments to =& must  be
              fixnums.



   3.3.  Trignometric Functions

           Some of these funtcions are taken from  the  host
      math  library,  and  we take no further responsibility
      for their accuracy.

(cos 'fx_angle)

     RETURNS: the (flonum)  cosine  of  fx_angle  (which  is
              assumed to be in radians).

(sin 'fx_angle)

     RETURNS: the sine of fx_angle (which is assumed  to  be
              in radians).

(acos 'fx_arg)

     RETURNS: the (flonum) arc cosine of fx_arg in the range
              0 to J.

(asin 'fx_arg)

     RETURNS: the (flonum) arc sine of fx_arg in  the  range
              -J/2 to J/2.

(atan 'fx_arg1 'fx_arg2)

     RETURNS: the (flonum) arc tangent of fx_arg1/fx_arg2 in
              the range -J to J.



   3.4.  Bignum/Fixnum Manipulation







9

9                                   Printed: January 31, 1984







Arithmetic Functions                                     3-6


(haipart bx_number x_bits)

     RETURNS: a fixnum (or bignum) which contains the x_bits
              high  bits  of  (_a_b_s _b_x__n_u_m_b_e_r)  if  x_bits is
              positive,    otherwise    it    returns    the
              (_a_b_s _x__b_i_t_s) low bits of (_a_b_s _b_x__n_u_m_b_e_r).

(haulong bx_number)

     RETURNS: the number of significant bits in bx_number.

     NOTE: the result is equal to the least integer  greater
           to or equal to the base two logarithm of one plus
           the absolute value of bx_number.

(bignum-leftshift bx_arg x_amount)

     RETURNS: bx_arg shifted left by x_amount.  If  x_amount
              is  negative,  bx_arg will be shifted right by
              the magnitude of x_amount.

     NOTE: If bx_arg is shifted right, it will be rounded to
           the nearest even number.

(sticky-bignum-leftshift 'bx_arg 'x_amount)

     RETURNS: bx_arg shifted left by x_amount.  If  x_amount
              is  negative,  bx_arg will be shifted right by
              the magnitude of x_amount and rounded.

     NOTE: sticky rounding is done this way: after shifting,
           the low order bit is changed to 1 if any 1's were
           shifted off to the right.



   3.5.  Bit Manipulation

(boole 'x_key 'x_v1 'x_v2 ...)

     RETURNS: the result of the bitwise boolean operation as
              described in the following table.

     NOTE: If there are more than 3 arguments, then  evalua-
           tion  proceeds  left  to  right with each partial
           result becoming the new value of x_v1.  That is,
                (_b_o_o_l_e '_k_e_y '_v_1 '_v_2 '_v_3) =_ (_b_o_o_l_e '_k_e_y (_b_o_o_l_e '_k_e_y '_v_1 '_v_2) '_v_3).
           In the following table, * represents bitwise and,
           + represents bitwise or, O+ represents bitwise xor
           and  _  represents  bitwise  negation  and is the
           highest precedence operator.












Arithmetic Functions                                     3-7


8____________________________________________________________________________________________
                                     (boole 'key 'x 'y)

8________________________________________________________________________________________________________________________________________________________________________________________
  key         0          1          2         3         4          5          6         7
 result       0        x * y     _ x * y      y      x * _ y       x        x O+ y     x + y

 common
 names                  and                          bitclear                xor       or

8____________________________________________________________________________________________

  key         8          9         10        11         12        13         14        15
 result   _ (x + y)   _(x O+ y)     _ x     _ x + y     _ y      x + _ y   _ x + _ y    -1
 common
 names       nor       equiv               implies                          nand
8____________________________________________________________________________________________
7|8|7|7|7|7|7|7|7|7|7|7|7|7|7|












9                                                                                           |8|7|7|7|7|7|7|7|7|7|7|7|7|7|














9
(lsh 'x_val 'x_amt)

     RETURNS: x_val shifted left by x_amt if x_amt is  posi-
              tive.   If x_amt is negative, then _l_s_h returns
              x_val shifted right by the magnitude if x_amt.

     NOTE: This always  returns  a  fixnum  even  for  those
           numbers  whose  magnitude  is  so large that they
           would normally be represented as a  bignum,  i.e.
           shifter  bits  are  lost.   For  more general bit
           shifters,  see   _b_i_g_n_u_m-_l_e_f_t_s_h_i_f_t   and   _s_t_i_c_k_y-
           _b_i_g_n_u_m-_l_e_f_t_s_h_i_f_t.

(rot 'x_val 'x_amt)

     RETURNS: x_val rotated left by x_amt if x_amt is  posi-
              tive.  If  x_amt  is  negative,  then x_val is
              rotated right by the magnitude of x_amt.



   3.6.  Other Functions

           As noted above, some of the  following  functions
      are  inherited  from  the  host math library, with all
      their virtues and vices.









9

9                                   Printed: January 31, 1984







Arithmetic Functions                                     3-8


(abs 'n_arg)
(absval 'n_arg)

     RETURNS: the absolute value of n_arg.

(exp 'fx_arg)

     RETURNS: _e raised to the fx_arg power (flonum) .

(expt 'n_base 'n_power)

     RETURNS: n_base raised to the n_power power.

     NOTE: if either of the arguments are flonums, the  cal-
           culation will be done using _l_o_g and _e_x_p.

(fact 'x_arg)

     RETURNS: x_arg factorial. (fixnum or bignum)

(fix 'n_arg)

     RETURNS: a fixnum as close as we can get to n_arg.

     NOTE: _f_i_x will round down.  Currently, if  n_arg  is  a
           flonum  larger  than  the  size of a fixnum, this
           will fail.

(float 'n_arg)

     RETURNS: a flonum as close as we can get to n_arg.

     NOTE: if n_arg is a bignum larger than the maximum size
           of  a  flonum,  then  a  floating  exception will
           occur.

(log 'fx_arg)

     RETURNS: the natural logarithm of fx_arg.

(max 'n_arg1 ... )

     RETURNS: the maximum value in the list of arguments.









9

9                                   Printed: January 31, 1984







Arithmetic Functions                                     3-9


(min 'n_arg1 ... )

     RETURNS: the minimum value in the list of arguments.

(mod 'i_dividend 'i_divisor)
(remainder 'i_dividend 'i_divisor)

     RETURNS: the remainder when i_dividend  is  divided  by
              i_divisor.

     NOTE: The sign of the result will have the same sign as
           i_dividend.

(*mod 'x_dividend 'x_divisor)

     RETURNS: the  balanced  representation  of   x_dividend
              modulo x_divisor.

     NOTE: the  range  of  the  balanced  representation  is
           abs(x_divisor)/2    to    (abs(x_divisor)/2)    -
           x_divisor + 1.

(random ['x_limit])

     RETURNS: a fixnum between 0 and x_limit - 1 if  x_limit
              is  given.   If x_limit is not given, any fix-
              num, positive or negative, might be returned.

(sqrt 'fx_arg)

     RETURNS: the square root of fx_arg.





















9

9                                   Printed: January 31, 1984



