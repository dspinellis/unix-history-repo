






                         CHAPTER  9


                           Arrays




     Arrays in FRANZ LISP provide a programmable data struc-
ture  access  mechanism.   One  possible  use for FRANZ LISP
arrays is to implement Maclisp style arrays which are simple
vectors of fixnums, flonums or general lisp values.  This is
described in more detail in 9.3 but first we  will  describe
how array references are handled by the lisp system.

     The structure of an array object is given in 1.3.9  and
reproduced here for your convenience.


8_______________________________________________________________
  Subpart name     Get value   Set value          Type

8______________________________________________________________________________________________________________________________
 access function   getaccess   putaccess      binary, list
                                                or symbol
8_______________________________________________________________
    auxiliary       getaux      putaux           lispval
8_______________________________________________________________
      data         arrayref     replace    block of contiguous
                                  set            lispval
8_______________________________________________________________
     length        getlength   putlength         fixnum
8_______________________________________________________________
      delta        getdelta    putdelta          fixnum
8_______________________________________________________________
7|7|7|7|7|7|7|7|7|7|7|7|










                |7|7|7|7|7|7|7|7|7|7|7|










                            |7|7|7|7|7|7|7|7|7|7|7|










                                        |7|7|7|7|7|7|7|7|7|7|7|










                                                              |7|7|7|7|7|7|7|7|7|7|7|
















_9._1.  _g_e_n_e_r_a_l _a_r_r_a_y_s   Suppose  the  evaluator  is  told  to
evaluate  (_f_o_o _a _b)  and the function cell of the symbol foo
contains an array object (which we will  call  foo_arr_obj).
First  the evaluator will evaluate and stack the values of _a
and _b.  Next it will stack  the  array  object  foo_arr_obj.
Finally  it  will  call  the access function of foo_arr_obj.
The access function should be a lexpr[] or  a  symbol  whose
function cell contains a  lexpr.   The  access  function  is
responsible  for  locating  and  returning  a value from the
array.  The array access function is free to  interpret  the
arguments as it wishes.  The Maclisp compatible array access
function which is provided in the standard FRANZ LISP system
____________________
9   []A lexpr is a function which accepts any number of argu-
ments which are evaluated before the function is called.



Arrays                                                   9-1







Arrays                                                   9-2


interprets the arguments as subscripts in the  same  way  as
languages like Fortran and Pascal.

     The array access function will also be called  upon  to
store    elements    in    the    array.     For    example,
(_s_t_o_r_e (_f_o_o _a _b) _c) will automatically expand to (foo c a b)
and  when  the evaluator is called to evaluate this, it will
evaluate the arguments _c, _b and _a.  Then it will  stack  the
array  object  (which is stored in the function cell of foo)
and call the array access function  with  (now)  four  argu-
ments.   The array access function must be able to tell this
is a store operation which it can by checking the number  of
arguments  it  has  been  given  (a  lexpr  can do this very
easily).




_9._2.  _s_u_b_p_a_r_t_s _o_f _a_n _a_r_r_a_y _o_b_j_e_c_t   An array is  created  by
allocating  an  array object with _m_a_r_r_a_y and  filling in the
fields.  Certain lisp functions interpret the values of  the
subparts of the array object in special ways as described in
the following text.  Placing illegal values  in  these  sub-
parts may cause the lisp system to fail.




_9._2._1.  _a_c_c_e_s_s _f_u_n_c_t_i_o_n   The purpose of the access function
has  been described above.  The contents of the access func-
tion should be a lexpr, either a binary (compiled  function)
or  a  list (interpreted function).  It may also be a symbol
whose function cell contains a  function  definition.   This
subpart  is used by _e_v_a_l, _f_u_n_c_a_l_l, and _a_p_p_l_y when evaluating
array references.




_9._2._2.  _a_u_x_i_l_i_a_r_y   This can be used for any purpose. If  it
is  a  list and the first element of that list is the symbol
unmarked_array then the data subpart will not be  marked  by
the  garbage collector (this is used in the Maclisp compati-
ble array package and has the potential for causing  strange
errors if used incorrectly).




_9._2._3.  _d_a_t_a   This is either nil or points to  a  block  of
data space allocated by _s_e_g_m_e_n_t or _s_m_a_l_l-_s_e_g_m_e_n_t.

9

9                                     Printed: March 23, 1982







Arrays                                                   9-3


_9._2._4.  _l_e_n_g_t_h   This is a fixnum whose value is the  number
of  elements in the data block.  This is used by the garbage
collector and by _a_r_r_a_y_r_e_f to determine if your index  is  in
bounds.




_9._2._5.  _d_e_l_t_a   This is a fixnum whose value is  the  number
of  bytes  in  each element of the data block.  This will be
four for an array of fixnums or value cells, and  eight  for
an  array of flonums.  This is used by the garbage collector
and _a_r_r_a_y_r_e_f as well.




_9._3.  _T_h_e _M_a_c_l_i_s_p _c_o_m_p_a_t_i_b_l_e _a_r_r_a_y _p_a_c_k_a_g_e

     A Maclisp style array is similar to what  are  know  as
arrays  in other languages: a block of homogeneous data ele-
ments which is indexed by one or more integers  called  sub-
scripts.   The  data elements can be all fixnums, flonums or
general lisp objects.  An array is created by a call to  the
function  _a_r_r_a_y  or  *_a_r_r_a_y.   The  only  difference is that
*_a_r_r_a_y evaluates its arguments.  This call: (_a_r_r_a_y _f_o_o  _t  _3
_5)  sets  up  an array called foo of dimensions 3 by 5.  The
subscripts are zero based. The first element  is  (_f_o_o _0 _0),
the  next  is  (_f_o_o _0 _1)  and  so on up to (_f_o_o _2 _4).  The t
indicates a general lisp object array which means each  ele-
ment  of  foo can be any type.  Each element can be any type
since all that is stored in the array is a pointer to a lisp
object,  not the object itself.  _A_r_r_a_y does this by allocat-
ing an array object with _m_a_r_r_a_y and then allocating  a  seg-
ment  of  15  consecutive value cells with _s_m_a_l_l-_s_e_g_m_e_n_t and
storing a pointer to that segment in the data subpart of the
array  object.   The  length  and delta subpart of the array
object are filled in (with 15 and 4  respectively)  and  the
access  function  subpart is set to point to the appropriate
array access function.  In this  case  there  is  a  special
access function for two dimensional value cell arrays called
arrac-twoD, and this access function is used.  The auxiliary
subpart  is set to (t 3 5) which describes the type of array
and the bounds of the subscripts. Finally this array  object
is  placed in the function cell of the symbol foo.  Now when
(_f_o_o _1 _3) is evaluated, the array access function is invoked
with  three  arguments: 1, 3 and the array object.  From the
auxiliary field of the array object it gets a description of
the  particular  array.   It  then  determines which element
(_f_o_o _1 _3) refers to  and uses arrayref to extract that  ele-
ment.   Since this is an array of value cells, what arrayref
returns is a value cell whose value is what we want,  so  we
evaluate  the  value  cell  and  return  it  as the value of


                                     Printed: March 23, 1982







Arrays                                                   9-4


(_f_o_o _1 _3).

     In Maclisp the call  (_a_r_r_a_y _f_o_o _f_i_x_n_u_m _2_5)  returns  an
array whose data object is a block of 25 memory words.  When
fixnums are stored in this array,  the  actual  numbers  are
stored  instead  of  pointers  to the numbers as are done in
general lisp object arrays.  This is efficient under Maclisp
but  inefficient  in FRANZ LISP since every time a value was
referenced from an array it had to be copied and  a  pointer
to the copy returned to prevent aliasing[].  Thus t,  fixnum
and  flonum  arrays  are all implemented in the same manner.
This should not affect  the  compatibility  of  Maclisp  and
FRANZ  LISP.   If  there  is an application where a block of
fixnums or flonums is required, then the exact  same  effect
of  fixnum  and  flonum arrays in Maclisp can be achieved by
using fixnum-block and flonum-block arrays.  Such arrays are
required  if you want to pass a large number of arguments to
a Fortran or C coded function and then get answers back.

     The Maclisp compatible array package is just one  exam-
ple  of  how  a  general  array  scheme  can be implemented.
Another type of array you could implement  would  be  hashed
arrays.  The subscript could be anything, not just a number.
The access function would hash the  subscript  and  use  the
result  to  select an array element.  With the generality of
arrays also comes extra cost; if you just want a simple vec-
tor  of  (less  than  128) general lisp objects you would be
wise to look into using hunks.















____________________
9   []Aliasing is when  two  variables  are  share  the  same
storage  location.   For  example  if  the copying mentioned
weren't done then after (_s_e_t_q _x (_f_o_o _2)) was done, the value
of x and (foo 2) would share the same location.  Then should
the value of (foo 2) change, x's value would change as well.
This  is  considered  dangerous and as a result pointers are
never returned into the data space of arrays.



9                                     Printed: March 23, 1982



