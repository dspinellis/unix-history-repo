






                         CHAPTER  2


                   Data Structure Access




     The following functions allow one to create and manipu-
late  the  various  types of lisp data structures.  Refer to
1.2 for details of the data structures known to FRANZ LISP.



   2.1.  Lists

           The following functions exist  for  the  creation
      and  manipulating  of  lists.  Lists are composed of a
      linked list of objects  called  either  'list  cells',
      'cons cells' or 'dtpr cells'.  Lists are normally ter-
      minated with the special symbol nil.  nil  is  both  a
      symbol and a representation for the empty list ().



      2.1.1.  list creation

(cons 'g_arg1 'g_arg2)

     RETURNS: a new list cell whose car is g_arg1 and  whose
              cdr is g_arg2.

(xcons 'g_arg1 'g_arg2)

     EQUIVALENT TO: (_c_o_n_s '_g__a_r_g_2 '_g__a_r_g_1)

(ncons 'g_arg)

     EQUIVALENT TO: (_c_o_n_s '_g__a_r_g _n_i_l)

(list ['g_arg1 ... ])

     RETURNS: a list whose elements are the g_arg_i.









9

9Data Structure Access                                    2-1







Data Structure Access                                    2-2


(append 'l_arg1 'l_arg2)

     RETURNS: a list containing the elements of l_arg1  fol-
              lowed by l_arg2.

     NOTE: To generate the result, the top level list  cells
           of  l_arg1 are duplicated and the cdr of the last
           list cell is set to point to l_arg2.   Thus  this
           is  an  expensive  operation  if l_arg1 is large.
           See the  descriptions  of  _n_c_o_n_c  and  _t_c_o_n_c  for
           cheaper  ways  of  doing  the  _a_p_p_e_n_d if the list
           l_arg1 can be altered.

(append1 'l_arg1 'g_arg2)

     RETURNS: a list like l_arg1 with  g_arg2  as  the  last
              element.

     NOTE: this  is  equivalent  to  (append  'l_arg1  (list
           'g_arg2)).


    ____________________________________________________

    ; A common mistake is using append to add one element to the end of a list
    -> (_a_p_p_e_n_d '(_a _b _c _d) '_e)
    (a b c d . e)
    ; The user intended to say:
    -> (_a_p_p_e_n_d '(_a _b _c _d) '(_e))
    (_a _b _c _d _e)
    ; _b_e_t_t_e_r _i_s _a_p_p_e_n_d_1
    -> (_a_p_p_e_n_d_1 '(_a _b _c _d) '_e)
    (_a _b _c _d _e)
    ____________________________________________________




(quote! [g_qform_i] ...[! 'g_eform_i] ...  [!! 'l_form_i] ...)

     RETURNS: The list  resulting  from  the   splicing  and
              insertion process described below.

     NOTE: _q_u_o_t_e!  is the complement of the  _l_i_s_t  function.
           _l_i_s_t  forms  a list by evaluating each for in the
           argument list; evaluation is  suppressed  if  the
           form  is _q_u_o_t_eed.  In _q_u_o_t_e!, each form is impli-
           citly _q_u_o_t_eed.  To be evaluated, a form  must  be
           preceded  by one of the evaluate operations ! and
           !!. ! g_eform evaluates g_form and the  value  is
           inserted  in  the  place  of  the call; !! l_form
           evaluates l_form and the value  is  spliced  into
           the place of the call.


                                   Printed: January 31, 1984







Data Structure Access                                    2-3


           `Splicing in' means  that  the  parentheses  sur-
           rounding  the  list  are  removed  as the example
           below shows.  Use of the evaluate  operators  can
           occur at any level in a form argument.

           Another way to get the effect of the _q_u_o_t_e! func-
           tion is to use the backquote character macro (see
            8.3.3).


    ____________________________________________________

    (_q_u_o_t_e! _c_o_n_s ! (_c_o_n_s _1 _2) _3) = (_c_o_n_s (_1 . _2) _3)
    (_q_u_o_t_e! _1 !! (_l_i_s_t _2 _3 _4) _5) = (_1 _2 _3 _4 _5)
    (_s_e_t_q _q_u_o_t_e_d '_e_v_a_l_e_d)(_q_u_o_t_e! ! ((_I _a_m  ! _q_u_o_t_e_d))) = ((_I _a_m _e_v_a_l_e_d))
    (_q_u_o_t_e! _t_r_y ! '(_t_h_i_s ! _o_n_e)) = (_t_r_y (_t_h_i_s ! _o_n_e))
    ____________________________________________________





(bignum-to-list 'b_arg)

     RETURNS: A list  of  the  fixnums  which  are  used  to
              represent the bignum.

     NOTE: the inverse of this function is _l_i_s_t-_t_o-_b_i_g_n_u_m.

(list-to-bignum 'l_ints)

     WHERE:   l_ints is a list of fixnums.

     RETURNS: a bignum constructed of the given fixnums.

     NOTE: the inverse of this function is _b_i_g_n_u_m-_t_o-_l_i_s_t.




      2.1.2.  list predicates











9

9                                   Printed: January 31, 1984







Data Structure Access                                    2-4


(dtpr 'g_arg)

     RETURNS: t iff g_arg is a list cell.

     NOTE: that (dtpr '()) is nil.  The name dtpr is a  con-
           traction for ``dotted pair''.

(listp 'g_arg)

     RETURNS: t iff g_arg is a list object or nil.

(tailp 'l_x 'l_y)

     RETURNS: l_x, if a list cell _e_q  to  l_x  is  found  by
              _c_d_ring down l_y zero or more times, nil other-
              wise.


    ____________________________________________________

    -> (_s_e_t_q _x '(_a _b _c _d) _y (_c_d_d_r _x))
    (c d)
    -> (_a_n_d (_d_t_p_r _x) (_l_i_s_t_p _x))     ; x and y are dtprs and lists
    t
    -> (_d_t_p_r '())           ; () is the same as nil and is not a dtpr
    nil
    -> (_l_i_s_t_p '())          ; however it is a list
    t
    -> (_t_a_i_l_p _y _x)
    (c d)
    ____________________________________________________




(length 'l_arg)

     RETURNS: the number of elements in  the  top  level  of
              list l_arg.



      2.1.3.  list accessing









9

9                                   Printed: January 31, 1984







Data Structure Access                                    2-5


(car 'l_arg)
(cdr 'l_arg)

     RETURNS: _c_o_n_s cell.  (_c_a_r (_c_o_n_s x y)) is always x, (_c_d_r
              (_c_o_n_s  x  y)) is always y.  In FRANZ LISP, the
              cdr portion is located first in memory.   This
              is  hardly  noticeable, and we mention it pri-
              marily as a curiosity.

(c..r 'lh_arg)

     WHERE:   the .. represents any positive number  of  a's
              and d's.

     RETURNS: the result of accessing the list structure  in
              the  way determined by the function name.  The
              a's and d's are read from right to left,  a  d
              directing  the access down the cdr part of the
              list cell and an a down the car part.

     NOTE: lh_arg may also be nil, and it is guaranteed that
           the  car  and  cdr of nil is nil.  If lh_arg is a
           hunk,  then  (_c_a_r '_l_h__a_r_g)   is   the   same   as
           (_c_x_r _1 '_l_h__a_r_g) and  (_c_d_r '_l_h__a_r_g) is the same as
           (_c_x_r _0 '_l_h__a_r_g).
           It is generally hard to read and  understand  the
           context  of  functions  with large strings of a's
           and d's, but these  functions  are  supported  by
           rapid  accessing  and open-compiling (see Chapter
           12).

(nth 'x_index 'l_list)

     RETURNS: the nth element of l_list, assuming zero-based
              index.   Thus  (nth  0  l_list) is the same as
              (car l_list).  _n_t_h is both a function,  and  a
              compiler  macro,  so  that more efficient code
              might be generated than for _n_t_h_e_l_e_m (described
              below).

     NOTE: If x_arg1 is non-positive  or  greater  than  the
           length of the list, nil is returned.










9

9                                   Printed: January 31, 1984







Data Structure Access                                    2-6


(nthcdr 'x_index 'l_list)

     RETURNS: the result of  _c_d_ring  down  the  list  l_list
              x_index times.

     NOTE: If    x_index    is    less    than    0,    then
           (_c_o_n_s _n_i_l '_l__l_i_s_t) is returned.

(nthelem 'x_arg1 'l_arg2)

     RETURNS: The x_arg1'_s_t element of the list l_arg2.

     NOTE: This function comes from the PDP-11 Lisp system.

(last 'l_arg)

     RETURNS: the last list cell in the list l_arg.

     EXAMPLE: _l_a_s_t does NOT return the  last  element  of  a
              list!
              (_l_a_s_t '(_a _b)) = (b)

(ldiff 'l_x 'l_y)

     RETURNS: a  list  of all elements in l_x but not in l_y
              , i.e., the list difference of l_x and l_y.

     NOTE: l_y must be a tail of l_x, i.e., _e_q to the result
           of  applying  some  number  of _c_d_r's to l_x. Note
           that  the  value   of   _l_d_i_f_f   is   always   new
           list  structure  unless l_y is nil, in which case
           (_l_d_i_f_f _l__x _n_i_l) is l_x itself.  If l_y   is   not
           a  tail  of  l_x, _l_d_i_f_f generates an error.

     EXAMPLE: (_l_d_i_f_f '_l__x (_m_e_m_b_e_r '_g__f_o_o  '_l__x))  gives  all
              elements in l_x up to the first g_foo.



      2.1.4.  list manipulation

(rplaca 'lh_arg1 'g_arg2)

     RETURNS: the modified lh_arg1.

     SIDE EFFECT: the car of lh_arg1 is set to  g_arg2.   If
                  lh_arg1  is a hunk then the second element
                  of the hunk is set to g_arg2.




9

9                                   Printed: January 31, 1984







Data Structure Access                                    2-7


(rplacd 'lh_arg1 'g_arg2)

     RETURNS: the modified lh_arg1.

     SIDE EFFECT: the cdr of lh_arg2 is set to  g_arg2.   If
                  lh_arg1  is  a hunk then the first element
                  of the hunk is set to g_arg2.


(attach 'g_x 'l_l)

     RETURNS: l_l whose _c_a_r is now g_x, whose  _c_a_d_r  is  the
              original (_c_a_r _l__l), and whose _c_d_d_r is the ori-
              ginal (_c_d_r _l__l).

     NOTE: what happens is that g_x is added to  the  begin-
           ning  of  list l_l  yet maintaining the same list
           cell  at the beginning of the list.

(delete 'g_val 'l_list ['x_count])

     RETURNS: the result of  splicing  g_val  from  the  top
              level of l_list no more than x_count times.

     NOTE: x_count defaults to a very large number, thus  if
           x_count  is  not  given, all occurrences of g_val
           are removed from the top level of l_list.   g_val
           is compared with successive _c_a_r's of l_list using
           the function _e_q_u_a_l.

     SIDE EFFECT: l_list is modified using  rplacd,  no  new
                  list cells are used.

(delq 'g_val 'l_list ['x_count])
(dremove 'g_val 'l_list ['x_count])

     RETURNS: the result of  splicing  g_val  from  the  top
              level of l_list no more than x_count times.

     NOTE: _d_e_l_q (and _d_r_e_m_o_v_e) are the same as _d_e_l_e_t_e  except
           that _e_q is used for comparison instead of _e_q_u_a_l.











9

9                                   Printed: January 31, 1984







Data Structure Access                                    2-8



    ____________________________________________________

    ; note that you should use the value returned by _d_e_l_e_t_e or _d_e_l_q
    ; and not assume that g_val will always show the deletions.
    ; For example

    -> (_s_e_t_q _t_e_s_t '(_a _b _c _a _d _e))
    (a b c a d e)
    -> (_d_e_l_e_t_e '_a _t_e_s_t)
    (b c d e)         ; the value returned is what we would expect
    -> _t_e_s_t
    (a b c d e)       ; but test still has the first a in the list!
    ____________________________________________________




(remq 'g_x 'l_l ['x_count])
(remove 'g_x 'l_l)

     RETURNS: a _c_o_p_y of l_l  with  all  top  level  elements
              _e_q_u_a_l to g_x removed.  _r_e_m_q uses _e_q instead of
              _e_q_u_a_l for comparisons.

     NOTE: remove does not modify its arguments like _d_e_l_e_t_e,
           and _d_e_l_q do.

(insert 'g_object 'l_list 'u_comparefn 'g_nodups)

     RETURNS: a list consisting of l_list with g_object des-
              tructively  inserted  in a place determined by
              the ordering function u_comparefn.

     NOTE: (_c_o_m_p_a_r_e_f_n '_g__x  '_g__y)  should  return  something
           non-nil  if  g_x can precede g_y in sorted order,
           nil if g_y must precede g_x.  If  u_comparefn  is
           nil, alphabetical order will be used. If g_nodups
           is non-nil, an element will not be inserted if an
           equal  element  is  already  in the list.  _i_n_s_e_r_t
           does binary search to determine where  to  insert
           the new element.










9

9                                   Printed: January 31, 1984







Data Structure Access                                    2-9


(merge 'l_data1 'l_data2 'u_comparefn)

     RETURNS: the merged list of the two input sorted  lists
              l_data1  and  l_data1  using binary comparison
              function u_comparefn.

     NOTE: (_c_o_m_p_a_r_e_f_n '_g__x  '_g__y)  should  return  something
           non-nil  if  g_x can precede g_y in sorted order,
           nil if g_y must precede g_x.  If  u_comparefn  is
           nil,    alphabetical    order   will   be   used.
           u_comparefn should be thought of as "less than or
           equal".   _m_e_r_g_e  changes  both  of its data argu-
           ments.

(subst 'g_x 'g_y 'l_s)
(dsubst 'g_x 'g_y 'l_s)

     RETURNS: the result of substituting g_x for  all  _e_q_u_a_l
              occurrences of g_y  at all levels in l_s.

     NOTE: If g_y is a symbol, _e_q will be used for comparis-
           ons.   The function _s_u_b_s_t does not modify l_s but
           the function  _d_s_u_b_s_t  (destructive  substitution)
           does.

(lsubst 'l_x 'g_y 'l_s)

     RETURNS: a copy of l_s  with l_x spliced in  for  every
              occurrence  of  of g_y at all levels. Splicing
              in means that the parentheses surrounding  the
              list  l_x  are  removed  as  the example below
              shows.


    ____________________________________________________

    -> (_s_u_b_s_t '(_a _b _c) '_x '(_x _y _z (_x _y _z) (_x _y _z)))
    ((a b c) y z ((a b c) y z) ((a b c) y z))
    -> (_l_s_u_b_s_t '(_a _b _c) '_x '(_x _y _z (_x _y _z) (_x _y _z)))
    (a b c y z (a b c y z) (a b c y z))
    ____________________________________________________











9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-10


(subpair 'l_old 'l_new 'l_expr)

     WHERE:   there are  the  same  number  of  elements  in
              l_old as l_new.

     RETURNS: the list l_expr  with  all  occurrences  of  a
              object  in l_old replaced by the corresponding
              one in l_new.  When a substitution is made,  a
              copy  of  the  value  to  substitute in is not
              made.

     EXAMPLE: (_s_u_b_p_a_i_r '(_a _c)' (_x _y) '(_a _b _c _d)) = (_x _b _y _d)


(nconc 'l_arg1 'l_arg2 ['l_arg3 ...])

     RETURNS: A list consisting of the  elements  of  l_arg1
              followed by the elements of l_arg2 followed by
              l_arg3 and so on.

     NOTE: The _c_d_r of  the  last  list  cell  of  l_arg_i  is
           changed to point to l_arg_i+_1.


    ____________________________________________________

    ; _n_c_o_n_c is faster than _a_p_p_e_n_d because it doesn't allocate new list cells.
    -> (_s_e_t_q _l_i_s_1 '(_a _b _c))
    (a b c)
    -> (_s_e_t_q _l_i_s_2 '(_d _e _f))
    (d e f)
    -> (_a_p_p_e_n_d _l_i_s_1 _l_i_s_2)
    (a b c d e f)
    -> _l_i_s_1
    (a b c)       ; note that lis1 has not been changed by _a_p_p_e_n_d
    -> (_n_c_o_n_c _l_i_s_1 _l_i_s_2)
    (a b c d e f) ; _n_c_o_n_c returns the same value as _a_p_p_e_n_d
    -> _l_i_s_1
    (a b c d e f) ; but in doing so alters lis1
    ____________________________________________________












9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-11


(reverse 'l_arg)
(nreverse 'l_arg)

     RETURNS: the list l_arg with the elements  at  the  top
              level in reverse  order.

     NOTE: The function _n_r_e_v_e_r_s_e does the reversal in place,
           that is the list structure is modified.

(nreconc 'l_arg 'g_arg)

     EQUIVALENT TO: (_n_c_o_n_c (_n_r_e_v_e_r_s_e '_l__a_r_g) '_g__a_r_g)




   2.2.  Predicates

           The following functions test  for  properties  of
      data  objects.  When  the result of the test is either
      'false' or 'true',  then  nil  will  be  returned  for
      'false' and something other than nil (often t) will be
      returned for 'true'.

(arrayp 'g_arg)

     RETURNS: t iff g_arg is of type array.

(atom 'g_arg)

     RETURNS: t iff g_arg is not a list or hunk object.

     NOTE: (_a_t_o_m '()) returns t.

(bcdp 'g_arg)

     RETURNS: t iff g_arg is a data object of type binary.

     NOTE: This function is a throwback to the  PDP-11  Lisp
           system.   The  name stands for binary code predi-
           cate.











9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-12


(bigp 'g_arg)

     RETURNS: t iff g_arg is a bignum.

(dtpr 'g_arg)

     RETURNS: t iff g_arg is a list cell.

     NOTE: that (dtpr '()) is nil.

(hunkp 'g_arg)

     RETURNS: t iff g_arg is a hunk.

(listp 'g_arg)

     RETURNS: t iff g_arg is a list object or nil.

(stringp 'g_arg)

     RETURNS: t iff g_arg is a string.

(symbolp 'g_arg)

     RETURNS: t iff g_arg is a symbol.

(valuep 'g_arg)

     RETURNS: t iff g_arg is a value cell

(vectorp 'v_vector)

     RETURNS: t iff the argument is a vector.

(vectorip 'v_vector)

     RETURNS: t iff the argument is an immediate-vector.

(type 'g_arg)
(typep 'g_arg)

     RETURNS: a symbol whose pname  describes  the  type  of
              g_arg.









9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-13


(signp s_test 'g_val)

     RETURNS: t iff g_val is a number  and  the  given  test
              s_test on g_val returns true.

     NOTE: The fact that _s_i_g_n_p simply returns nil  if  g_val
           is  not  a  number is probably the most important
           reason that _s_i_g_n_p is used.  The permitted  values
           for  s_test  and what they mean are given in this
           table.

8                         ____________________
                          s_test   tested

8                         ________________________________________
                          l        g_val < 0
                          le       g_val <_ 0
                          e        g_val = 0
                          n        g_val =/ 0
                          ge       g_val >_ 0
                          g        g_val > 0
8                         ____________________
7                        |7|7|7|7|7|7|7|7|







                                            |7|7|7|7|7|7|7|7|










(eq 'g_arg1 'g_arg2)

     RETURNS: t if g_arg1 and g_arg2 are the exact same lisp
              object.

     NOTE: _E_q simply tests if g_arg1 and g_arg2 are  located
           in  the exact same place in memory.  Lisp objects
           which print the same are not necessarily _e_q.  The
           only  objects  guaranteed  to  be _e_q are interned
           symbols with the same print name.  [Unless a sym-
           bol  is  created  in  a special way (such as with
           _u_c_o_n_c_a_t or _m_a_k_n_a_m) it will be interned.]

(neq 'g_x 'g_y)

     RETURNS: t if g_x is not _e_q to g_y, otherwise nil.

(equal 'g_arg1 'g_arg2)
(eqstr 'g_arg1 'g_arg2)

     RETURNS: t iff g_arg1 and g_arg2 have the  same  struc-
              ture as described below.

     NOTE: g_arg and g_arg2 are _e_q_u_a_l if

     (1)  they are _e_q.

     (2)  they are both fixnums with the same value




9                                   Printed: January 31, 1984







Data Structure Access                                   2-14


     (3)  they are both flonums with the same value

     (4)  they are both bignums with the same value

     (5)  they are both strings and are identical.

     (6)  they are both lists and their cars  and  cdrs  are
          _e_q_u_a_l.


    ____________________________________________________

    ; _e_q is much faster than _e_q_u_a_l, especially in compiled code,
    ; however you cannot use _e_q to test for equality of numbers outside
    ; of the range -1024 to 1023.  _e_q_u_a_l will always work.
    -> (_e_q _1_0_2_3 _1_0_2_3)
    t
    -> (_e_q _1_0_2_4 _1_0_2_4)
    nil
    -> (_e_q_u_a_l _1_0_2_4 _1_0_2_4)
    t
    ____________________________________________________





(not 'g_arg)
(null 'g_arg)

     RETURNS: t iff g_arg is nil.


(member 'g_arg1 'l_arg2)
(memq 'g_arg1 'l_arg2)

     RETURNS: that part of the  l_arg2  beginning  with  the
              first  occurrence of g_arg1.  If g_arg1 is not
              in the top level of l_arg2, nil is returned.

     NOTE: _m_e_m_b_e_r tests for equality with _e_q_u_a_l, _m_e_m_q  tests
           for equality with _e_q.




   2.3.  Symbols and Strings

           In many of the following functions  the  distinc-
      tion  between symbols and strings is somewhat blurred.
      To remind ourselves of the difference, a string  is  a
      null terminated sequence of characters, stored as com-
      pactly as possible.  Strings are used as constants  in


                                   Printed: January 31, 1984







Data Structure Access                                   2-15


      FRANZ  LISP.   They  _e_v_a_l to themselves.  A symbol has
      additional structure: a value, property list, function
      binding,  as  well  as its external representation (or
      print-name).  If a symbol  is  given  to  one  of  the
      string  manipulation  functions  below, its print name
      will be used as the string.

           Another popular way to represent strings in  Lisp
      is  as  a  list of fixnums which represent characters.
      The suffix 'n' to a string manipulation function indi-
      cates that it returns a string in this form.



      2.3.1.  symbol and string creation

(concat ['stn_arg1 ... ])
(uconcat ['stn_arg1 ... ])

     RETURNS: a symbol whose print name  is  the  result  of
              concatenating  the print names, string charac-
              ters  or  numerical  representations  of   the
              sn_arg_i.

     NOTE: If no arguments are given, a symbol with  a  null
           pname  is  returned.   _c_o_n_c_a_t  places  the symbol
           created on the oblist, the function _u_c_o_n_c_a_t  does
           the  same thing but does not place the new symbol
           on the oblist.

     EXAMPLE: (_c_o_n_c_a_t '_a_b_c (_a_d_d _3 _4) "_d_e_f") = abc7def

(concatl 'l_arg)

     EQUIVALENT TO: (_a_p_p_l_y '_c_o_n_c_a_t '_l__a_r_g)


(implode 'l_arg)
(maknam 'l_arg)

     WHERE:   l_arg is a list of symbols, strings and  small
              fixnums.

     RETURNS: The symbol whose print name is the  result  of
              concatenating  the  first  characters  of  the
              print names of the symbols and strings in  the
              list.    Any  fixnums  are  converted  to  the
              equivalent ascii character.  In order to  con-
              catenate  entire  strings  or print names, use
              the function _c_o_n_c_a_t.

     NOTE: _i_m_p_l_o_d_e interns the  symbol  it  creates,  _m_a_k_n_a_m
           does not.


                                   Printed: January 31, 1984







Data Structure Access                                   2-16


(gensym ['s_leader])

     RETURNS: a new uninterned atom beginning with the first
              character  of  s_leader's  pname, or beginning
              with g if s_leader is not given.

     NOTE: The  symbol  looks  like  x0nnnnn  where   x   is
           s_leader's  first  character  and  nnnnn  is  the
           number of times you have called gensym.

(copysymbol 's_arg 'g_pred)

     RETURNS: an uninterned symbol with the same print  name
              as  s_arg.   If  g_pred  is  non nil, then the
              value, function binding and property  list  of
              the new symbol are made _e_q to those of s_arg.


(ascii 'x_charnum)

     WHERE:   x_charnum is between 0 and 255.

     RETURNS: a symbol whose print name is the single  char-
              acter    whose    fixnum   representation   is
              x_charnum.


(intern 's_arg)

     RETURNS: s_arg

     SIDE EFFECT: s_arg is put on the oblist if  it  is  not
                  already there.

(remob 's_symbol)

     RETURNS: s_symbol

     SIDE EFFECT: s_symbol is removed from the oblist.

(rematom 's_arg)

     RETURNS: t if s_arg is indeed an atom.

     SIDE EFFECT: s_arg is  put  on  the  free  atoms  list,
                  effectively reclaiming an atom cell.

     NOTE: This function does _n_o_t check to see if  s_arg  is
           on  the  oblist  or is referenced anywhere.  Thus
           calling _r_e_m_a_t_o_m on an  atom  in  the  oblist  may
           result in disaster when that atom cell is reused!

9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-17


      2.3.2.  string and symbol predicates

(boundp 's_name)

     RETURNS: nil  if s_name is unbound:  that  is,  it  has
              never  been  given a value.  If x_name has the
              value g_val, then (nil . g_val)  is  returned.
              See also _m_a_k_u_n_b_o_u_n_d.

(alphalessp 'st_arg1 'st_arg2)

     RETURNS: t iff the `name' of st_arg1 is  alphabetically
              less  than the name of st_arg2. If st_arg is a
              symbol then its `name' is its print name.   If
              st_arg  is  a  string,  then its `name' is the
              string itself.



      2.3.3.  symbol and string accessing

(symeval 's_arg)

     RETURNS: the value of symbol s_arg.

     NOTE: It is illegal to ask for the value of an  unbound
           symbol.   This  function  has  the same effect as
           _e_v_a_l, but compiles into much more efficient code.

(get_pname 's_arg)

     RETURNS: the string which is the print name of s_arg.

(plist 's_arg)

     RETURNS: the property list of s_arg.

(getd 's_arg)

     RETURNS: the function definition of  s_arg  or  nil  if
              there is no function definition.

     NOTE: the function definition may turn  out  to  be  an
           array header.








9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-18


(getchar 's_arg 'x_index)
(nthchar 's_arg 'x_index)
(getcharn 's_arg 'x_index)

     RETURNS: the x_index_t_h character of the print  name  of
              s_arg  or  nil  if  x_index  is less than 1 or
              greater than the length of s_arg's print name.

     NOTE: _g_e_t_c_h_a_r and _n_t_h_c_h_a_r return a symbol with a single
           character print name, _g_e_t_c_h_a_r_n returns the fixnum
           representation of the character.

(substring 'st_string 'x_index ['x_length])
(substringn 'st_string 'x_index ['x_length])

     RETURNS: a string of length at most  x_length  starting
              at x_index_t_h character in the string.

     NOTE: If x_length is not given, all of  the  characters
           for   x_index  to  the  end  of  the  string  are
           returned.  If  x_index  is  negative  the  string
           begins  at  the x_index_t_h character from the end.
           If x_index is out of bounds, nil is returned.

     NOTE: _s_u_b_s_t_r_i_n_g returns a list of  symbols,  _s_u_b_s_t_r_i_n_g_n
           returns  a  list  of  fixnums.   If _s_u_b_s_t_r_i_n_g_n is
           given a 0 x_length argument then a single  fixnum
           which is the x_index_t_h character is returned.



      2.3.4.  symbol and string manipulation

(set 's_arg1 'g_arg2)

     RETURNS: g_arg2.

     SIDE EFFECT: the value of s_arg1 is set to g_arg2.

(setq s_atm1 'g_val1 [ s_atm2 'g_val2 ... ... ])

     WHERE:   the arguments are  pairs  of  atom  names  and
              expressions.

     RETURNS: the last g_val_i.

     SIDE EFFECT: each s_atm_i  is  set  to  have  the  value
                  g_val_i.

     NOTE: _s_e_t evaluates all of its arguments, _s_e_t_q does not
           evaluate the s_atm_i.

9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-19


(desetq sl_pattern1 'g_exp1 [... ...])

     RETURNS: g_expn

     SIDE EFFECT: This  acts  just  like  _s_e_t_q  if  all  the
                  sl_pattern_i  are  symbols.  If sl_pattern_i
                  is a list then it   is  a  template  which
                  should  have  the same structure as g_exp_i
                  The symbols in sl_pattern are assigned  to
                  the  corresponding  parts  of g_exp.  (See
                  also _s_e_t_f )

     EXAMPLE: (_d_e_s_e_t_q (_a _b (_c . _d)) '(_1 _2 (_3 _4 _5)))
              sets a to 1, b to 2, c to 3, and d to (4 5).


(setplist 's_atm 'l_plist)

     RETURNS: l_plist.

     SIDE EFFECT: the property  list  of  s_atm  is  set  to
                  l_plist.

(makunbound 's_arg)

     RETURNS: s_arg

     SIDE EFFECT: the value of s_arg is made `unbound'.   If
                  the interpreter attempts to evaluate s_arg
                  before it  is  again  given  a  value,  an
                  unbound variable error will occur.

(aexplode 's_arg)
(explode 'g_arg)
(aexplodec 's_arg)
(explodec 'g_arg)
(aexploden 's_arg)
(exploden 'g_arg)

     RETURNS: a list of the characters  used  to  print  out
              s_arg or g_arg.

     NOTE: The functions beginning  with  'a'  are  internal
           functions  which are limited to symbol arguments.
           The functions _a_e_x_p_l_o_d_e and _e_x_p_l_o_d_e return a  list
           of  characters which _p_r_i_n_t would use to print the
           argument. These characters include all  necessary
           escape   characters.    Functions  _a_e_x_p_l_o_d_e_c  and
           _e_x_p_l_o_d_e_c return a list of characters which  _p_a_t_o_m
           would  use  to print the argument (i.e. no escape
           characters).  Functions  _a_e_x_p_l_o_d_e_n  and  _e_x_p_l_o_d_e_n
           are similar to _a_e_x_p_l_o_d_e_c and _e_x_p_l_o_d_e_c except that
           a list of fixnum equivalents  of  characters  are


                                   Printed: January 31, 1984







Data Structure Access                                   2-20


           returned.


    ____________________________________________________

    -> (_s_e_t_q _x '|_q_u_o_t_e _t_h_i_s _\| _o_k?|)
    |quote this \| ok?|
    -> (_e_x_p_l_o_d_e _x)
    (q u o t e |\\| | | t h i s |\\| | | |\\| |\|| |\\| | | o k ?)
    ; note that |\\| just means the single character: backslash.
    ; and |\|| just means the single character: vertical bar
    ; and | | means the single character: space

    -> (_e_x_p_l_o_d_e_c _x)
    (q u o t e | | t h i s | | |\|| | | o k ?)
    -> (_e_x_p_l_o_d_e_n _x)
    (113 117 111 116 101 32 116 104 105 115 32 124 32 111 107 63)
    ____________________________________________________






   2.4.  Vectors

           See Chapter 9 for a discussion of vectors.   They
      are  less efficient that hunks but more efficient than
      arrays.



      2.4.1.  vector creation

(new-vector 'x_size ['g_fill ['g_prop]])

     RETURNS: A vector of length x_size.  Each data entry is
              initialized to g_fill, or to nil, if the argu-
              ment g_fill is not present.  The vector's pro-
              perty is set to g_prop, or to nil, by default.

(new-vectori-byte 'x_size ['g_fill ['g_prop]])
(new-vectori-word 'x_size ['g_fill ['g_prop]])
(new-vectori-long 'x_size ['g_fill ['g_prop]])

     RETURNS: A vectori with x_size  elements  in  it.   The
              actual  memory requirement is two long words +
              x_size*(n  bytes),  where  n  is  1  for  new-
              vector-byte,  2  for new-vector-word, or 4 for
              new-vectori-long.  Each data entry is initial-
              ized  to  g_fill,  or to zero, if the argument
              g_fill is not present.  The vector's  property
              is set to g_prop, or nil, by default.


                                   Printed: January 31, 1984







Data Structure Access                                   2-21


     Vectors may be created by specifying  multiple  initial
     values:

(vector ['g_val0 'g_val1 ...])

     RETURNS: a vector, with as many data elements as  there
              are arguments.  It is quite possible to have a
              vector with no data  elements.   The  vector's
              property will be a null list.

(vectori-byte ['x_val0 'x_val2 ...])
(vectori-word ['x_val0 'x_val2 ...])
(vectori-long ['x_val0 'x_val2 ...])

     RETURNS: a vectori, with as many data elements as there
              are  arguments.  The arguments are required to
              be fixnums.  Only the low order byte  or  word
              is  used  in  the  case  of  vectori-byte  and
              vectori-word.  The vector's property  will  be
              null.



      2.4.2.  vector reference

(vref 'v_vect 'x_index)
(vrefi-byte 'V_vect 'x_bindex)
(vrefi-word 'V_vect 'x_windex)
(vrefi-long 'V_vect 'x_lindex)

     RETURNS: the desired data element from a  vector.   The
              indices  must  be  fixnums.  Indexing is zero-
              based.  The vrefi functions  sign  extend  the
              data.

(vprop 'Vv_vect)

     RETURNS: The Lisp property associated with a vector.

(vget 'Vv_vect 'g_ind)

     RETURNS: The value stored under g_ind if the Lisp  pro-
              perty  associated  with 'Vv_vect is a disembo-
              died property list.








9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-22


(vsize 'Vv_vect)
(vsize-byte 'V_vect)
(vsize-word 'V_vect)

     RETURNS: the number of data  elements  in  the  vector.
              For  immediate-vectors,  the  functions vsize-
              byte and vsize-word return the number of  data
              elements,  if one thinks of the binary data as
              being comprised of bytes or words.



      2.4.3.  vector modfication

(vset 'v_vect 'x_index 'g_val)
(vseti-byte 'V_vect 'x_bindex 'x_val)
(vseti-word 'V_vect 'x_windex 'x_val)
(vseti-long 'V_vect 'x_lindex 'x_val)

     RETURNS: the datum.

     SIDE EFFECT: The indexed element of the vector  is  set
                  to  the value.  As noted above, for vseti-
                  word and vseti-byte,  the  index  is  con-
                  strued  as  the number of the data element
                  within the  vector.   It  is  not  a  byte
                  address.   Also,  for those two functions,
                  the low order byte or  word  of  x_val  is
                  what is stored.

(vsetprop 'Vv_vect 'g_value)

     RETURNS: g_value.  This should be either a symbol or  a
              disembodied  property list whose _c_a_r is a sym-
              bol identifying the type of the vector.

     SIDE EFFECT: the property list of  Vv_vect  is  set  to
                  g_value.

(vputprop 'Vv_vect 'g_value 'g_ind)

     RETURNS: g_value.

     SIDE EFFECT: If the vector property  of  Vv_vect  is  a
                  disembodied  property  list, then vputprop
                  adds the value g_value under the indicator
                  g_ind.  Otherwise, the old vector property
                  is made the first element of the list.




9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-23


   2.5.  Arrays

           See Chapter  9  for  a  complete  description  of
      arrays.  Some of these functions are part of a Maclisp
      array compatibility package representing only one sim-
      ple way of using the array structure of FRANZ LISP.



      2.5.1.  array creation

(marray 'g_data 's_access 'g_aux 'x_length 'x_delta)

     RETURNS: an array type with the fields set up from  the
              above  arguments  in  the  obvious  way  (see
              1.2.10).

(*array 's_name 's_type 'x_dim1 ... 'x_dim_n)
(array s_name s_type x_dim1 ... x_dim_n)

     WHERE:   s_type may be one of t, nil,  fixnum,  flonum,
              fixnum-block and flonum-block.

     RETURNS: an array of type s_type with n  dimensions  of
              extents given by the x_dim_i.

     SIDE EFFECT: If s_name is non nil, the function defini-
                  tion  of s_name is set to the array struc-
                  ture returned.

     NOTE: These  functions  create  a  Maclisp   compatible
           array.  In FRANZ LISP arrays of type t, nil, fix-
           num and flonum are equivalent and the elements of
           these  arrays  can  be  any  type of lisp object.
           Fixnum-block and  flonum-block  arrays  are  res-
           tricted  to  fixnums and flonums respectively and
           are used mainly to communicate with foreign func-
           tions (see 8.5).

     NOTE: *_a_r_r_a_y evaluates its arguments, _a_r_r_a_y does not.



      2.5.2.  array predicate








9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-24


(arrayp 'g_arg)

     RETURNS: t iff g_arg is of type array.



      2.5.3.  array accessors


(getaccess 'a_array)
(getaux 'a_array)
(getdelta 'a_array)
(getdata 'a_array)
(getlength 'a_array)

     RETURNS: the field of the array object a_array given by
              the function name.

(arrayref 'a_name 'x_ind)

     RETURNS: the  x_ind_t_h  element  of  the  array   object
              a_name.  x_ind of zero accesses the first ele-
              ment.

     NOTE: _a_r_r_a_y_r_e_f uses the data, length and  delta  fields
           of a_name to determine which object to return.

(arraycall s_type 'as_array 'x_ind1 ... )

     RETURNS: the element selected by  the indices from  the
              array a_array of type s_type.

     NOTE: If as_array is a symbol then the function binding
           of this symbol should contain an array object.
           s_type is ignored by _a_r_r_a_y_c_a_l_l  but  is  included
           for compatibility with Maclisp.

(arraydims 's_name)

     RETURNS: a list of the type and  bounds  of  the  array
              s_name.











9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-25


(listarray 'sa_array ['x_elements])

     RETURNS: a  list  of  all  of  the  elements  in  array
              sa_array.   If  x_elements is given, then only
              the first x_elements are returned.



    ____________________________________________________

    ; We will create a 3 by 4 array of general lisp objects
    -> (_a_r_r_a_y _e_r_n_i_e _t _3 _4)
    array[12]

    ; the array header is stored in the function definition slot of the
    ; symbol ernie
    -> (_a_r_r_a_y_p (_g_e_t_d '_e_r_n_i_e))
    t
    -> (_a_r_r_a_y_d_i_m_s (_g_e_t_d '_e_r_n_i_e))
    (t 3 4)

    ; store in ernie[2][2] the list (test list)
    -> (_s_t_o_r_e (_e_r_n_i_e _2 _2) '(_t_e_s_t _l_i_s_t))
    (test list)

    ; check to see if it is there
    -> (_e_r_n_i_e _2 _2)
    (test list)

    ; now use the low level function _a_r_r_a_y_r_e_f to find the same element
    ; arrays are 0 based and row-major (the last subscript varies the fastest)
    ; thus element [2][2] is the 10th element , (starting at 0).
    -> (_a_r_r_a_y_r_e_f (_g_e_t_d '_e_r_n_i_e) _1_0)
    (ptr to)(test list)    ; the result is a value cell (thus the (ptr to))
    ____________________________________________________






      2.5.4.  array manipulation










9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-26


(putaccess 'a_array 'su_func)
(putaux 'a_array 'g_aux)
(putdata 'a_array 'g_arg)
(putdelta 'a_array 'x_delta)
(putlength 'a_array 'x_length)

     RETURNS: the second argument to the function.

     SIDE EFFECT: The field of the array object given by the
                  function  name  is  replaced by the second
                  argument to the function.

(store 'l_arexp 'g_val)

     WHERE:   l_arexp is an expression which  references  an
              array element.

     RETURNS: g_val

     SIDE EFFECT: the array location which contains the ele-
                  ment  which  l_arexp references is changed
                  to contain g_val.

(fillarray 's_array 'l_itms)

     RETURNS: s_array

     SIDE EFFECT: the array s_array is filled with  elements
                  from l_itms.  If there are not enough ele-
                  ments in l_itms to fill the entire  array,
                  then the last element of l_itms is used to
                  fill the remaining parts of the array.



   2.6.  Hunks

           Hunks are  vector-like  objects  whose  size  can
      range  from  1 to 128 elements.  Internally, hunks are
      allocated in sizes which are powers of 2.  In order to
      create  hunks  of  a  given size, a hunk with at least
      that many elements is allocated  and  a  distinguished
      symbol   EMPTY   is   placed  in  those  elements  not
      requested.  Most hunk  functions  respect  those  dis-
      tinguished  symbols,  but  there are two (*_m_a_k_h_u_n_k and
      *_r_p_l_a_c_x) which will overwrite the  distinguished  sym-
      bol.





9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-27


      2.6.1.  hunk creation

(hunk 'g_val1 ['g_val2 ... 'g_val_n])

     RETURNS: a hunk of length n whose elements are initial-
              ized to the g_val_i.

     NOTE: the maximum size of a hunk is 128.

     EXAMPLE: (_h_u_n_k _4 '_s_h_a_r_p '_k_e_y_s) = {4 sharp keys}

(makhunk 'xl_arg)

     RETURNS: a hunk of length  xl_arg  initialized  to  all
              nils  if  xl_arg  is a fixnum.  If xl_arg is a
              list,  then  we  return   a   hunk   of   size
              (_l_e_n_g_t_h '_x_l__a_r_g)  initialized  to the elements
              in xl_arg.

     NOTE: (_m_a_k_h_u_n_k '(_a _b _c))     is      equivalent      to
           (_h_u_n_k '_a '_b '_c).

     EXAMPLE: (_m_a_k_h_u_n_k _4) = {_n_i_l _n_i_l _n_i_l _n_i_l}

(*makhunk 'x_arg)

     RETURNS: a hunk of size 2[x_arg] initialized to EMPTY.

     NOTE: This is only to be used by such functions as _h_u_n_k
           and _m_a_k_h_u_n_k which create and initialize hunks for
           users.



      2.6.2.  hunk accessor

(cxr 'x_ind 'h_hunk)

     RETURNS: element x_ind (starting at 0) of hunk h_hunk.

(hunk-to-list 'h_hunk)

     RETURNS: a list consisting of the elements of h_hunk.



      2.6.3.  hunk manipulators





9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-28


(rplacx 'x_ind 'h_hunk 'g_val)
(*rplacx 'x_ind 'h_hunk 'g_val)

     RETURNS: h_hunk

     SIDE EFFECT: Element x_ind (starting at 0) of h_hunk is
                  set to g_val.

     NOTE: _r_p_l_a_c_x will not modify one of  the  distinguished
           (EMPTY) elements whereas *_r_p_l_a_c_x will.

(hunksize 'h_arg)

     RETURNS: the size of the hunk h_arg.

     EXAMPLE: (_h_u_n_k_s_i_z_e (_h_u_n_k _1 _2 _3)) = 3



   2.7.  Bcds

           A bcd object contains a pointer to compiled  code
      and  to  the type of function object the compiled code
      represents.

(getdisc 'y_bcd)
(getentry 'y_bcd)

     RETURNS: the field of the bcd object given by the func-
              tion name.

(putdisc 'y_func 's_discipline)

     RETURNS: s_discipline

     SIDE EFFECT: Sets the discipline  field  of  y_func  to
                  s_discipline.



   2.8.  Structures

           There are three common structures constructed out
      of  list  cells: the assoc list, the property list and
      the tconc list.  The functions below manipulate  these
      structures.



      2.8.1.  assoc list

              An `assoc list' (or alist) is  a  common  lisp
         data structure.  It has the form


                                   Printed: January 31, 1984







Data Structure Access                                   2-29


         ((key1 . value1) (key2 . value2) (key3 . value3) ... (keyn . valuen))

(assoc 'g_arg1 'l_arg2)
(assq 'g_arg1 'l_arg2)

     RETURNS: the first top level element  of  l_arg2  whose
              _c_a_r is _e_q_u_a_l (with _a_s_s_o_c) or _e_q (with _a_s_s_q) to
              g_arg1.

     NOTE: Usually l_arg2 has an _a-_l_i_s_t structure and g_arg1
           acts as key.

(sassoc 'g_arg1 'l_arg2 'sl_func)

     RETURNS: the                 result                  of
              (_c_o_n_d ((_a_s_s_o_c '_g__a_r_g '_l__a_r_g_2) (_a_p_p_l_y '_s_l__f_u_n_c _n_i_l)))

     NOTE: sassoc is written as a macro.

(sassq 'g_arg1 'l_arg2 'sl_func)

     RETURNS: the                 result                  of
              (_c_o_n_d ((_a_s_s_q '_g__a_r_g '_l__a_r_g_2) (_a_p_p_l_y '_s_l__f_u_n_c _n_i_l)))

     NOTE: sassq is written as a macro.



    ____________________________________________________

    ; _a_s_s_o_c or _a_s_s_q is given a key and an assoc list and returns
    ; the key and value item if it exists, they differ only in how they test
    ; for equality of the keys.

    -> (_s_e_t_q _a_l_i_s_t '((_a_l_p_h_a . _a) ( (_c_o_m_p_l_e_x _k_e_y) . _b) (_j_u_n_k . _x)))
    ((alpha . a) ((complex key) . b) (junk . x))

    ; we should use _a_s_s_q when the key is an atom
    -> (_a_s_s_q '_a_l_p_h_a _a_l_i_s_t)
    (alpha . a)

    ; but it may not work when the key is a list
    -> (_a_s_s_q '(_c_o_m_p_l_e_x _k_e_y) _a_l_i_s_t)
    nil

    ; however _a_s_s_o_c will always work
    -> (_a_s_s_o_c '(_c_o_m_p_l_e_x _k_e_y) _a_l_i_s_t)
    ((complex key) . b)
    ____________________________________________________



9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-30


(sublis 'l_alst 'l_exp)

     WHERE:   l_alst is an _a-_l_i_s_t.

     RETURNS: the list l_exp with every occurrence  of  key_i
              replaced by val_i.

     NOTE: new list structure is returned to prevent modifi-
           cation  of l_exp.  When a substitution is made, a
           copy of the value to substitute in is not made.



      2.8.2.  property list

              A property list  consists  of  an  alternating
         sequence  of  keys and values.  Normally a property
         list is stored on a symbol. A list is  a  'disembo-
         died' property list if it contains an odd number of
         elements, the first of which is ignored.

(plist 's_name)

     RETURNS: the property list of s_name.

(setplist 's_atm 'l_plist)

     RETURNS: l_plist.

     SIDE EFFECT: the property  list  of  s_atm  is  set  to
                  l_plist.


(get 'ls_name 'g_ind)

     RETURNS: the value under indicator g_ind  in  ls_name's
              property list if ls_name is a symbol.

     NOTE: If there is no indicator g_ind in ls_name's  pro-
           perty list nil is returned.  If ls_name is a list
           of an odd number of elements then it is a  disem-
           bodied  property list. _g_e_t searches a disembodied
           property list by starting at its _c_d_r, and compar-
           ing every other element with g_ind, using _e_q.








9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-31


(getl 'ls_name 'l_indicators)

     RETURNS: the property list  ls_name  beginning  at  the
              first  indicator which is a member of the list
              l_indicators, or nil if none of the indicators
              in  l_indicators  are  on  ls_name's  property
              list.

     NOTE: If ls_name is a list, then it is assumed to be  a
           disembodied property list.


(putprop 'ls_name 'g_val 'g_ind)
(defprop ls_name g_val g_ind)

     RETURNS: g_val.

     SIDE EFFECT: Adds to the property list of  ls_name  the
                  value g_val under the indicator g_ind.

     NOTE: _p_u_t_p_r_o_p evaluates it arguments, _d_e_f_p_r_o_p does not.
           ls_name  may  be a disembodied property list, see
           _g_e_t.

(remprop 'ls_name 'g_ind)

     RETURNS: the portion of  ls_name's property list begin-
              ning  with  the  property  under the indicator
              g_ind.  If there  is  no  g_ind  indicator  in
              ls_name's plist, nil is returned.

     SIDE EFFECT: the value under indicator g_ind and  g_ind
                  itself  is  removed from the property list
                  of ls_name.

     NOTE: ls_name may be a disembodied property  list,  see
           _g_e_t.















9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-32



    ____________________________________________________

    -> (_p_u_t_p_r_o_p '_x_l_a_t_e '_a '_a_l_p_h_a)
    a
    -> (_p_u_t_p_r_o_p '_x_l_a_t_e '_b '_b_e_t_a)
    b
    -> (_p_l_i_s_t '_x_l_a_t_e)
    (alpha a beta b)
    -> (_g_e_t '_x_l_a_t_e '_a_l_p_h_a)
    a
    ; use of a disembodied property list:
    -> (_g_e_t '(_n_i_l _f_a_t_e_m_a_n _r_j_f _s_k_l_o_w_e_r _k_l_s _f_o_d_e_r_a_r_o _j_k_f) '_s_k_l_o_w_e_r)
    kls
    ____________________________________________________






      2.8.3.  tconc structure

              A tconc structure is a special  type  of  list
         designed to make it easy to add objects to the end.
         It consists of a list cell whose _c_a_r  points  to  a
         list  of the elements added with _t_c_o_n_c or _l_c_o_n_c and
         whose _c_d_r points to the last list cell of the  list
         pointed to by the _c_a_r.

(tconc 'l_ptr 'g_x)

     WHERE:   l_ptr is a tconc structure.

     RETURNS: l_ptr with g_x added to the end.

(lconc 'l_ptr 'l_x)

     WHERE:   l_ptr is a tconc structure.

     RETURNS: l_ptr with the list l_x spliced in at the end.











9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-33



    ____________________________________________________

    ; A _t_c_o_n_c structure can be initialized in two  ways.
    ; nil can be given to _t_c_o_n_c in which case _t_c_o_n_c will generate
    ; a _t_c_o_n_c structure.

    ->(_s_e_t_q _f_o_o (_t_c_o_n_c _n_i_l _1))
    ((1) 1)

    ; Since _t_c_o_n_c destructively adds to
    ; the list, you can now add to foo without using _s_e_t_q again.

    ->(_t_c_o_n_c _f_o_o _2)
    ((1 2) 2)
    ->_f_o_o
    ((1 2) 2)

    ; Another way to create a null  _t_c_o_n_c structure
    ; is to use (_n_c_o_n_s _n_i_l).

    ->(_s_e_t_q _f_o_o (_n_c_o_n_s _n_i_l))
    (nil)
    ->(_t_c_o_n_c _f_o_o _1)
    ((1) 1)

    ; now see what _l_c_o_n_c can do
    -> (_l_c_o_n_c _f_o_o _n_i_l)
    ((1) 1)            ; no change
    -> (_l_c_o_n_c _f_o_o '(_2 _3 _4))
    ((1 2 3 4) 4)
    ____________________________________________________






      2.8.4.  fclosures

              An  fclosure  is  a  functional  object  which
         admits some data manipulations.  They are discussed
         in 8.4.  Internally, they are constructed from vec-
         tors.








9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-34


(fclosure 'l_vars 'g_funobj)

     WHERE:   l_vars is a list of variables, g_funobj is any
              object that can be funcalled (including, fclo-
              sures).

     RETURNS: A vector which is the fclosure.

(fclosure-alist 'v_fclosure)

     RETURNS: An association list representing the variables
              in  the  fclosure.   This is a snapshot of the
              current state of the fclosure.  If  the  bind-
              ings  in  the fclosure are changed, any previ-
              ously  calculated  results  of  _f_c_l_o_s_u_r_e-_a_l_i_s_t
              will not change.

(fclosure-function 'v_fclosure)

     RETURNS: the functional object part of the fclosure.

(fclosurep 'v_fclosure)

     RETURNS: t iff the argument is an fclosure.

(symeval-in-fclosure 'v_fclosure 's_symbol)

     RETURNS: the current binding of a particular symbol  in
              an fclosure.

(set-in-fclosure 'v_fclosure 's_symbol 'g_newvalue)

     RETURNS: g_newvalue.

     SIDE EFFECT: The variable  s_symbol  is  bound  in  the
                  fclosure to g_newvalue.



   2.9.  Random functions

           The following functions don't fall  into  any  of
      the classifications above.









9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-35


(bcdad 's_funcname)

     RETURNS: a fixnum which is the address in memory  where
              the function s_funcname begins.  If s_funcname
              is not a machine coded function (binary)  then
              _b_c_d_a_d returns nil.

(copy 'g_arg)

     RETURNS: A structure _e_q_u_a_l to g_arg but with  new  list
              cells.

(copyint* 'x_arg)

     RETURNS: a fixnum with the same value as x_arg but in a
              freshly allocated cell.

(cpy1 'xvt_arg)

     RETURNS: a new cell of the same type  as  xvt_arg  with
              the same value as xvt_arg.

(getaddress 's_entry1 's_binder1  'st_discipline1  [...  ...
...])

     RETURNS: the binary object which s_binder1's   function
              field is set to.

     NOTE: This looks in the running lisp's symbol table for
           a symbol with the same name as s_entry_i.  It then
           creates a binary object whose entry field  points
           to    s_entry_i    and    whose    discipline   is
           st_discipline_i.  This binary object is stored  in
           the    function    field    of   s_binder_i.    If
           st_discipline_i is nil, then "subroutine" is  used
           by  default.  This is especially useful for _c_f_a_s_l
           users.

(macroexpand 'g_form)

     RETURNS: g_form after all macros in it are expanded.

     NOTE: This function will only  macroexpand  expressions
           which  could  be  evaluated  and it does not know
           about the special nlambdas such as _c_o_n_d  and  _d_o,
           thus it misses many macro expansions.






9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-36


(ptr 'g_arg)

     RETURNS: a value cell initialized to point to g_arg.

(quote g_arg)

     RETURNS: g_arg.

     NOTE: the reader allows you to abbreviate  (quote  foo)
           as 'foo.

(kwote 'g_arg)

     RETURNS:  (_l_i_s_t (_q_u_o_t_e _q_u_o_t_e) _g__a_r_g).

(replace 'g_arg1 'g_arg2)

     WHERE:   g_arg1 and g_arg2 must be  the  same  type  of
              lispval and not symbols or hunks.

     RETURNS: g_arg2.

     SIDE EFFECT: The effect of _r_e_p_l_a_c_e is dependent on  the
                  type  of  the  g_arg_i  although  one  will
                  notice a similarity in  the  effects.   To
                  understand what _r_e_p_l_a_c_e does to fixnum and
                  flonum arguments, you  must  first  under-
                  stand  that  such  numbers  are `boxed' in
                  FRANZ LISP.  What this means  is  that  if
                  the  symbol  x  has a value 32412, then in
                  memory the value  element  of  x's  symbol
                  structure  contains the address of another
                  word of memory (called a box)  with  32412
                  in it.

                  Thus, there are two ways of  changing  the
                  value  of  x:  the  first is to change the
                  value element of x's symbol  structure  to
                  point to a word of memory with a different
                  value.  The second way is  to  change  the
                  value  in  the box which x points to.  The
                  former method is used almost  all  of  the
                  time,  the  latter is used very rarely and
                  has the potential to  cause  great  confu-
                  sion.   The function _r_e_p_l_a_c_e allows you to
                  do the latter, i.e.,  to  actually  change
                  the value in the box.

                  You should watch out for these situations.
                  If  you  do  (_s_e_t_q _y _x), then both x and y
                  will point to the same box.   If  you  now
                  (_r_e_p_l_a_c_e _x _1_2_3_4_5),  then  y will also have
                  the value 12345.  And, in fact, there  may


                                   Printed: January 31, 1984







Data Structure Access                                   2-37


                  be many other pointers to that box.

                  Another problem with replacing fixnums  is
                  that  some  boxes are read-only.  The fix-
                  nums between -1024 and 1023 are stored  in
                  a  read-only  area and attempts to replace
                  them will result  in  an  "Illegal  memory
                  reference"  error  (see the description of
                  _c_o_p_y_i_n_t* for a way around this problem).

                  For the other valid types, the  effect  of
                  _r_e_p_l_a_c_e is easy to understand.  The fields
                  of g_val1's structure are made eq  to  the
                  corresponding  fields  of  g_val2's struc-
                  ture.  For example,  if  x   and   y  have
                  lists   as   values  then  the  effect  of
                  (_r_e_p_l_a_c_e _x _y)    is    the     same     as
                  (_r_p_l_a_c_a _x (_c_a_r _y)) and (_r_p_l_a_c_d _x (_c_d_r _y)).

(scons 'x_arg 'bs_rest)

     WHERE:   bs_rest is a bignum or nil.

     RETURNS: a bignum whose first bigit is x_arg and  whose
              higher order bigits are bs_rest.

(setf g_refexpr 'g_value)

     NOTE: _s_e_t_f is a generalization  of  setq.   Information
           may  be  stored  by  binding variables, replacing
           entries of arrays, and vectors, or being  put  on
           property  lists,  among  others.  Setf will allow
           the user to store data  into  some  location,  by
           mentioning  the  operation  used  to refer to the
           location.  Thus, the first argument may  be  par-
           tially  evaluated,  but only to the extent needed
           to calculate a reference.  _s_e_t_f returns  g_value.
           (Compare to _d_e_s_e_t_q )


    ____________________________________________________

      (setf x 3)        =  (setq x 3)
      (setf (car x) 3)  = (rplaca x 3)
      (setf (get foo 'bar) 3) = (putprop foo 3 'bar)
      (setf (vref vector index) value) = (vset vector index value)
    ____________________________________________________





9

9                                   Printed: January 31, 1984







Data Structure Access                                   2-38


(sort 'l_data 'u_comparefn)

     RETURNS: a list of the elements of  l_data  ordered  by
              the comparison function u_comparefn.

     SIDE EFFECT: the list l_data is  modified  rather  than
                  allocated in new storage.

     NOTE: (_c_o_m_p_a_r_e_f_n '_g__x  '_g__y)  should  return  something
           non-nil  if  g_x can precede g_y in sorted order;
           nil if g_y must precede g_x.  If  u_comparefn  is
           nil, alphabetical order will be used.

(sortcar 'l_list 'u_comparefn)

     RETURNS: a list of the  elements  of  l_list  with  the
              _c_a_r's    ordered    by   the   sort   function
              u_comparefn.

     SIDE EFFECT: the list l_list is  modified  rather  than
                  copied.

     NOTE: Like _s_o_r_t, if u_comparefn  is  nil,  alphabetical
           order will be used.




























9

9                                   Printed: January 31, 1984



