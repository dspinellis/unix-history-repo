






                                CHAPTER  2


                           Data Structure Access




     The following functions allow one to create and manipulate the various
types of lisp data structures.  Refer to 1.3 for details of the data struc-
tures known to FRANZ LISP.





(*array 's_name 's_type 'x_dim1 ... x_dim_n)

     WHERE:   s_type may be one of t, nil, fixnum, flonum, fixnum-block and
              flonum-block.

     RETURNS: an array of type s_type with n dimensions of extents given by
              the x_dim_i.

     SIDE EFFECT: If s_name is non nil, the function definition  of  s_name
                  is set to the array structure returned.

     NOTE: The *_a_r_r_a_y function creates a Maclisp compatible array.   Arrays
           are  fully described in 9.  In FRANZ LISP arrays of type t, nil,
           fixnum and flonum are  equivalent  and  the  elements  of  these
           arrays  can  be  any  type  of  lisp  object.   Fixnum-block and
           flonum-block  arrays  are  restricted  to  fixnums  and  flonums
           respectively  and  are  used  mainly to communicate with foreign
           functions (see 8.4).


(aexplode 's_arg)

     RETURNS: a list of single character symbols which _p_r_i_n_t would  use  to
              print out g_arg, that is the list returned will contain quot-
              ing characters if _p_r_i_n_t would have used them to print s_arg.

     NOTE: this is restricted to symbols and is mainly for use by explode.









9

9Data Structure Access                                                   2-1







Data Structure Access                                                   2-2


(aexplodec 's_arg)

     RETURNS: a list of symbols whose pnames are the characters in  s_arg's
              pname.


(aexploden 's_arg)

     RETURNS: a list of fixnums which represent the characters  of  s_arg's
              pname.


    ___________________________________________________________________

    -> (_s_e_t_q _x '|_q_u_o_t_e _t_h_i_s _\| _o_k?|)
    |quote this \| ok?|
    -> (_a_e_x_p_l_o_d_e _x)
    (q u o t e |\\| | | t h i s |\\| | | |\\| |\|| |\\| | | o k ?)
    ; note that |\\| just means the single character: backslash.
    ; and |\|| just means the single character: vertical bar

    -> (_a_e_x_p_l_o_d_e_c _x)
    (q u o t e | | t h i s | | |\|| | | o k ?)
    -> (_a_e_x_p_l_o_d_e_n _x)
    (113 117 111 116 101 32 116 104 105 115 32 124 32 111 107 63)
    ___________________________________________________________________





(alphalessp 's_arg1 's_arg2)

     RETURNS: t iff the print name of s_arg1 is  alphabetically  less  than
              the print name of s_arg2.


(append 'l_arg1 'l_arg2)

     RETURNS: a list containing the elements of l_arg1 followed by l_arg2.

     NOTE: To generate the result, the top level list cells of  l_arg1  are
           duplicated  and the cdr of the last list cell is set to point to
           l_arg2.  Thus this is an expensive operation if l_arg1 is large.
           See  the  description  of  nconc  for a cheaper way of doing the
           append.






9

9                                                  Printed: October 21, 1980







Data Structure Access                                                   2-3


(append1 'l_arg1 'g_arg2)

     RETURNS: a list like l_arg1 with g_arg2 as the last element.

     NOTE: this is equivalent to (append 'l_arg1 (list 'g_arg2)).


    ___________________________________________________________________

    ; A common mistake is using append to add one element to the end of a list
    -> (_a_p_p_e_n_d '(_a _b _c _d) '_e)
    (a b c d . e)
    ; better is append1
    -> (_a_p_p_e_n_d_1 '(_a _b _c _d) '_e)
    (a b c d e)
    ->
    ___________________________________________________________________





(array s_name s_type x_dim1 ... x_dim_i)

     NOTE: this is  the  same  as  *array  except  the  arguments  are  not
           evaluated.


(arraycall s_type 'as_array 'x_ind1 ... )

     RETURNS: the element selected by  the indicies from the array  a_array
              of type s_type.

     NOTE: if as_array is a symbol then the function binding of this symbol
           should contain an array object.
           s_type is ignored by _a_r_r_a_y_c_a_l_l but is included for compatibility
           with Maclisp.


(arraydims 's_name)

     RETURNS: a list of the type and bounds of the array s_name.










9

9                                                  Printed: October 21, 1980







Data Structure Access                                                   2-4


(arrayp 'g_arg)

     RETURNS: t iff g_arg is of type array.


(arrayref 'a_name 'x_ind)

     RETURNS: the x_ind_t_h element of the array  object  a_name.   x_ind  of
              zero accesses the first element.

     NOTE: _a_r_r_a_y_r_e_f used the data, length and delta  fields  of  a_name  to
           determine which object to return.


    ___________________________________________________________________

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
    ___________________________________________________________________











9

9                                                  Printed: October 21, 1980







Data Structure Access                                                   2-5


(ascii x_charnum)

     WHERE:   x_charnum is between 0 and 255.

     RETURNS: a symbol whose print name is the single character whose  fix-
              num representation is x_charnum.


(assoc 'g_arg1 'l_arg2)

     RETURNS: the first top level element of l_arg2 whose _c_a_r is  _e_q_u_a_l  to
              g_arg1.

     NOTE: the test is make with the lisp function equal.   Usually  l_arg2
           has an _a-_l_i_s_t structure and g_arg1 acts as key.


(assq 'g_arg1 'l_arg2)

     RETURNS: the first top level element of l_arg2 whose _c_a_r is  equal  to
              g_arg1 using the lisp function _e_q.

     NOTE: This is faster than assoc since eq is faster than equal but lisp
           objects  which  print alike are not always _e_q.  See the descrip-
           tion of eq for more details.


    ___________________________________________________________________

    ; an `assoc list' (or alist) is a common lisp data structure.  It has the
    ; form ((key1 . value1) (key2 . value2) (key3 . value3) ... (keyn . valuen))
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
    ___________________________________________________________________


9

9                                                  Printed: October 21, 1980







Data Structure Access                                                   2-6


(atom 'g_arg)

     RETURNS: t iff g_arg is not a list or hunk object.

     NOTE: (atom '()) returns t.


(bcdad 's_funcname)

     RETURNS: a fixnum which is the address in memory  where  the  function
              s_funcname  begins.   If  s_funcname  is  not a machine coded
              function (binary) then bcdad returns nil.


(bcdp 'g_arg)

     RETURNS: t iff g_arg is a data object of type binary.

     NOTE: the name of this function is a throwback to the PDP-11 Lisp sys-
           tem.


(bigp 'g_arg)

     RETURNS: t iff g_arg is a bignum.


(c..r 'lh_arg)

     WHERE:   the .. represents any positive number of a's and d's.

     RETURNS: the result of accessing the list structure in the way  deter-
              mined  by  the  function name.  The a's and d's are read from
              right to left, a _d directing the access down the cdr part  of
              the list cell and an _a down the car part.

     NOTE: lh_arg may also be nil, and it is guaranteed that  the  car  and
           cdr of nil is nil.
           Currently one may dissect hunks and bignums with  c..r  as  well
           although this is subject to change.












9

9                                                  Printed: October 21, 1980







Data Structure Access                                                   2-7


(concat ['stn_arg1 ... ])

     RETURNS: a symbol whose print name is the result of concatenating  the
              print  names,  string characters or numerical representations
              of the sn_arg_i.

     NOTE: If no arguments are  given,  a  symbol  with  a  null  pname  is
           returned.   Concat  places the symbol created on the oblist, the
           function uconcat does the same thing but does not place the  new
           symbol on the oblist.

     EXAMPLE: (_c_o_n_c_a_t '_a_b_c (_a_d_d _3 _4) "_d_e_f") ==> abc7def


(cons 'g_arg1 'g_arg2)

     RETURNS: a new list cell whose car is g_arg1 and whose cdr is g_arg2.


(copy 'g_arg)

     RETURNS: A structure _e_q_u_a_l to g_arg but with new list cells.


(copysymbol 's_arg 'g_pred)

     RETURNS: an uninterned symbol with the same print name as  s_arg.   If
              g_pred  is non nil, then the value, function binding and pro-
              perty list of the new symbol are made _e_q to those of s_arg.


(cpy1 'xvt_arg)

     RETURNS: a new cell of the same type as xvt_arg with the same value as
              xvt_arg.


(cxr 'x_ind 'h_hunk)

     RETURNS: element x_ind (starting at 0) of hunk h_hunk.












9

9                                                  Printed: October 21, 1980







Data Structure Access                                                   2-8


(defprop ls_name g_val g_ind)

     RETURNS: g_val.

     SIDE EFFECT: The property list of ls_name is updated by  adding  g_val
                  as the value of indicator g_ind.

     NOTE: this is similar to putprop except that the arguments to  defprop
           are  not evaluated.  ls_name may be a disembodied property list,
           see _g_e_t.


(delete 'g_val 'l_list ['x_count])

     RETURNS: the result of splicing g_val from the top level of l_list  no
              more than x_count times.

     NOTE: x_count defaults to a very large number, thus if x_count is  not
           given, all occurances of g_val are removed from the top level of
           l_list.  g_val is compared with successive _c_a_r's of l_list using
           the function _e_q_u_a_l.

     SIDE EFFECT: l_list is modified using rplacd, no new  list  cells  are
                  used.


(delq 'g_val 'l_list ['x_count])

     RETURNS: the result of splicing g_val from the top level of l_list  no
              more than x_count times.

     NOTE: _d_e_l_q is the same as _d_e_l_e_t_e except that _e_q is used for comparison
           instead of _e_q_u_a_l.


    ___________________________________________________________________

    ; note that you should use the value returned by _d_e_l_e_t_e or _d_e_l_q
    ; and not assume that g_val will always show the deletions.
    ; For example

    -> (_s_e_t_q _t_e_s_t '(_a _b _c _a _d _e))
    (a b c a d e)
    -> (_d_e_l_e_t_e '_a _t_e_s_t)
    (b c d e)         ; the value returned is what we would expect
    -> _t_e_s_t
    (a b c d e)       ; but test still has the first a in the list!
    ___________________________________________________________________




9

9                                                  Printed: October 21, 1980







Data Structure Access                                                   2-9


(dtpr 'g_arg)

     RETURNS: t iff g_arg is a list cell.

     NOTE: that (dtpr '()) is nil.


(eq 'g_arg1 'g_arg2)

     RETURNS: t if g_arg1 and g_arg2 are the exact same lisp object.

     NOTE: _E_q simply tests if g_arg1 and g_arg2 are located  in  the  exact
           same place in memory.  Lisp objects which print the same are not
           necessarily _e_q.  The  only  objects  guaranteed  to  be  _e_q  are
           interned  symbols with the same print name.  [Unless a symbol is
           created in a special way (such as with  uconcat  or  maknam)  it
           will be interned.]


(equal 'g_arg1 'g_arg2)

     RETURNS: t iff g_arg1 and g_arg2 have the same structure as  described
              below.

     NOTE: g_arg and g_arg2 are _e_q_u_a_l if

     (1)  they are eq.

     (2)  they are both fixnums with the same value

     (3)  they are both flonums with the same value

     (4)  they are both bignums with the same value

     (5)  they are both strings and are identical.

     (6)  they are both lists and their cars and cdrs are _e_q_u_a_l.















9

9                                                  Printed: October 21, 1980







Data Structure Access                                                  2-10



    ___________________________________________________________________

    ; _e_q is much faster than _e_q_u_a_l, especially in compiled code,
    ; however you cannot use _e_q to test for equality of numbers outside
    ; of the range -1024 to 1023.  _e_q_u_a_l will always work.
    -> (_e_q _1_0_2_3 _1_0_2_3)
    t
    -> (_e_q _1_0_2_4 _1_0_2_4)
    nil
    -> (_e_q_u_a_l _1_0_2_4 _1_0_2_4)
    t
    ___________________________________________________________________





(explode 'g_arg)

     RETURNS: a list of single character symbols which _p_r_i_n_t would  use  to
              print g_arg.


(explodec 'g_val)

     RETURNS: the list of characters which print would use to  print  g_val
              except  that  special  characters  in symbols are not escaped
              (just as if patom were used to print them).


(exploden 'g_val)

     RETURNS: a list of fixnums which print would use to print g_val except
              that  special  characters in symbols are not escaped (just as
              if patom were used to print them).


(fillarray 's_array 'l_itms)

     RETURNS: s_array

     SIDE EFFECT: the array s_array is filled with  elements  from  l_itms.
                  If  there  are  not enough elements in l_itms to fill the
                  entire array, then the last element of l_itms is used  to
                  fill the remaining parts of the array.






9

9                                                  Printed: October 21, 1980







Data Structure Access                                                  2-11


(gensym 's_leader)

     RETURNS: a new uninterned atom beginning with the first  character  of
              s_leader's  pname,  or  beginning  with  g if s_leader is not
              given.

     NOTE: The symbol looks like x0nnnnn where x is s_leader's first  char-
           acter and nnnnn is the number of times you have called gensym.


(get 'ls_name 'g_ind)

     RETURNS: the value under indicator g_ind in ls_name's property list if
              ls_name is a symbol.

     NOTE: If there is no indicator g_ind in ls_name's property list nil is
           returned.   If  ls_name  is  a list of an odd number of elements
           then it is a disembodied property list. _g_e_t searches a  disembo-
           died  property list by  starting at its _c_d_r and looking at every
           other element for g_ind.


    ___________________________________________________________________

    -> (_p_u_t_p_r_o_p '_x_l_a_t_e '_a '_a_l_p_h_a)
    a
    -> (_p_u_t_p_r_o_p '_x_l_a_t_e '_b '_b_e_t_a)
    b
    -> (_p_l_i_s_t '_x_l_a_t_e)
    (alpha a beta b)
    -> (_g_e_t '_x_l_a_t_e '_a_l_p_h_a)
    a
    -> (_g_e_t '(_n_i_l _f_a_t_e_m_a_n _r_j_f _s_k_l_o_w_e_r _k_l_s _f_o_d_e_r_a_r_o _j_k_f) '_s_k_l_o_w_e_r)
    kls
    ___________________________________________________________________





(get_pname 's_arg)

     RETURNS: the string which is the print name of s_arg.









9

9                                                  Printed: October 21, 1980







Data Structure Access                                                  2-12


(getaccess 'a_array)

     RETURNS: the access function for the array a_array.

     NOTE: this function will most likely disappear in future releases.


(getaddress 's_entry1 's_binder1 'st_discipline1 [... ...])

     RETURNS: the binary object  which s_binder1's  function field  is  set
              to.

     NOTE: This looks in the running lisp's symbol table for a symbol  with
           the  same  name  as  s_entry_i.   It then creates a binary object
           whose entry field points to s_entry_i  and  whose  discipline  is
           st_discipline_i.   This  binary  object is stored in the function
           field of s_binder_i.  If st_discipline_i is nil, then "subroutine"
           is used by default.  This is especially useful for _c_f_a_s_l users.


(getaux 'a_array)

     RETURNS: the auxiliary field for the array a_array.

     NOTE: this function will most likely disappear in future releases.


(getchar 's_arg 'x_index)

     RETURNS: the x_index'th character of the print name of s_arg or nil if
              x_index  is less than 1 or greater than the length of s_arg's
              print name.


(getcharn 's_arg 'x_index)

     RETURNS: the fixnum representation of the x_index'th character of  the
              print  name  of  s_arg  or  nil  if x_index is less than 1 or
              greater than the length of s_arg's print name.


(getd 's_arg)

     RETURNS: the function definition of s_arg or nil if there is no  func-
              tion definition.

     NOTE: the function definition may turn out to be an array header.





9

9                                                  Printed: October 21, 1980







Data Structure Access                                                  2-13


(getdelta 'a_array)

     RETURNS: the delta field for a_array.

     NOTE: this function will most likely disappear in future releases.


(getentry 'y_funchd)

     RETURNS: the entry field of the binary y_funchd.

     NOTE: this function will most likely disappear in future releases.


(getlength 'a_array)

     RETURNS: the length field of the array a_array.

     NOTE: this function will most likely disappear in future releases.


(hunk 'g_val1 ['g_val2 ... 'g_val_n])

     RETURNS: a hunk of length n whose  elements  are  initialized  to  the
              g_val_i.

     NOTE: the maximum size of a hunk is 128.

     EXAMPLE: (_h_u_n_k _4 '_s_h_a_r_p '_k_e_y_s) ==> {4 sharp keys}


(hunksize 'h_arg)

     RETURNS: the size of the hunk h_arg.

     EXAMPLE: (_h_u_n_k_s_i_z_e (_h_u_n_k _1 _2 _3)) ==> 3


(implode 'l_arg)

     WHERE:   l_arg is a list of symbols and small fixnums.

     RETURNS: The symbol whose print name is the  result  of  concatenating
              the  print names of the symbols in the list.  Any fixnums are
              converted to the equivalent ascii character.







9

9                                                  Printed: October 21, 1980







Data Structure Access                                                  2-14


(intern 's_arg)

     RETURNS: s_arg

     SIDE EFFECT: s_arg is put on the oblist if it is not already there.


(last 'l_arg)

     RETURNS: the last list cell in the list l_arg.

     EXAMPLE: _l_a_s_t does NOT return the last element of a list!
              (_l_a_s_t '(_a _b)) ==>  (b)


(length 'l_arg)

     RETURNS: the number of elements in the top level of list l_arg.


(list ['g_arg1 ... ])

     RETURNS: a list whose elements are the g_arg_i.


(makhunk 'xl_arg)

     RETURNS: a hunk of length xl_arg initialized to all nils if xl_arg  is
              a fixnum.  If xl_arg is a list, then we return a hunk of size
              (_l_e_n_g_t_h '_x_l__a_r_g) initialized to the elements in xl_arg.

     NOTE: (_m_a_k_h_u_n_k '(_a _b _c)) is equivalent to (_h_u_n_k '_a '_b '_c).

     EXAMPLE: (_m_a_k_h_u_n_k _4) ==> {nil nil nil nil}


(*makhunk 'x_arg)

     RETURNS: a hunk of size 2[x_arg] initialized to EMPTY.

     NOTE: This is only to be used by such functions as  _h_u_n_k  and  _m_a_k_h_u_n_k
           which create and initialize hunks for users.










9

9                                                  Printed: October 21, 1980







Data Structure Access                                                  2-15


(maknam 'l_arg)

     RETURNS: what implode would return  except  the  resulting  symbol  is
              uninterned.


(makunbound 's_arg)

     RETURNS: s_arg

     SIDE EFFECT: the value of s_arg is made `unbound'.  If the interpreter
                  attempts  to  evaluate  s_arg  before it is again given a
                  value, an unbound variable error will occur.


(marray 'g_data 's_access 'g_aux 'x_length 'x_delta)

     RETURNS: an array type with the fields set up from the above arguments
              in the obvious way (see  1.3.9).


(member 'g_arg1 'l_arg2)

     RETURNS: that part of the l_arg2 beginning with the  first  occurrence
              of  g_arg1.  If g_arg1 is not in the top level of l_arg2, nil
              is returned.

     NOTE: the test for equality is made with _e_q_u_a_l.


(memq 'g_arg1 'l_arg2)

     RETURNS: that part of the l_arg2 beginning with the first occurance of
              g_arg1.   If g_arg1 is not in the top level of l_arg2, nil is
              returned.

     NOTE: the test for equality is made with _e_q.


(nconc 'l_arg1 'l_arg2 ['l_arg3 ...])

     RETURNS: A list consisting of the elements of l_arg1 followed  by  the
              elements of l_arg2 followed by l_arg3 and so on.

     NOTE: The _c_d_r of the last list cell of l_arg_i is changed to  point  to
           l_arg_i+_1.






9

9                                                  Printed: October 21, 1980







Data Structure Access                                                  2-16



    ___________________________________________________________________

    ; _n_c_o_n_c is faster than _a_p_p_e_n_d because it doesn't allocate new list
    ; cells.
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
    ___________________________________________________________________





(ncons 'g_arg)

     RETURNS: a new list cell with g_arg as car and nil as cdr.


(not 'g_arg)

     RETURNS: t iff g_arg is nil.


(nreverse 'l_arg)

     RETURNS: the reverse of l_arg.

     NOTE: The reverse is done in place, that  is  the  list  structure  is
           modified.  No new list cells are allocated.


(nthelem 'n_arg1 'l_arg2)

     RETURNS: The n_arg1'_s_t element of the list l_arg2.

     NOTE: If n_arg1 is non-positive or greater  than  the  length  of  the
           list, nil is returned.





9

9                                                  Printed: October 21, 1980







Data Structure Access                                                  2-17


(null 'g_arg)

     RETURNS: t iff g_arg is nil.

     EQUIVALENT TO: not.


(plist 's_name)

     RETURNS: the property list of s_name.


(ptr 'g_arg)

     RETURNS: a value cell initialize to point to g_arg.


(putaccess 'a_array 's_func)

     RETURNS: s_func.

     SIDE EFFECT: replaces the access field of a_array with s_func.

     NOTE: this function will most likely disappear in future releases.


(putaux 'a_array 'g_aux)

     RETURNS: s_aux.

     SIDE EFFECT: replaces the auxillary field of a_array with g_aux.

     NOTE: this function will most likely disappear in future releases.


(putdelta 'a_array 'x_delta)

     RETURNS: x_delta.

     SIDE EFFECT: replaces the delta field of a_array with x_delta.

     NOTE: this function will most likely disappear in future releases.










9

9                                                  Printed: October 21, 1980







Data Structure Access                                                  2-18


(putdisc 'y_func 's_discipline)

     RETURNS: s_discipline

     SIDE EFFECT: the discipline field of y_func is set to s_discipline.


(putlength 'a_array 'x_length)

     RETURNS: x_length

     SIDE EFFECT: replaces the length field of a_array with x_length.

     NOTE: this function will most likely disappear in future releases.


(putprop 'ls_name 'g_val 'g_ind)

     RETURNS: g_val.

     SIDE EFFECT: Adds to the property list  of  ls_name  the  value  g_val
                  under the indicator g_ind.

     NOTE: this is similar to _d_e_f_p_r_o_p except the arguments are evaluated to
           _p_u_t_p_r_o_p.  ls_name may be a disembodied property list, see _g_e_t.


(quote g_arg)

     RETURNS: g_arg.

     NOTE: the reader allows you to abbreviate (quote foo) as 'foo.


(rematom 's_arg)

     RETURNS: t if s_arg is indeed an atom.

     SIDE EFFECT: s_arg is put on the free atoms list, effectively reclaim-
                  ing an atom cell.

     NOTE: This function does _n_o_t check to see if s_arg is on the oblist or
           is  referenced anywhere.  Thus calling _r_e_m_a_t_o_m on an atom in the
           oblist may result in disaster when that atom cell is reused!








9

9                                                  Printed: October 21, 1980







Data Structure Access                                                  2-19


(remob 's_symbol)

     RETURNS: s_symbol

     SIDE EFFECT: s_symbol is removed from the oblist.


(remprop 'ls_name 'g_ind)

     RETURNS: the portion of  ls_name's property list  beginning  with  the
              property  under  the  indicator  g_ind.  If there is no g_ind
              indicator in ls_name's plist, nil is returned.

     SIDE EFFECT: the value under  indicator  g_ind  and  g_ind  itself  is
                  removed from the property list of ls_name.

     NOTE: ls_name may be a disembodied property list, see _g_e_t.


(replace 'g_arg1 'g_arg2)

     WHERE:   g_arg1 and g_arg2 must be the same type of  lispval  and  not
              symbols or hunks.

     RETURNS: g_arg2.

     SIDE EFFECT: The effect of _r_e_p_l_a_c_e is dependent on  the  type  of  the
                  g_arg_i  although  one  will  notice  a  similarity in the
                  effects.  To understand what _r_e_p_l_a_c_e does to  fixnum  and
                  flonum  arguments  you  must  first  understand that such
                  numbers are `boxed' in FRANZ LISP.  What  this  means  is
                  that  if  the  symbol x has a value 32412, then in memory
                  the value element of x's symbol  structure  contains  the
                  address  of  another  word  of memory (called a box) with
                  32412 in it.  Thus there are two  ways  of  changing  the
                  value  of  x,  one  is to change the value element of x's
                  symbol structure to point to a word of memory with a dif-
                  ferent  value.  Another way is to change the value in the
                  box which x points to.  The former method is used  almost
                  all  of  the time, the latter is used very rarely and has
                  the potential to cause  great  confusion.   The  function
                  _r_e_p_l_a_c_e  allows you to do the latter, that is to actually
                  change the value in the box.  You should  watch  out  for
                  these  situations.  If you do (_s_e_t_q _y _x) then both  x and
                  y  will  point   to   the   same   box.    If   you   now
                  (_r_e_p_l_a_c_e _x _1_2_3_4_5)  then y will also have the value 12345.
                  And in fact there may be many other pointers to that box.
                  Another problem with replacing fixnums is that some boxes
                  are read only.  The fixnums between -1024  and  1023  are
                  stored  in  a read only area and attempts to replace them
                  will result in an "Illegal memory reference"  error  (see
                  the  description  of _c_o_p_y_i_n_t* for a way around this prob-
                  lem)..  For the other valid types, the effect of  _r_e_p_l_a_c_e


                                                  Printed: October 21, 1980







Data Structure Access                                                  2-20


                  is  easy to understand.  The fields of g_val1's structure
                  are made eq  to  the  corresponding  fields  of  g_val2's
                  structure.   For  example,  if  x   and   y have lists as
                  values then the effect of (_r_e_p_l_a_c_e _x _y) is  the  same  as
                  (_r_p_l_a_c_a _x (_c_a_r _y)) and (_r_p_l_a_c_d _x (_c_d_r _y)).


(reverse 'l_arg)

     RETURNS: the reverse of the list l_arg.

     NOTE: The _r_e_v_e_r_s_e is performed by allocating new list cells to  dupli-
           cate  the  top level of l_arg. This can be expensive if l_arg is
           large.  The function nreverse  will  reverse  the  list  without
           allocating new list cells.


(rplaca 'lh_arg1 'g_arg2)

     RETURNS: the modified lh_arg1.

     SIDE EFFECT: the car of lh_arg1 is set to  g_arg2.  If  lh_arg1  is  a
                  hunk  then  the  second  element  of  the  hunk is set to
                  g_arg2.


(rplacd 'lh_arg1 'g_arg2)

     RETURNS: the modified lh_arg1.

     SIDE EFFECT: the cdr of lh_arg2 is set to g_arg2.   If  lh_arg1  is  a
                  hunk then the first element of the hunk is set to g_arg2.


(rplacx 'x_ind 'h_hunk 'g_val)

     RETURNS: h_hunk

     SIDE EFFECT: Element x_ind (starting at 0) of h_hunk is set to g_val.


(*rplacx 'x_ind 'h_hunk 'g_val)

     RETURNS: h_hunk

     SIDE EFFECT: Element x_ind (starting at 0) of h_hunk is set to g_val.

     NOTE: This is the same as _r_p_l_a_c_x except you may replace  uninitialized
           hunk  entries. This is only to be used by functions such as _h_u_n_k
           and _m_a_k_h_u_n_k which create hunks of sizes which are not powers  of
           two.

9

9                                                  Printed: October 21, 1980







Data Structure Access                                                  2-21


(sassoc 'g_arg1 'l_arg2 'sl_func)

     RETURNS: the                         result                         of
              (_c_o_n_d ((_a_s_s_o_c '_g__a_r_g '_l__a_r_g_2) (_a_p_p_l_y '_s_l__f_u_n_c _n_i_l)))

     NOTE: sassoc is written as a macro.


(sassq 'g_arg1 'l_arg2 'sl_func)

     RETURNS: the                         result                         of
              (_c_o_n_d ((_a_s_s_q '_g__a_r_g '_l__a_r_g_2) (_a_p_p_l_y '_s_l__f_u_n_c _n_i_l)))

     NOTE: sassq is written as a macro.


(set 's_arg1 'g_arg2)

     RETURNS: g_arg2.

     SIDE EFFECT: the value of s_arg1 is set to g_arg2.


(setplist 's_atm 'l_plist)

     RETURNS: l_plist.

     SIDE EFFECT: the property list of s_atm is set to l_plist.


(setq s_atm1 'g_val1 [ s_atm2 'g_val2 ... ... ])

     WHERE:   the arguments are pairs of atom names and expressions.

     RETURNS: the last g_val_i.

     SIDE EFFECT: each s_atm_i is set to have the value g_val_i.


(stringp 'g_arg)

     RETURNS: t iff g_arg is a string.










9

9                                                  Printed: October 21, 1980







Data Structure Access                                                  2-22


(symbolp 'g_arg)

     RETURNS: t iff g_arg is a symbol.


(type 'g_arg)

     RETURNS: a symbol whose pname describes the type of g_arg.


(typep 'g_arg)

     EQUIVALENT TO: type.


(uconcat ['s_arg1 ... ])

     RETURNS: a symbol whose pname is the result of concatenating the print
              names (pnames) of the s_arg_i.

     NOTE: If no arguments are  given,  a  symbol  with  a  null  pname  is
           returned.   _u_c_o_n_c_a_t  does  not  place  the symbol created on the
           oblist, the function concat does the same thing but  does  place
           the new symbol on the oblist.


(valuep 'g_arg)

     RETURNS: t iff g_arg is a value cell























9

9                                                  Printed: October 21, 1980



