






                         CHAPTER  9


                     Arrays and Vectors




     Arrays and vectors are two means of  expressing  aggre-
gate  data objects in FRANZ LISP.  Vectors may be thought of
as sequences of data.  They are intended as  a  vehicle  for
user-defined  data  types.   This  use  of  vectors is still
experimental and subject to  revision.   As  a  simple  data
structure,  they  are similar to hunks and strings.  Vectors
are used to implement closures, and are useful  to  communi-
cate with foreign functions.  Both of these topics were dis-
cussed in Chapter 8.  Later in this chapter, we describe the
current  implementation of vectors, and will advise the user
what is most likely to change.

     Arrays in FRANZ LISP provide a programmable data struc-
ture  access  mechanism.   One  possible  use for FRANZ LISP
arrays is to implement Maclisp style arrays which are simple
vectors of fixnums, flonums or general lisp values.  This is
described in more detail in 9.3 but first we  will  describe
how array references are handled by the lisp system.

     The structure of an array object is given in 1.3.10 and
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















   9.1.  general arrays   Suppose the evaluator is  told  to
      evaluate (_f_o_o _a _b) and the function cell of the symbol
      foo contains an  array  object  (which  we  will  call
      foo_arr_obj).   First  the evaluator will evaluate and
      stack the values of _a and _b.  Next it will  stack  the


9Arrays and Vectors                                       9-1







Arrays and Vectors                                       9-2


      array  object  foo_arr_obj.   Finally it will call the
      access function of foo_arr_obj.  The  access  function
      should  be  a  lexpr[] or a symbol whose function cell
      contains a lexpr.  The access function is  responsible
      for  locating  and  returning  a value from the array.
      The array access function is  free  to  interpret  the
      arguments  as it wishes.  The Maclisp compatible array
      access function which  is  provided  in  the  standard
      FRANZ  LISP  system  interprets  the arguments as sub-
      scripts in the same way as languages like Fortran  and
      Pascal.

           The array access function  will  also  be  called
      upon  to  store  elements  in the array.  For example,
      (_s_t_o_r_e (_f_o_o _a _b) _c) will automatically expand to  (foo
      c  a  b)  and when the evaluator is called to evaluate
      this, it will evaluate the arguments _c, _b and _a.  Then
      it will stack the array object (which is stored in the
      function cell of foo) and call the array access  func-
      tion  with  (now)  four  arguments.   The array access
      function must be able to tell this is a  store  opera-
      tion,  which it can do by checking the number of argu-
      ments it has been given (a  lexpr  can  do  this  very
      easily).



   9.2.  subparts of an array object   An array  is  created
      by allocating an array object with _m_a_r_r_a_y and  filling
      in the fields.  Certain lisp functions  interpret  the
      values  of the subparts of the array object in special
      ways as described  in  the  following  text.   Placing
      illegal  values  in  these subparts may cause the lisp
      system to fail.



      9.2.1.  access function   The purpose  of  the  access
         function has been described above.  The contents of
         the access function should be  a  lexpr,  either  a
         binary  (compiled  function) or a list (interpreted
         function).  It may also be a symbol whose  function
         cell  contains a function definition.  This subpart
         is used by _e_v_a_l, _f_u_n_c_a_l_l, and _a_p_p_l_y when evaluating
         array references.



____________________
9   []A lexpr is a function which accepts any number of argu-
ments which are evaluated before the function is called.



9                                      Printed: July 21, 1983







Arrays and Vectors                                       9-3


      9.2.2.  auxiliary   This can be used for any  purpose.
         If  it is a list and the first element of that list
         is the symbol unmarked_array then the data  subpart
         will  not  be marked by the garbage collector (this
         is used in the Maclisp compatible array package and
         has  the  potential  for  causing strange errors if
         used incorrectly).



      9.2.3.  data   This is either nil or points to a block
         of  data  space  allocated  by  _s_e_g_m_e_n_t  or  _s_m_a_l_l-
         _s_e_g_m_e_n_t.



      9.2.4.  length   This is a fixnum whose value  is  the
         number of elements in the data block.  This is used
         by the garbage collector and by _a_r_r_a_y_r_e_f to  deter-
         mine if your index is in bounds.



      9.2.5.  delta   This is a fixnum whose  value  is  the
         number  of bytes in each element of the data block.
         This will be four for an array of fixnums or  value
         cells,  and eight for an array of flonums.  This is
         used by the garbage collector and _a_r_r_a_y_r_e_f as well.



   9.3.  The Maclisp compatible array package

           A Maclisp style array is similar to what is known
      as  arrays  in other languages: a block of homogeneous
      data elements which is indexed by one or more integers
      called  subscripts.  The data elements can be all fix-
      nums, flonums or general lisp objects.   An  array  is
      created  by  a  call  to the function _a_r_r_a_y or *_a_r_r_a_y.
      The only difference is that *_a_r_r_a_y evaluates its argu-
      ments.   This call: (_a_r_r_a_y _f_o_o _t _3 _5) sets up an array
      called foo of dimensions 3 by 5.  The  subscripts  are
      zero  based.  The first element is (_f_o_o _0 _0), the next
      is (_f_o_o _0 _1) and so on up to (_f_o_o _2 _4).  The  t  indi-
      cates  a  general  lisp  object array which means each
      element of foo can be any type.  Each element  can  be
      any  type  since  all that is stored in the array is a
      pointer to a  lisp  object,  not  the  object  itself.
      _A_r_r_a_y  does  this  by  allocating an array object with
      _m_a_r_r_a_y and then allocating a segment of 15 consecutive
      value  cells  with _s_m_a_l_l-_s_e_g_m_e_n_t and storing a pointer
      to that segment in  the  data  subpart  of  the  array
      object.   The  length  and  delta subpart of the array


                                      Printed: July 21, 1983







Arrays and Vectors                                       9-4


      object are filled in (with 15 and 4 respectively)  and
      the  access  function  subpart  is set to point to the
      appropriate  array  access  function.   In  this  case
      there is a special access function for two dimensional
      value cell arrays called arrac-twoD, and  this  access
      function  is  used.   The  auxiliary subpart is set to
      (t 3 5) which describes the  type  of  array  and  the
      bounds of the subscripts. Finally this array object is
      placed in the function cell of the  symbol  foo.   Now
      when (_f_o_o _1 _3) is evaluated, the array access function
      is invoked with three arguments: 1, 3  and  the  array
      object.   From the auxiliary field of the array object
      it gets a description of  the  particular  array.   It
      then determines which element (_f_o_o _1 _3) refers to  and
      uses arrayref to extract that element.  Since this  is
      an  array  of  value cells, what arrayref returns is a
      value cell whose value is what we want, so we evaluate
      the   value  cell  and  return  it  as  the  value  of
      (_f_o_o _1 _3).

           In Maclisp the call (_a_r_r_a_y _f_o_o _f_i_x_n_u_m _2_5) returns
      an  array  whose  data  object is a block of 25 memory
      words.  When fixnums are stored  in  this  array,  the
      actual  numbers  are stored instead of pointers to the
      numbers as is done  in  general  lisp  object  arrays.
      This  is  efficient  under  Maclisp but inefficient in
      FRANZ LISP since every time  a  value  was  referenced
      from an array it had to be copied and a pointer to the
      copy returned to prevent aliasing[].  Thus  t,  fixnum
      and  flonum  arrays  are  all  implemented in the same
      manner.  This should not affect the  compatibility  of
      Maclisp  and  FRANZ  LISP.  If there is an application
      where a block of fixnums or flonums is required,  then
      the  exact  same effect of fixnum and flonum arrays in
      Maclisp can be  achieved  by  using  fixnum-block  and
      flonum-block  arrays.  Such arrays are required if you
      want to pass a large number of arguments to a  Fortran
      or C coded function and then get answers back.

           The Maclisp compatible array package is just  one
      example  of  how  a general array scheme can be imple-
      mented.  Another type of  array  you  could  implement
      would  be  hashed  arrays.   The  subscript  could  be
____________________
9   []Aliasing is when  two  variables  are  share  the  same
storage  location.   For  example  if  the copying mentioned
weren't done then after (_s_e_t_q _x (_f_o_o _2)) was done, the value
of x and (foo 2) would share the same location.  Then should
the value of (foo 2) change, x's value would change as well.
This  is  considered  dangerous and as a result pointers are
never returned into the data space of arrays.



9                                      Printed: July 21, 1983







Arrays and Vectors                                       9-5


      anything, not just  a  number.   The  access  function
      would  hash the subscript and use the result to select
      an array element.  With the generality of arrays  also
      comes  extra cost; if you just want a simple aggregate
      of (less than 128) general lisp objects you  would  be
      wise to look into using hunks.



   9.4.   vectors    Vectors  were  invented  to   fix   two
      shortcommings with hunks.  They can be longer than 128
      elements.  They also have a tag associated with  them,
      which is intended to say, for example, "Think of me as
      an _B_l_o_b_i_t."  Thus a vector is an arbitrary sized  hunk
      with a property list.

           Continuing the example, the lisp kernel  may  not
      know how to print out or evaluate _b_l_o_b_i_t_s, but this is
      information which will be common to all  _b_l_o_b_i_t_s.   On
      the  other hand, for each individual blobits there are
      particulars  which  are  likely  to  change,  (height,
      weight,  eye-color).  This is the part that would pre-
      viously have been stored in the individual entries  in
      the hunk, and are stored in the data slots of the vec-
      tor.  Once again we summarize the structure of a  vec-
      tor in tabular form:


8         ________________________________________________
          Subpart name   Get value   Set value    Type

8         ________________________________________________________________________________________________
            datum[_i]       vref        vset      lispval
8         ________________________________________________
            property       vprop     vsetprop    lispval
                                     vputprop
8         ________________________________________________
              size         vsize         -       fixnum
8         ________________________________________________
7        |7|7|7|7|7|7|7|






                      |7|7|7|7|7|7|7|






                                  |7|7|7|7|7|7|7|






                                              |7|7|7|7|7|7|7|






                                                        |7|7|7|7|7|7|7|








      Vectors are created specifying size and optional  fill
      value using the function (_n_e_w-_v_e_c_t_o_r  'x_size ['g_fill
      ['g_prop]]), or by  initial  values:  (_v_e_c_t_o_r  ['g_val
      ...]).



   9.5.  anatomy  of  vectors    There  are  some  technical
      details about vectors, that the user should know:








9                                      Printed: July 21, 1983







Arrays and Vectors                                       9-6


      9.5.1.  size   The user is not free to alter this.  It
         is noted when the vector is created, and is used by
         the garbage collector.  The garbage collector  will
         coallesce  two free vectors, which are neighbors in
         the heap.  Internally, this is kept as  the  number
         of  bytes of data.  Thus, a vector created by (_v_e_c_-
         _t_o_r 'foo), has a size of 4.



      9.5.2.  property   Currently, we expect  the  property
         to  be either a symbol, or a list whose first entry
         is a symbol.  The symbols fclosure  and  structure-
         value-argument  are  magic,  and  their  effect  is
         described in Chapter  8.   If  the  property  is  a
         (non-null)  symbol,  the vector will be printed out
         as <symbol>[<size>]. Another case is  if  the  pro-
         perty  is  actually  a (disembodied) property-list,
         which contains a value  for  the  indicator  print.
         The value is taken to be a Lisp function, which the
         printer will invoke with two arguments:  the vector
         and the current output port.  Otherwise, the vector
         will be printed as vector[<size>].  We  have  vague
         (as  yet unimplemented) ideas about similar mechan-
         isms for evaluation  properties.   Users  are  cau-
         tioned  against  putting anything other than nil in
         the property entry of a vector.



      9.5.3.  internal order   In memory, vectors start with
         a  longword containing the size (which is immediate
         data within the vector).  The next cell contains  a
         pointer  to  the property.  Any remaining cells (if
         any) are for data.  Vectors are handled differently
         from  any  other  object  in  FRANZ LISP, in that a
         pointer to a vector is pointer to  the  first  data
         cell,  i.e.  a pointer to the _t_h_i_r_d longword of the
         structure.  This was done for  efficiency  in  com-
         piled   code  and  for  uniformity  in  referencing
         immediate-vectors  (described  below).   The   user
         should  never return a pointer to any other part of
         a vector, as this may cause the  garbage  collector
         to follow an invalid pointer.



   9.6.  immediate-vectors   Immediate-vectors  are  similar
      to  vectors.   They  differ,  in  that binary data are
      stored in space directly within the vector.  Thus  the
      garbage  collector will preserve the vector itself (if
      used), and will only traverse the property cell.   The
      data  may  be  referenced as longwords, shortwords, or


                                      Printed: July 21, 1983







Arrays and Vectors                                       9-7


      even bytes.   Shorts  and  bytes  are  returned  sign-
      extended.   The  compiler  open-codes such references,
      and will avoid  boxing  the  resulting  integer  data,
      where  possible.   Thus, immediate vectors may be used
      for efficiently processing character data.   They  are
      also  useful in storing results from functions written
      in other languages.


8        __________________________________________________
         Subpart name   Get value    Set value     Type

8        ____________________________________________________________________________________________________
           datum[_i]     vrefi-byte   vseti-byte   fixnum
                        vrefi-word   vseti-word   fixnum
                        vrefi-long   vseti-long   fixnum
8        __________________________________________________
           property       vprop       vsetprop    lispval
                                      vputprop
8        __________________________________________________
             size         vsize          -        fixnum
                        vsize-byte                fixnum
                        vsize-word                fixnum
8        __________________________________________________
7       |7|7|7|7|7|7|7|7|7|7|7|










                     |7|7|7|7|7|7|7|7|7|7|7|










                                  |7|7|7|7|7|7|7|7|7|7|7|










                                               |7|7|7|7|7|7|7|7|7|7|7|










                                                         |7|7|7|7|7|7|7|7|7|7|7|












      To create immediate vectors specifying size  and  fill
      data,  you  can  use  the  functions _n_e_w-_v_e_c_t_o_r_i-_b_y_t_e,
      _n_e_w-_v_e_c_t_o_r_i-_w_o_r_d, or _n_e_w-_v_e_c_t_o_r_i-_l_o_n_g.  You  can  also
      use   the  functions  _v_e_c_t_o_r_i-_b_y_t_e,  _v_e_c_t_o_r_i-_w_o_r_d,  or
      _v_e_c_t_o_r_i-_l_o_n_g.  All of these functions are described in
      chapter 2.


























9                                      Printed: July 21, 1983



