






                                CHAPTER  7


                                The Reader




     The FRANZ LISP reader is controlled by a _r_e_a_d_t_a_b_l_e.  A readtable is an
array of fixnums, one fixnum for each of the 128 ascii characters. The fix-
nums tell the reader what the properties of the each  character  are.   The
initial  readtable  is described below.  The user may create new readtables
using the _m_a_k_e_r_e_a_d_t_a_b_l_e function.  The current readtable is  the  value  of
the  lisp symbol _r_e_a_d_t_a_b_l_e which, like any lisp symbol, may be lambda bound
to allow the user to change the reader syntax  very  quickly.   The  values
which may appear in the readtable are:

8 _________________________________________________________________________
    type     value (decimal)           meaning                default
8 __________________________________________________________________________________________________________________________________________________
   vnum              0                  digit                   0-9
8 _________________________________________________________________________
   vsign             1              plus or minus               + -
8 _________________________________________________________________________
   vchar             2           alphabetic character
7                                                          A-Z a-z ^H !  $
                                                          % & * , / : ; <
                                                          = > ? @ ^ _ ` {
                                                          | } ~
8 _________________________________________________________________________
   vsca             66             single char atom            none
8 _________________________________________________________________________
   vlpara          195                left paren                 (
8 _________________________________________________________________________
   vrpara          196               right paren                 )
8 _________________________________________________________________________
   vperd           197                  period                   .
8 _________________________________________________________________________
   vlbrkt          198               left bracket                [
8 _________________________________________________________________________
   vrbrkt          199              right bracket                ]
8 _________________________________________________________________________
   veof            200               end of file              rubout
8 _________________________________________________________________________
   vsq             201               single quote                '
8 _________________________________________________________________________
   vdq             138                                           |
7                               double quote, all  char-
                               acters  between matching
                               double  quotes  are  es-
                               caped  (i.e.  treated as
                               vchar)
8 _________________________________________________________________________
   vsd             137                                           "
7                               string  delimiter,   all
                               characters       between
                               matching delimiters  are
                               _c_o_n_c_a_ted  into an object
                               of type string
8 _________________________________________________________________________





9The Reader                                                              7-1







The Reader                                                              7-2


   verr            203            illegal character
7                                                          null ^A-^I  ^N-
                                                          ^Z ^\-^_
8 _________________________________________________________________________
   vsep            204                separator           ^I-^M esc space
8 _________________________________________________________________________
   vspl            205         splicing macro character        none
8 _________________________________________________________________________
   vmac            206             macro character             none
8 _________________________________________________________________________
   vesc            143             escape character              \
8 _________________________________________________________________________
7
|
|
|
|
|
|
|
|
|
777777777799          |99|99|99|99|99|99|99|99|
777777777799                            |99|99|99|99|99|99|99|99|
777777777799                                                       |99|99|99|99|99|99|99|99|
777777777799                                                                         |99|99|99|99|99|99|99|99|
77777777
     The names in the type column are not known to Franz, we are just using
them  to  tag  the  various  classes.  You must use the value in the second
column.  The default column shows the syntax values of  characters  in  the
raw  lisp,  i.e.,  the lisp which contains only machine language functions.
The lisp which you get when you give the lisp command to the  shell  is  an
augmented version of the raw lisp, with additional lisp coded functions and
changes in the readtable.  The syntax changes in the lisp may  differ  from
installation to installation but will probably include making one character
be a comment character.  In the lisp at Berkeley, semicolon is the  comment
character.  This was done  by declaring it to be a splicing macro character
which invokes the function _z_a_p_l_i_n_e when seen.

     To read the syntax of a character, you may use (status syntax s_char).

     To change the syntax bits of a character, use the _s_e_t_s_y_n_t_a_x  function.
There  are two forms, one when you just want to change the syntax bits, and
the other when you want to define a character macro.  The first form is:

                        (setsyntax  _'_s___c  _'_x___v_a_l_u_e)

Here _s__c may be the character itself or it may be the fixnum representation
of  that character.  _x__v_a_l_u_e is one of the values given above in the second
column.  You should be careful when you change the syntax of a character as
the  change  lasts  until  you explicitly change it back or until you begin
with a new lisp.  Also, some syntax changes are  silly  and  will  probably
cause system errors (e.g. changing the syntax of an alphabetic character to
be a _v_n_u_m ).  The only syntax values you will probably ever  use  are:  _v_d_q
and _v_e_s_c.  You should not change the syntax to _v_s_p_l or _v_m_a_c using the above
form, instead it will be done automatically when you use the form below.

     To declare a character macro use:

                    (setsyntax  _'_s___c  _'_s___t_y_p_e  _'_s___f_c_n)

Where _s__c is again either the character itself or  its  fixnum  equivalent,
type  is  _s_p_l_i_c_i_n_g  or  _m_a_c_r_o,  and  _s__f_c_n is either the name of a function
expecting no arguments or is a lambda expression.  The result of  the  set-
syntax  function  is  twofold:  the  readtable  value for that character is
changed to _v_s_p_l or _v_m_a_c, and the function is put on the  property  list  of
the character under the indicator "macro".  The difference between a splic-
ing macro and a macro is this: the value returned by a  splicing  macro  is
_n_c_o_n_ced  to  what  has  been read so far (i.e. (nconc sofar justreturned)),


9                                                  Printed: October 22, 1980







The Reader                                                              7-3


while the value returned by a macro is added to what has  been  read,  (i.e
(nconc sofar (list  justread)).  Thus if a splicing macro returns nil, then
it isn't seen since (nconc any nil) ==> any.  In particular, splicing  mac-
ros are useful for conditional loading of lisp expressions.

     FRANZ LISP treats left and right square brackets in a special way when
building  lists.   A  left  bracket  is just like a left parenthesis, and a
right bracket matches a left bracket or all open left  parentheses,  which-
ever comes first.

     When building atoms, a character with the syntax code _v_e_s_c will  cause
the  next  character  to  be  read in and treated as a _v_c_h_a_r.  To escape an
entire string of characters, you surround  them  with  matching  characters
having  the _v_d_q syntax code.  To escape the _v_d_q character within the string
of characters you use any character  of  class  _v_e_s_c.   The  standard  UNIX
escape character, backslash (`\'), is in this class by default.




































9

9                                                  Printed: October 22, 1980



