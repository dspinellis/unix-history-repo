






                        CCCCHHHHAAAAPPPPTTTTEEEERRRR  11116666


                      TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr








11116666....1111....  TTTThhhheeee EEEEddddiiiittttoooorrrrssss

It is quite possible to use VI, Emacs or other standard edi-
tors  to  edit  your  lisp programs, and many people do just
that.  However there is a lisp  structure  editor  which  is
particularly  good  for  the  editing  of lisp programs, and
operates in a rather different fashion, namely within a lisp
environment.   application.   It is handy to know how to use
it for fixing problems without exiting from the lisp  system
(e.g.  from  the  debugger  so  you  can continue to execute
rather than having to start over.)  The editor is not  quite
like  the  top-level and debugger, in that it expects you to
type editor commands to it.  It will not  evaluate  whatever
you happen to type.  (There is an editor command to evaluate
things, though.)

The editor is available (assuming  your  system  is  set  up
correctly with a lisp library) by typing (load 'cmufncs) and
(load 'cmuedit).

The  most  frequent  use of the editor is to change function
definitions  by starting the editor with one of the commands
described in section 16.14.  (see  _e_d_i_t_f),  values  (_e_d_i_t_v),
properties (_e_d_i_t_p), and  expressions  (_e_d_i_t_e).  The beginner
is advised to start with the  following  (very  basic)  com-
mands:  _o_k,  _u_n_d_o,  _p, #, under which are explained two dif-
ferent basic commands which  start with numbers, and f.

This documentation, and the editor, were imported from  PDP-
10  CMULisp  by Don Cohen.  PDP-10 CMULisp is based on UCIL-
isp, and the editor itself was derived from an early version
of Interlisp.  Lars Ericson, the author of this section, has
provided this very concise summary.  Tutorial  examples  and
implementation  details may be found in the Interlisp Refer-
ence Manual, where a similar editor is described.





9

9TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                         11116666----1111







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                         11116666----2222


11116666....2222....  SSSSccccooooppppeeee ooooffff AAAAtttttttteeeennnnttttiiiioooonnnn

Attention-changing commands allow you to look at a different
part  of  a  Lisp  expression  you  are  editing.   The sub-
structure upon which the editor's attention is  centered  is
called  "the  current  expression".    Changing  the current
expression means shifting attention and not actually modify-
ing any structure.

____________________________________________________________

_S_C_O_P_E _O_F _A_T_T_E_N_T_I_O_N _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

_n (_n>_0) . Makes the nth element of the current expression be
the new current expression.

-_n (_n>_0). Makes the nth element from the end of the  current
expression be the new current expression.

_0. Makes the  next  higher expression  be  the  new  correct
expression.   If  the  intention  is  to go back to the next
higher left parenthesis, use the command !0.

_u_p .  If a p command would cause  the  editor  to  type  ...
before  typing  the current expression, (the current expres-
sion is a tail of the next higher expression)  then  has  no
effect;  else, up makes the old current expression the first
element in the new current expression.

!_0 . Goes back to the next higher left parenthesis.

^ .  Makes the top level expression be the  current  expres-
sion.

_n_x .  Makes the current expression be the next expression.

(_n_x _n) equivalent to n nx commands.

!_n_x .  Makes current expression be the next expression at  a
higher  level.  Goes through any number of right parentheses
to get to the next expression.

 _b_k .  Makes the current expression be the previous  expres-
sion in the next higher expression.

(_n_t_h _n) _n>_0 .  Makes the list starting with the nth  element
of the current expression be the current expression.

(_n_t_h $) - _g_e_n_e_r_a_l_i_z_e_d _n_t_h _c_o_m_m_a_n_d. nth locates $,  and  then
backs up to the current level, where the new current expres-
sion is the tail whose first element contains, however  dee-
ply,  the  expression  that was the terminus of the location
operation.


                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                         11116666----3333


:: .  (pattern ::  .  $)  e.g., (cond :: return).   finds  a
cond that contains a return, at any depth.

(_b_e_l_o_w _c_o_m _x) .  The below command is useful for locating  a
substructure  by  specifying something  it contains.  (below
cond) will cause the cond  clause  containing   the  current
expression  to  become  the new current expression.  Suppose
you are editing a list of lists, and want to find a  sublist
that  contains a foo (at any depth).  Then simply executes f
foo (below ).

(_n_e_x _x) .  same as (_b_e_l_o_w _x) followed by nx.   For  example,
if  you are deep inside of a selectq clause, you can advance
to the next clause with (_n_e_x _s_e_l_e_c_t_q).

_n_e_x.  The  atomic  form  of  _n_e_x is useful if  you  will  be
performing  repeated   executions   of  (_n_e_x  _x).  By simply
marking  the  chain corresponding to x,  you can use _n_e_x  to
step through the sublists.
____________________________________________________________






11116666....3333....  PPPPaaaatttttttteeeerrrrnnnn MMMMaaaattttcccchhhhiiiinnnngggg CCCCoooommmmmmmmaaaannnnddddssss

Many editor commands that search take patterns.   A  pattern
_p_a_t matches with x if:

____________________________________________________________

_P_A_T_T_E_R_N _S_P_E_C_I_F_I_C_A_T_I_O_N _S_U_M_M_A_R_Y

- _p_a_t is _e_q to x.

- _p_a_t is &.

- _p_a_t is a number and equal to x.

- if (car _p_a_t) is the atom *any*, (cdr _p_a_t)  is  a  list  of
patterns,  and  _p_a_t matches x if and only if one of the pat-
terns on (cdr _p_a_t) matches x.

- if _p_a_t is a literal atom or string, and (nthchar  _p_a_t  -1)
is @, then _p_a_t matches with any literal atom or string which
has the same initial characters as _p_a_t, e.g.   ver@  matches
with verylongatom, as well as "verylongstring".

- if (car _p_a_t) is the atom --, _p_a_t matches  x  if  (a)  (cdr
_p_a_t)=nil,  i.e.   _p_a_t=(--), e.g., (a --) matches (a) (a b c)
and (a .  b) in other words, -- can  match  any  tail  of  a


                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                         11116666----4444


list.   (b)  (cdr _p_a_t) matches with some tail of x, e.g.  (a
-- (&)) will match with (a b c (d)), but not (a b c  d),  or
(a  b c (d) e).  however, note that (a -- (&) --) will match
with (a b c (d) e).  in other words, -- will match any inte-
rior segment of a list.

- if (car _p_a_t) is the atom ==, _p_a_t matches x if and only  if
(cdr  _p_a_t) is _e_q to x.  (this pattern is for use by programs
that call the editor as a subroutine, since  any  non-atomic
expression  in a command typed in by the user obviously can-
not be _e_q to existing structure.) -  otherwise  if  x  is  a
list,  _p_a_t  matches x if (car _p_a_t) matches (car x), and (cdr
_p_a_t) matches (cdr x).

- when searching, the pattern  matching  routine  is  called
only  to  match  with  elements in the structure, unless the
pattern begins with :::, in which case cdr of the pattern is
matched  against tails in the structure.  (in this case, the
tail does not have to be a proper tail, e.g.   (:::   a  --)
will  match  with the element (a b c) as well as with cdr of
(x a b c), since (a b c) is a tail of (a b c).)
____________________________________________________________






11116666....3333....1111....  CCCCoooommmmmmmmaaaannnnddddssss TTTThhhhaaaatttt SSSSeeeeaaaarrrrcccchhhh

____________________________________________________________

_S_E_A_R_C_H _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

_f _p_a_t_t_e_r_n .  f informs the editor that the next  command  is
to  be  interpreted as a pattern.  If no pattern is given on
the same line as the f then the last  pattern  is  used.   f
pattern means find the next instance of pattern.

(_f _p_a_t_t_e_r_n _n).  Finds the next instance of pattern.

(_f _p_a_t_t_e_r_n _t).  similar to f pattern, except,  for  example,
if the current expression is (cond ..), f cond will look for
the next cond, but (f cond t) will 'stay here'.

(_f _p_a_t_t_e_r_n  _n)  _n>_0.   Finds  the  nth  place  that  pattern
matches.   If the current expression is (foo1 foo2 foo3), (f
f00@ 3) will find foo3.

(_f _p_a_t_t_e_r_n) _o_r (_f _p_a_t_t_e_r_n _n_i_l).  only matches with  elements
at  the top level of the current expression.  If the current
expression is (_p_r_o_g _n_i_l (_s_e_t_q _x (_c_o_n_d & &)) (_c_o_n_d &) ...)  f
(cond  --)  will  find  the cond inside the setq, whereas (f


                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                         11116666----5555


(cond --)) will find the top level cond,  i.e.,  the  second
one.

(_s_e_c_o_n_d . $) .  same as (lc .  $) followed by another (lc  .
$)  except  that  if the first succeeds and second fails, no
change is made to the edit chain.

(_t_h_i_r_d . $) .  Similar to second.

(_f_s _p_a_t_t_e_r_n_1 ... _p_a_t_t_e_r_n_n) .  equivalent to f pattern1  fol-
lowed by f pattern2 ...  followed by f pattern n, so that if
f pattern m fails, edit chain is left at place  pattern  m-1
matched.

(_f= _e_x_p_r_e_s_s_i_o_n _x) .  Searches for a structure eq to  expres-
sion.

(_o_r_f _p_a_t_t_e_r_n_1 ... _p_a_t_t_e_r_n_n) .  Searches  for  an  expression
that is matched by either pattern1 or ...  patternn.

_b_f _p_a_t_t_e_r_n .  backwards find.   If the current expression is
(_p_r_o_g _n_i_l (_s_e_t_q _x (_s_e_t_q _y (_l_i_s_t _z))) (_c_o_n_d ((_s_e_t_q _w --) --))
--) f list followed  by  bf  setq  will  leave  the  current
expression  as (setq y (list z)), as will f cond followed by
bf setq

(_b_f _p_a_t_t_e_r_n _t).  backwards  find.   Search  always  includes
current  expression,  i.e., starts at end of current expres-
sion and works backward, then ascends and backs up, etc.
____________________________________________________________






11116666....3333....1111....1111....  LLLLooooccccaaaattttiiiioooonnnn SSSSppppeeeecccciiiiffffiiiiccccaaaattttiiiioooonnnnssss    Many  editor  commands
use  a  method  of  specifying  position  called  a location
specification.  The meta-symbol $ is used to denote a  loca-
tion specification.   $ is a list of commands interpreted as
described above.  $ can also be atomic, in which case it  is
interpreted as (list $).  a location specification is a list
of edit commands that are executed  in  the  normal  fashion
with  two exceptions.  first, all commands not recognized by
the editor are interpreted as though they had been  preceded
by  f.  The  location specification (cond 2 3) specifies the
3rd element in the first clause of the next cond.

the if command and the ## function provide a way of using in
location specifications arbitrary predicates applied to ele-
ments in the current expression.

In insert, delete, replace and change, if $ is nil  (empty),


                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                         11116666----6666


the corresponding operation is performed on the current edit
chain, i.e.  (replace with (car x)) is equivalent to  (:(car
x)).   for  added readability, here is also permitted, e.g.,
(insert (print x) before here) will insert (print x)  before
the  current expression (but not change the edit chain).  It
is perfectly legal to ascend to insert, replace, or  delete.
for example (insert (_r_e_t_u_r_n) after ^ prog -1) will go to the
top, find the first prog, and insert a (_r_e_t_u_r_n) at its  end,
and not change the current edit chain.

The a, b, and :  commands all make special checks in e1 thru
em  for  expressions of the form (## . coms).  In this case,
the expression used for inserting or replacing is a copy  of
the  current expression after executing coms, a list of edit
commands.   (insert (## f cond -1 -1) after3)  will  make  a
copy  of  the last form in the last clause of the next cond,
and insert it after the third element of the current expres-
sion.

$.  In descriptions of the editor, the meta-symbol $ is used
to  denote  a  location specification.   $ is a list of com-
mands interpreted as described above.  $ can also be atomic.

____________________________________________________________

_L_O_C_A_T_I_O_N _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

(_l_c . $) .  Provides a way of explicitly invoking the  loca-
tion operation.  (lc cond 2 3) will perform search.

(_l_c_l . $) .  Same as lc except search is confined to current
expression.   To  find a cond containing a _r_e_t_u_r_n, one might
use the location specification (cond (lcl  _r_e_t_u_r_n)  )  where
the   would reverse the effects of the lcl command, and make
the final current expression be the cond.
____________________________________________________________






11116666....3333....2222....  TTTThhhheeee EEEEddddiiiitttt CCCChhhhaaaaiiiinnnn   The edit-chain is a list of  which
the  first  element  is  the  the  one  you  are now editing
("current expression"),  the  next  element  is  what  would
become  the  current expression if you were to do a 0, etc.,
until the last element which  is  the  expression  that  was
passed to the editor.




9

9                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                         11116666----7777


____________________________________________________________

_E_D_I_T _C_H_A_I_N _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

_m_a_r_k .  Adds the current edit chain to the front of the list
marklst.

_ .  Makes the new edit chain be (car marklst).

(_ _p_a_t_t_e_r_n) .  Ascends the edit chain  looking  for  a  link
which matches pattern.  for example:

__ .  Similar to _ but also erases the mark.

\ . Makes the edit chain be the value of unfind.  unfind  is
set  to  the current edit chain by each command that makes a
"big jump", i.e., a command that usually performs more  than
a  single  ascent or descent, namely ^, _, __, !nx, all com-
mands that involve a search, e.g., f, lc, ::, below,  et  al
and                      and                     themselves.
if the user types f cond, and then f car,   would  take  him
back  to the cond.  another  would take him back to the car,
etc.

\_p .  Restores the edit chain to its state as  of  the  last
print  operation.   If  the edit chain has not changed since
the last printing, \p restores it to its  state  as  of  the
printing before that one.  If the user types p followed by 3
2 1 p, \p will  return  to  the  first  p,  i.e.,  would  be
equivalent to 0 0 0.  Another \p would then take him back to
the second p.
____________________________________________________________






11116666....4444....  PPPPrrrriiiinnnnttttiiiinnnngggg CCCCoooommmmmmmmaaaannnnddddssss

____________________________________________________________

_P_R_I_N_T_I_N_G _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

_p  Prints current expression in  abbreviated  form.   (p  m)
prints  mth  element  of  current  expression in abbreviated
form.  (p m n) prints mth element of current  expression  as
though  printlev  were  given  a depth of n.  (p 0 n) prints
current expression as though printlev were given a depth  of
n.  (p cond 3) will work.

?  .  prints the current expression as though printlev  were
given a depth of 100.


                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                         11116666----8888


_p_p .  pretty-prints the current expression.

_p_p*.  is like pp, but forces comments to be shown.
____________________________________________________________






11116666....5555....  SSSSttttrrrruuuuccccttttuuuurrrreeee MMMMooooddddiiiiffffiiiiccccaaaattttiiiioooonnnn CCCCoooommmmmmmmaaaannnnddddssss

All structure modification commands are undoable.  See _u_n_d_o.


____________________________________________________________

_S_T_R_U_C_T_U_R_E _M_O_D_I_F_I_C_A_T_I_O_N _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

# [_e_d_i_t_o_r _c_o_m_m_a_n_d_s] (n)  n>1 deletes the corresponding  ele-
ment from the current expression.

(_n _e_1 ...  _e_m) _n,_m>_1 replaces the nth element in the current
expression with e1 ...  em.

(-_n _e_1 ...  _e_m) _n,_m>_1 inserts e1 ...  em before the  n  ele-
ment in the current expression.

(_n _e_1 ...  _e_m) (the letter "n" for "next" or "nconc", not  a
number)  m>1  attaches  e1 ...  em at the end of the current
expression.

(_a _e_1 ... _e_m) .   inserts  e1  ...   em  after  the  current
expression (or after its first element if it is a tail).

(_b _e_1 ... _e_m) .  inserts  e1  ...   em  before  the  current
expression.   to  insert  foo before the last element in the
current expression, perform -1 and then (b foo).

(: _e_1 ... _e_m) .  replaces the current expression by  e1  ...
em.     If the current expression is a tail then replace its
first element.

_d_e_l_e_t_e _o_r (:)  .  deletes the current expression, or if  the
current expression is a tail, deletes its first element.

(_d_e_l_e_t_e . $).  does a (lc .  $) followed by delete.  current
edit chain is not changed.

(_i_n_s_e_r_t _e_1 ... _e_m _b_e_f_o_r_e . $) .  similar to  (lc.   $)  fol-
lowed by (b e1 ... em).

(_i_n_s_e_r_t _e_1 ...  _e_m _a_f_t_e_r .  $).  similar  to  insert  before


                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                         11116666----9999


except uses a instead of b.

(_i_n_s_e_r_t _e_1 ...  _e_m _f_o_r .   $).   similar  to  insert  before
except uses :  for b.

(_r_e_p_l_a_c_e $ _w_i_t_h _e_1 ... _e_m) .  here $ is the segment  of  the
command between replace and with.

(_c_h_a_n_g_e $ _t_o _e_1 ... _e_m) .  same as replace with.
____________________________________________________________






11116666....6666....  EEEExxxxttttrrrraaaaccccttttiiiioooonnnn aaaannnndddd EEEEmmmmbbbbeeeeddddddddiiiinnnngggg CCCCoooommmmmmmmaaaannnnddddssss

____________________________________________________________

_E_X_T_R_A_C_T_I_O_N _A_N_D _E_M_B_E_D_D_I_N_G _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

(_x_t_r . $) .  replaces the original current  expression  with
the expression that is current after performing (lcl . $).

(_m_b_d _x) .  x is a list, substitutes the  current  expression
for  all  instances  of  the  atom  * in x, and replaces the
current expression with the  result  of  that  substitution.
(mbd x) :  x atomic, same as (mbd (x *)).

(_e_x_t_r_a_c_t $_1 _f_r_o_m $_2) .  extract is an editor  command  which
replaces  the  current expression with one of its subexpres-
sions (from any depth).  ($1 is the segment between  extract
and  from.)    example:  if the current expression is (print
(cond ((null x) y) (t z))) then following  (extract  y  from
cond), the current expression will be (print y).  (extract 2
-1 from cond), (extract y from 2), (extract  2  -1  from  2)
will all produce the same result.

(_e_m_b_e_d $ _i_n . _x) .  embed replaces  the  current  expression
with  a new expression which contains it as a subexpression.
($ is the segment between embed and in.)   example:   (embed
print  in setq x), (embed 3 2 in _r_e_t_u_r_n), (embed cond 3 1 in
(or * (null x))).
____________________________________________________________







9

9                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                        11116666----11110000


11116666....7777....  MMMMoooovvvveeee aaaannnndddd CCCCooooppppyyyy CCCCoooommmmmmmmaaaannnnddddssss

____________________________________________________________

_M_O_V_E _A_N_D _C_O_P_Y _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

(_m_o_v_e $_1 _t_o _c_o_m . $_2) .  ($1 is the segment between move and
to.)  where com is before, after, or the name of a list com-
mand, e.g., :, n, etc.  If $2 is nil, or (here), the current
position specifies where the operation is to take place.  If
$1 is nil, the move command allows the user to specify  some
place  the  current  expression  is to be moved to.   if the
current expression is (a b d c), (move 2 to  after  4)  will
make the new current expression be (a c d b).

(_m_v _c_o_m . $) .  is the same as (move here to com . $).

(_c_o_p_y $_1 _t_o _c_o_m . $_2)  is like move except that  the  source
expression is not deleted.

(_c_p _c_o_m . $).  is like mv except that the source  expression
is not deleted.
____________________________________________________________






11116666....8888....  PPPPaaaarrrreeeennnntttthhhheeeesssseeeessss MMMMoooovvvviiiinnnngggg CCCCoooommmmmmmmaaaannnnddddssss   The commands  presented
in  this  section  permit modification of the list structure
itself, as opposed to modifying components  thereof.   their
effect  can  be  described as inserting or removing a single
left or  right  parenthesis,  or  pair  of  left  and  right
parentheses.

____________________________________________________________

_P_A_R_E_N_T_H_E_S_E_S _M_O_V_I_N_G _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

(_b_i _n _m) .  both in.  inserts  parentheses  before  the  nth
element and after the mth element in the current expression.
example:  if the current expression is (a b (c d  e)  f  g),
then (bi 2 4) will modify it to be (a (b (c d e) f) g).  (bi
n) :  same as (bi n n).  example:  if the current expression
is (a b (c d e) f g), then (bi -2) will modify it to be (a b
(c d e) (f) g).

(_b_o _n) .  both out.  removes both parentheses from  the  nth
element.   example:   if the current expression is (a b (c d
e) f g), then (bo d) will modify it to be (a b c d e f g).

(_l_i _n) .  left in.  inserts a left  parenthesis  before  the


                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                        11116666----11111111


nth  element (and a matching right parenthesis at the end of
the current expression).  example:  if the  current  expres-
sion  is (a b (c d e) f g), then (li 2) will modify it to be
(a (b (c d e) f g)).

(_l_o _n) .  left  out.  removes  a  left parenthesis  from the
nth  element.  all  elements  following  the nth element are
deleted.  example: if the current expression is (a b (c d e)
f g), then  (lo  3) will modify it to be (a b c d e).

(_r_i _n _m) .  right  in.  move  the  right parenthesis at  the
end of the nth element in to after the mth element.  inserts
a  right parenthesis  after  the mth element of the nth ele-
ment.   The  rest  of  the  nth element is brought up to the
level of the  current expression.   example: if the  current
expression  is (a (b c d e) f g), (ri 2 2) will modify it to
be (a (b c) d e f g).

(_r_o _n) .  right  out.  move the right parenthesis at the end
of  the  nth  element out to the  end of the current expres-
sion.  removes the right parenthesis from the  nth  element,
moving it to the end of the current expression. all elements
following the nth  element  are moved  inside of   the   nth
element.  example: if the current expression is (a b (c d e)
f  g),  (ro  3) will modify  it to be (a b (c d e f g)).

(_r _x _y)  replaces  all  instances  of x by y in the  current
expression,  e.g.,  (r  caadr  cadar).   x  can  be  the  s-
expression (or atom) to be substituted for, or can be a pat-
tern which specifies that s-expression (or atom).

(_s_w _n _m)  switches the nth and mth elements of  the  current
expression.  for example, if the current expression is (list
(cons (car x) (car y)) (cons (cdr y))),   (sw   2  3)   will
modify  it  to be (list (cons (cdr x) (cdr y)) (cons (car x)
(car y))).   (sw car  cdr) would produce the same result.
____________________________________________________________






11116666....8888....1111....  UUUUssssiiiinnnngggg ttttoooo aaaannnndddd tttthhhhrrrruuuu

to, thru, extract, embed, delete, replace, and move  can  be
made to operate on several contiguous elements, i.e., a seg-
ment of a list, by using the to or  thru  command  in  their
respective   location   specifications.   thru  and  to  are
intended to be used  in  conjunction  with  extract,  embed,
delete,  replace,  and move.    to and thru can also be used
directly with xtr (which takes after a  location  specifica-
tion), as in (xtr (2 thru 4)) (from the current expression).


                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                        11116666----11112222


____________________________________________________________

_T_O _A_N_D _T_H_R_U _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

($_1 _t_o $_2)    .   same  as  thru  except  last  element  not
included.

($_1 _t_o).  same as ($1 thru -1)

($_1 _t_h_r_u $_2)  .  If the current expression is (a  (b  (c  d)
(e)  (f  g  h)  i)  j  k), following (c thru g), the current
expression will be ((c d) (e) (f g h)).  If both $1  and  $2
are  numbers, and $2 is greater than $1, then $2 counts from
the beginning of the current expression, the same as $1.  in
other  words,  if the current expression is (a b c d e f g),
(3 thru 4) means (c thru d), not (c thru f).  in this  case,
the corresponding bi command is (bi 1 $2-$1+1).

($_1 _t_h_r_u). same as ($_1 _t_h_r_u -_1).
____________________________________________________________






11116666....9999....  UUUUnnnnddddooooiiiinnnngggg CCCCoooommmmmmmmaaaannnnddddssss   each command that causes structure
modification  automatically  adds  an  entry to the front of
undolst containing the information required to  restore  all
pointers that were changed by the command.  The undo command
undoes the last, i.e., most recent such command.

____________________________________________________________

_U_N_D_O _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

_u_n_d_o .  the  undo  command  undoes  most  recent,  structure
modification  command  that  has  not  yet  been undone, and
prints the name of that command, e.g., mbd undone.  The edit
chain  is  then exactly what it was before the 'undone' com-
mand had been performed.

!_u_n_d_o .  undoes  all  modifications  performed  during  this
editing session, i.e., this call to the editor.

_u_n_b_l_o_c_k .  removes  an  undo-block. If executed  at  a  non-
blocked  state,  i.e., if undo or !undo could operate, types
not blocked.

_t_e_s_t .  adds an undo-block at the front  of  undolst.   note
that  test   together   with  !undo  provide  a  'tentative'
mode  for editing, i.e., the user can perform  a  number  of
changes,  and  then  undo  all  of  them with a single !undo


                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                        11116666----11113333


command.

_u_n_d_o_l_s_t [_v_a_l_u_e].  each editor command that causes  structure
modification  automatically  adds  an  entry to the front of
undolst containing the information required to  restore  all
pointers that were changed by the command.

??   prints the entries on undolst.  The entries are  listed
most recent entry first.
____________________________________________________________






11116666....11110000....  CCCCoooommmmmmmmaaaannnnddddssss tttthhhhaaaatttt EEEEvvvvaaaalllluuuuaaaatttteeee

____________________________________________________________

_E_V_A_L_U_A_T_I_O_N _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

_e .  only when typed in, (i.e., (insert  d  before  e)  will
treat  e  as  a  pattern) causes  the  editor  to  call  the
lisp interpreter giving it the next input as argument.

(_e  _x)  evaluates  x, and prints the result.  (e x  t)  same
as (e x) but does not print.

(_i _c _x_1 ... _x_n)  same as (c y1 ...  yn) where yi=(eval  xi).
example:   (i  3  (cdr foo)) will replace the 3rd element of
the current expression with the cdr of the value of foo.  (i
n foo (car fie)) will attach the value of foo and car of the
value of fie to the end of the current  expression.   (i  f=
foo t) will search for an expression eq to the value of foo.
If c is not an atom, it is evaluated as well.

(_c_o_m_s _x_1 ... _x_n) .  each  xi  is  evaluated  and  its  value
executed as a command.  The i command is not very convenient
for computing an entire edit command for  execution,   since
it  computes  the command name and its arguments separately.
also, the i command cannot be used  to  compute  an   atomic
command.   The  coms  and comsq  commands provide more  gen-
eral ways of computing commands.  (coms  (cond  (x  (list  1
x))))  will replace the first element of the current expres-
sion with the value of x if non-nil, otherwise  do  nothing.
(nil as a command is a nop.)

(_c_o_m_s_q _c_o_m_1 ... _c_o_m_n) .  executes com1 ... comn.   comsq  is
mainly  useful in conjunction with the  coms command.    for
example,  suppose the user wishes to compute an entire  list
of  commands  for  evaluation,  as opposed to computing each
command one at a time  as does  the coms command.  he  would


                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                        11116666----11114444


then  write  (coms  (cons (quote comsq) x)) where x computed
the list of commands, e.g.,   (coms   (cons   (quote  comsq)
(get  foo (quote commands))))
____________________________________________________________






11116666....11111111....  CCCCoooommmmmmmmaaaannnnddddssss tttthhhhaaaatttt TTTTeeeesssstttt

____________________________________________________________

_T_E_S_T_I_N_G _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

(_i_f _x)  generates an error unless the value of (eval  x)  is
non-nil,  i.e., if (eval x) causes an error or (eval x)=nil,
if will cause an error.  (if x coms1 coms2) if (eval  x)  is
non-nil,  execute  coms1;  if (eval x) causes an error or is
equal to nil, execute coms2.  (if x coms1)   if   (eval   x)
is  non-nil,  execute  coms1; otherwise generate  an  error.

(_l_p . _c_o_m_s) .  repeatedly executes coms, a list of commands,
until  an   error   occurs.      (lp  f  print (n  t))  will
attach  a t  at the end of every  print  expression.  (lp  f
print  (if  (##  3) nil ((n t)))) will attach a t at the end
of each print expression  which  does  not  already  have  a
second  argument.   (i.e.   the  form   (## 3) will cause an
error if the edit command 3 causes an error, thereby select-
ing  ((n t)) as the list of commands to be executed.  The if
could also  be written  as  (if  (cddr (##)) nil ((n t))).).

(_l_p_q . _c_o_m_s)  same as lp but does not print n occurrences.

(_o_r_r _c_o_m_s_1 ... _c_o_m_s_n) .  orr begins by  executing  coms1,  a
list  of  commands.   If  no  error occurs, orr is finished.
otherwise, orr restores the edit  chain  to   its   original
value,   and  continues by executing coms2, etc.  If none of
the command lists execute without  errors,   i.e.,  the  orr
"drops off the end", orr generates an error.  otherwise, the
edit chain is left as of the completion of the first command
list  which executes  without error.
____________________________________________________________








9

9                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                        11116666----11115555


11116666....11112222....  EEEEddddiiiittttoooorrrr MMMMaaaaccccrrrroooossss

Many of the more sophisticated  branching  commands  in  the
editor,  such as orr, if,  etc.,  are  most  often  used  in
conjunction with edit macros.  The macro feature permits the
user  to define new commands and thereby expand the editor's
repertoire. (however, built in commands always   take   pre-
cedence   over   macros, i.e.,  the  editor's repertoire can
be expanded, but not modified.) macros are defined by  using
the m command.

(_m _c . _c_o_m_s)  for c an atom, m defines c as an  atomic  com-
mand.   (if  a  macro   is   redefined,  its  new definition
replaces its old.) executing c is then the same as executing
the   list  of   commands  coms.    macros  can  also define
list commands, i.e., commands that take arguments.   (m  (c)
(arg[1]  ...  arg[n])  .  coms) c an atom.  m defines c as a
list command.  executing (c e1 ...  en)  is then   performed
by  substituting   e1   for   arg[1],  ...    en  for arg[n]
throughout coms, and then executing coms.   a  list  command
can  be  defined  via a macro  so  as  to  take  a fixed  or
indefinite  number of 'arguments'.   The  form  given  above
specified a macro with a fixed number of arguments, as indi-
cated by its argument list.   if the of arguments.   (m  (c)
args  . coms) c,  args  both  atoms,  defines  c as  a  list
command.  executing  (c  e1 ...  en) is performed by substi-
tuting  (e1  ...   en),  i.e.,  cdr of the command, for args
throughout coms, and then executing coms.

(m bp bk up p) will define bp as an atomic   command   which
does  three  things,  a bk, an up, and a p. note that macros
can use commands defined by macros as well as built in  com-
mands in  their definitions.    for  example, suppose  z  is
defined by (m z -1 (if (null (##)) nil (p))), i.e. z does  a
-1,  and then if the current expression is not nil, a p. now
we can define zz by  (m zz  -1 z), and zzz by (m zzz  -1  -1
z)  or  (m zzz -1 zz).  we could define a more general bp by
(m (bp) (n) (bk n) up p).    (bp 3)  would  perform  (bk 3),
followed  by  an  up,  followed  by a p.  The command second
can be defined as a macro by (m (2nd) x (orr ((lc .  x)  (lc
.  x)))).

Note  that  for  all editor commands, 'built in' commands as
well as commands defined by macros, atomic  definitions  and
list definitions  are   completely  independent.   in  other
words, the existence of an atomic definition for c in no way
affects  the treatment of c when it appears as car of a list
command,  and the existence of a list definition for c in no
way affects the treatment of  c when it appears as an  atom.
in particular, c can be used as the name of either an atomic
command, or a list command, or both. in the latter case, two
entirely  different  definitions  can  be  used.   note also
that once c is defined as an  atomic  command  via  a  macro


                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                        11116666----11116666


definition, it will not be searched for when used in a loca-
tion specification, unless c is preceded by an  f.   (insert
--   before bp) would not search for bp, but instead perform
a bk, an up, and a  p,  and  then  do  the  insertion.   The
corresponding also holds true for list commands.

(_b_i_n_d . _c_o_m_s)  bind  is  an  edit  command which  is  useful
mainly  in  macros.   it binds three dummy variables #1, #2,
#3, (initialized to nil), and then  executes  the  edit com-
mands   coms.   note  that these bindings are only in effect
while the commands are being executed, and that bind can  be
used recursively; it will  rebind #1, #2,  and  #3 each time
it is invoked.

_u_s_e_r_m_a_c_r_o_s [_v_a_l_u_e].   this   variable   contains  the  users
editing  macros .   if you want to save your macros then you
should save usermacros.  you  should  probably   also   save
editcomsl.

_e_d_i_t_c_o_m_s_l [_v_a_l_u_e].  editcomsl  is  the  list of  "list  com-
mands" recognized by the editor.  (these are the ones of the
form (command arg1 arg2 ...).)





11116666....11113333....  MMMMiiiisssscccceeeellllllllaaaannnneeeeoooouuuussss EEEEddddiiiittttoooorrrr CCCCoooommmmmmmmaaaannnnddddssss

____________________________________________________________

_M_I_S_C_E_L_L_A_N_E_O_U_S _E_D_I_T_O_R _C_O_M_M_A_N_D _S_U_M_M_A_R_Y

_o_k .  Exits from the editor.

_n_i_l .  Unless preceded by f or bf, is always a  null  opera-
tion.

_t_t_y:  .  Calls  the  editor  recursively.  The user can then
type in commands, and have them executed.  The tty:  command
is completed when  the  user exits  from  the lower   editor
(with   ok   or  stop).  the tty:  command is extremely use-
ful. it enables the user to  set  up a  complex   operation,
and   perform   interactive attention-changing commands part
way through it. for example the command  (move  3  to  after
cond  3  p  tty:)  allows  the user to interact, in  effect,
within  the move  command.      he can  verify  for  himself
that  the  correct  location has been found, or complete the
specification "by hand". in effect, tty: says "I'll tell you
what you should do when you get there."

_s_t_o_p .  exits from the editor with an error.  mainly for use
in  conjunction  with  tty:  commands that the user wants to


                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                        11116666----11117777


abort.  since all of the commands in the editor are   errset
protected, the user must exit from the editor via a command.
stop provides a way of distinguishing between  a  successful
and unsuccessful  (from the user's  standpoint) editing ses-
sion.

_t_l .  tl  calls (top-level).  to return to the  editor  just
use the _r_e_t_u_r_n top-level command.

_r_e_p_a_c_k .  permits the 'editing' of an atom or string.

(_r_e_p_a_c_k $) does (lc . $) followed by  repack,  e.g.  (repack
this@).

(_m_a_k_e_f_n _f_o_r_m _a_r_g_s _n _m) .  makes (car form) an expr with  the
nth  through mth  elements  of  the  current expression with
each occurrence  of  an element of (cdr  form)  replaced  by
the  corresponding  element  of  args.   The nth through mth
elements  are replaced  by form.

(_m_a_k_e_f_n _f_o_r_m _a_r_g_s _n).  same as (makefn form args n n).

(_s _v_a_r . $) .  sets var (using setq) to the current  expres-
sion  after  performing  (lc .    $).    (s  foo)  will  set
foo to the current expression, (s foo -1 1) will set foo  to
the first element in the last element of the current expres-
sion.
____________________________________________________________






11116666....11114444....  EEEEddddiiiittttoooorrrr FFFFuuuunnnnccccttttiiiioooonnnnssss


((((eeeeddddiiiittttffff s_x1 ...))))

     SIDE EFFECT: edits a function. s_x1 is the name of  the
                  function,  any additional arguments are an
                  optional list of commands.

     RETURNS: s_x1.

     NOTE: if s_x1 is not an editable function,  editf  gen-
           erates an fn not editable error.





9

9                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                        11116666----11118888


((((eeeeddddiiiitttteeee l_expr l_coms s_atm)))))
edits an expression.  its  value  is  the  last  element  of
(editl (list l_expr) l_coms s_atm nil nil).


((((eeeeddddiiiittttrrrraaaacccceeeeffffnnnn s_com))))
is available to help the user debug complex edit macros,  or
subroutine  calls  to   the   editor.  editracefn  is  to be
defined by the user.  whenever the value of  editracefn   is
non-nil,   the   editor   calls  the   function   editracefn
before executing  each command (at  any  level),  giving  it
that command as its argument.  editracefn is initially equal
to nil, and undefined.


((((eeeeddddiiiittttvvvv s_var [ g_com1 ... ]))))

     SIDE EFFECT: similar  to  editf,  for  editing  values.
                  editv  sets  the  variable  to  the  value
                  returned.

     RETURNS: the name  of  the  variable  whose  value  was
              edited.


((((eeeeddddiiiittttpppp s_x))))

     SIDE EFFECT: similar  to  editf  for  editing  property
                  lists. used if x is nil.

     RETURNS: the atom whose property list was edited.


((((eeeeddddiiiittttllll coms atm marklst mess))))

     SIDE EFFECT: editl is the editor.  its  first  argument
                  is  the  edit  chain,  and its value is an
                  edit chain, namely the value of l  at  the
                  time  editl  is  exited.   (l is a special
                  variable, and so can be examined or set by
                  edit  commands.    ^  is  equivalent to (e
                  (setq l(last l)) t).)  coms is an optional
                  list  of  commands.  for interactive edit-
                  ing, coms is nil.   in  this  case,  editl
                  types  edit  and then waits for input from
                  the teletype.  (if mess is not  nil  editl
                  types  it  instead  of edit.  for example,
                  the tty:  command is essentially  (setq  l
                  (editl  l  nil  nil  nil  (quote tty:))).)
                  exit occurs only via an ok, stop, or  save
                  command.   If  coms is not nil, no message
                  is typed,  and  each  member  of  coms  is
                  treated  as a command and executed.  If an


                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                        11116666----11119999


                  error occurs in the execution  of  one  of
                  the  commands, no error message is printed
                  , the rest of the  commands  are  ignored,
                  and  editl  exits with an error, i.e., the
                  effect is the same as though a  stop  com-
                  mand  had  been executed.  If all commands
                  execute successfully,  editl  returns  the
                  current  value  of l.  marklst is the list
                  of marks.  on calls from editf, atm is the
                  name  of  the  function  being  edited; on
                  calls from editv, the name  of  the  vari-
                  able,  and  calls  from editp, the atom of
                  which some property of its  property  list
                  is being edited.  The property list of atm
                  is used by the save command for saving the
                  state  of  the  edit.   save will not save
                  anything if  atm=nil  i.e.,  when  editing
                  arbitrary  expressions  via edite or editl
                  directly.


((((eeeeddddiiiittttffffnnnnssss s_x [ g_coms1 ... ]))))
fsubr function, used to perform the same editing  operations
on  several  functions.  editfns maps down the list of func-
tions, prints the name of each function, and calls the  edi-
tor (via editf) on that function.

     EXAMPLE: editfns foofns (r fie fum)) will change  every
              fie   to   fum   in  each  of the functions on
              foofns.

     NOTE: the  call  to  the  editor is  errset  protected,
           so  that if the editing of one function causes an
           error, editfns will proceed to  the  next   func-
           tion.     in  the  above  example,  if one of the
           functions did not contain a fie,  the  r  command
           would   cause   an  error, but editing would con-
           tinue with  the  next  function.   The  value  of
           editfns is nil.


((((eeeeddddiiiitttt4444eeee pat y))))

     SIDE EFFECT: is the pattern match routine.

     RETURNS: t if pat matches y. see edit-match for defini-
              tion of 'match'.

     NOTE: before  each  search  operation  in  the   editor
           begins,  the   entire   pattern  is  scanned  for
           atoms  or strings that end  in  at-signs.   These
           are  replaced  by  patterns  of  the  form  (cons
           (quote  /@)  (explodec   atom)).       from   the


                                     Printed: March 23, 1982







TTTThhhheeee LLLLIIIISSSSPPPP EEEEddddiiiittttoooorrrr                                        11116666----22220000


           standpoint   of  edit4e, pattern type 5, atoms or
           strings  ending  in  at-signs,  is   really   "if
           car[pat]  is the atom @ (at-sign), pat will match
           with  any  literal atom  or  string  whose   ini-
           tial   character codes (up to the @) are the same
           as those in cdr[pat]." if  the  user  wishes   to
           call  edit4e  directly, he must therefore convert
           any patterns which contain   atoms   or   strings
           ending  in  at-signs  to  the  form recognized by
           edit4e.   this  can  be  done  via  the  function
           editfpat.

((((eeeeddddiiiittttffffppppaaaatttt pat flg))))
makes a copy of pat with all patterns of type 5  (see  edit-
match)  converted to the form expected by edit4e. flg should
be passed as nil (flg=t is for internal use by the editor).


((((eeeeddddiiiittttffffiiiinnnnddddpppp x pat flg))))

     NOTE: Allows a program to use the edit find command  as
           a  pure  predicate from outside the editor.  x is
           an expression, pat a pattern.  The value of edit-
           findp  is  t  if the command f pat would succeed,
           nil otherwise.  editfindp calls editfpat to  con-
           vert  pat  to the form expected by edit4e, unless
           flg=t.    if the program is applying editfindp to
           several different expressions using the same pat-
           tern, it will be more efficient to call  editfpat
           once,  and then call editfindp with the converted
           pattern and flg=t.


((((######## g_com1 ...))))

     RETURNS: what the current  expression  would  be  after
              executing the edit commands com1 ...  starting
              from the present  edit  chain.   generates  an
              error  if  any  of  comi  cause  errors.   The
              current edit chain is never changed.  example:
              (i  r  (quote x) (## (cons ..z))) replaces all
              x's in the current  expression  by  the  first
              cons containing a z.









9

9                                     Printed: March 23, 1982



