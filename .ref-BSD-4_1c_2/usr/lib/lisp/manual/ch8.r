






                         CCCCHHHHAAAAPPPPTTTTEEEERRRR  8888


                    FFFFuuuunnnnccccttttiiiioooonnnnssss aaaannnndddd MMMMaaaaccccrrrroooossss







8888....1111....  vvvvaaaalllliiiidddd ffffuuuunnnnccccttttiiiioooonnnn oooobbbbjjjjeeeeccccttttssss

     There are many different objects which can  occupy  the
function  field  of  a  symbol  object.  The following table
shows all of the possibilities, how to  recognize  them  and
where to look for documentation.


8_____________________________________________________________
  informal name           object type         documentation
8__________________________________________________________________________________________________________________________
   interpreted           list with _c_a_r             8.2
 lambda function         _e_q to lambda
8_____________________________________________________________
   interpreted           list with _c_a_r             8.2
 nlambda function        _e_q to nlambda
8_____________________________________________________________
   interpreted           list with _c_a_r             8.2
  lexpr function          _e_q to lexpr
8_____________________________________________________________
   interpreted           list with _c_a_r             8.3
      macro               _e_q to macro
8_____________________________________________________________
     compiled       binary with discipline         8.2
 lambda or lexpr         _e_q to lambda
     function
8_____________________________________________________________
     compiled       binary with discipline         8.2
 nlambda function        _e_q to nlambda
8_____________________________________________________________
     compiled       binary with discipline         8.3
      macro               _e_q to macro
8_____________________________________________________________
     foreign        binary with discipline         8.4
    subroutine         of "subroutine"[]
8_____________________________________________________________
     foreign        binary with discipline         8.4
     function           of "function"[]
8_____________________________________________________________
     foreign        binary with discipline         8.4
 integer function   of "integer-function"[]
8_____________________________________________________________
     foreign        binary with discipline         8.4
  real function      of "real-function"[]
8_____________________________________________________________
      array              array object               9
8_____________________________________________________________
7|8|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|





























9                 |8|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|





























9                                           |8|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|





























9                                                            |8|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|7|































99

9FFFFuuuunnnnccccttttiiiioooonnnnssss aaaannnndddd MMMMaaaaccccrrrroooossss                                     8888----1111







FFFFuuuunnnnccccttttiiiioooonnnnssss aaaannnndddd MMMMaaaaccccrrrroooossss                                     8888----2222


8888....2222....  ffffuuuunnnnccccttttiiiioooonnnnssss

     The basic lisp function is the lambda function.  When a
lambda   function   is  called,  the  actual  arguments  are
evaluated from left to right and  are  lambda-bound  to  the
formal parameters of the lambda function.

     An nlambda function is usually used for functions which
are  invoked  by the user at top level.  Some built-in func-
tions which evaluate their arguments  in  special  ways  are
also  nlambdas (e.g _c_o_n_d, _d_o, _o_r).  When an nlambda function
is called, the list of unevaluated arguments is lambda bound
to the single formal parameter of the nlambda function.

     Some programmers will use an nlambda function when they
are  not  sure  how many arguments will be passed.  Then the
first thing the nlambda function does is map _e_v_a_l  over  the
list  of  unevaluated arguments it has been passed.  This is
usually the wrong thing to do as it won't work  compiled  if
any of the arguments are local variables. The solution is to
use a lexpr.  When a lexpr function is called, the arguments
are  evaluated  and  a  fixnum  whose value is the number of
arguments is lambda-bound to the single formal parameter  of
the  lexpr  function.  The lexpr then accesses the arguments
using the _a_r_g function.

     When a function is compiled, _s_p_e_c_i_a_l  declarations  may
be  needed  to  preserve  its  behavior.  An argument is not
lambda-bound to the name of the corresponding formal parame-
ter  unless  that formal parameter has been declared _s_p_e_c_i_a_l
(see 12.3.2.2).

     Lambda and lexpr functions both compile into  a  binary
object  with  a  discipline  of lambda.  However, a compiled
lexpr still acts like an interpreted lexpr.




8888....3333....  mmmmaaaaccccrrrroooossss

     An important features of Lisp is its ability to manipu-
late  programs  as  data.   As  a  result of this, most Lisp
implementations have very powerful  macro  facilities.   The
Lisp  language's  macro  facility can be used to incorporate
popular features of the  other  languages  into  Lisp.   For
example,  there are macro packages which allow one to create
records (ala Pascal) and refer to elements of those  records
____________________
9   []Only the first character of the string  is  significant
(i.e "s" is ok for "subroutine")



9                                     Printed: March 23, 1982







FFFFuuuunnnnccccttttiiiioooonnnnssss aaaannnndddd MMMMaaaaccccrrrroooossss                                     8888----3333


by the key names.[] Another  popular use for  macros  is  to
create  more  readable  control structures which expand into
_c_o_n_d, _o_r and _a_n_d.  One such example is the If macro  in  the
jkfmacs.l package.  It allows you to write

(_I_f (_e_q_u_a_l _n_u_m_b _0) _t_h_e_n (_p_r_i_n_t '_z_e_r_o) (_t_e_r_p_r)
 _e_l_s_e_i_f (_e_q_u_a_l _n_u_m_b _1) _t_h_e_n (_p_r_i_n_t '_o_n_e) (_t_e_r_p_r)
 _e_l_s_e (_p_r_i_n_t '|_I _g_i_v_e _u_p|))

which expands to

(_c_o_n_d
    ((_e_q_u_a_l _n_u_m_b _0) (_p_r_i_n_t '_z_e_r_o) (_t_e_r_p_r))
    ((_e_q_u_a_l _n_u_m_b _1) (_p_r_i_n_t '_o_n_e) (_t_e_r_p_r))
    (_t (_p_r_i_n_t '|_I _g_i_v_e _u_p|)))





8888....3333....1111....  mmmmaaaaccccrrrroooo ffffoooorrrrmmmmssss

     A macro is a function which accepts a  Lisp  expression
as  input  and  returns another Lisp expression.  The action
the macro takes is called macro expansion.  Here is a simple
example:

-> (_d_e_f _f_i_r_s_t (_m_a_c_r_o (_x) (_c_o_n_s '_c_a_r (_c_d_r _x))))
first
-> (_f_i_r_s_t '(_a _b _c))
a
-> (_a_p_p_l_y '_f_i_r_s_t '(_f_i_r_s_t '(_a _b _c)))
(car '(a b c))

The first input line defines a macro called  _f_i_r_s_t.   Notice
that  the  macro  has one formal parameter, _x. On the second
input   line,   we   ask   the   interpreter   to   evaluate
(_f_i_r_s_t '(_a _b _c)).   _E_v_a_l  sees  that  _f_i_r_s_t  has  a function
definition of type macro  so it evaluates _f_i_r_s_t's definition
passing  to  _f_i_r_s_t  as  an argument the form _e_v_a_l itself was
trying to evaluate: (_f_i_r_s_t '(_a _b _c)).  The _f_i_r_s_t macro chops
off  the  car  of  the argument with _c_d_r, cons' a _c_a_r at the
beginning of the list and returns (_c_a_r '(_a _b _c)).  Now  _e_v_a_l
evaluates  that, and the value is _a which is returned as the
value of (_f_i_r_s_t '(_a _b _c)).   Thus  whenever  _e_v_a_l  tries  to
evaluate  a list whose car has a macro definition it ends up
doing (at least) two operations, one is a call to the  macro
____________________
9   []A record definition macro package especially suited for
FRANZ  LISP  is in the planning stages at Berkeley.  At this
time the Maclisp _s_t_r_u_c_t package can be used.



9                                     Printed: March 23, 1982







FFFFuuuunnnnccccttttiiiioooonnnnssss aaaannnndddd MMMMaaaaccccrrrroooossss                                     8888----4444


to let it macro expand  the  form,  and  the  other  is  the
evaluation  of  the  result of the macro.  The result of the
macro may be yet another call to a macro, so _e_v_a_l  may  have
to  do  even more evaluations until it can finally determine
the  value of an expression.  One way to  see  how  a  macro
will expand is to use _a_p_p_l_y as shown on the third input line
above.




8888....3333....2222....  ddddeeeeffffmmmmaaaaccccrrrroooo

     The macro _d_e_f_m_a_c_r_o makes it  easier  to  define  macros
because  it  allows  you  to name the arguments to the macro
call.  For example, suppose we find ourselves often  writing
code  like (_s_e_t_q _s_t_a_c_k (_c_o_n_s _n_e_w_e_l_t _s_t_a_c_k).  We could define
a macro named _p_u_s_h to do this for us.  One way to define  it
is:

-> (_d_e_f _p_u_s_h
      (_m_a_c_r_o (_x) (_l_i_s_t '_s_e_t_q (_c_a_d_d_r _x) (_l_i_s_t '_c_o_n_s (_c_a_d_r _x) (_c_a_d_d_r _x)))))
push

then (_p_u_s_h _n_e_w_e_l_t _s_t_a_c_k) will expand to the  form  mentioned
above.  The same macro written using defmacro would be:

-> (_d_e_f_m_a_c_r_o _p_u_s_h (_v_a_l_u_e _s_t_a_c_k) (_l_i_s_t '_s_e_t_q _s_t_a_c_k (_l_i_s_t '_c_o_n_s _v_a_l_u_e _s_t_a_c_k))
push

Defmacro allows you to name the arguments of the macro call,
and  makes  the  macro  definition look more like a function
definition.




8888....3333....3333....  tttthhhheeee bbbbaaaacccckkkkqqqquuuuooootttteeee cccchhhhaaaarrrraaaacccctttteeeerrrr mmmmaaaaccccrrrroooo

     The default syntax for FRANZ LISP has four   characters
with associated character macros.  One is semicolon for com-
ments.  Two others are the backquote  and  comma  which  are
used  by  the  backquote character macro.  The fourth is the
sharp sign macro described in the next section.

     The backquote macro is used to create lists where  many
of  the elements are fixed (quoted). This makes it very use-
ful for creating macro definitions.  In the simplest case, a
backquote acts just like a single quote:

->`(_a _b _c _d _e)
(a b c d e)
9

9                                     Printed: March 23, 1982







FFFFuuuunnnnccccttttiiiioooonnnnssss aaaannnndddd MMMMaaaaccccrrrroooossss                                     8888----5555


If a comma precedes an element of  a  backquoted  list  then
that element is evaluated and its value is put in the list.

->(_s_e_t_q _d '(_x _y _z))
(x y z)
->`(_a _b _c ,_d _e)
(a b c (x y z) e)

If a comma followed by an at sign precedes an element  in  a
backquoted  list, then that element is evaluated and spliced
into the list with _a_p_p_e_n_d.

->`(_a _b _c ,@_d _e)
(a b c x y z e)

Once a list begins with a backquote, the commas  may  appear
anywhere in the list as this example shows:

->`(_a _b (_c _d ,(_c_d_r _d)) (_e _f (_g _h ,@(_c_d_d_r _d) ,@_d)))
(a b (c d (y z)) (e f (g h z x y z)))

It is also possible and sometimes even  useful  to  use  the
backquote  macro within itself.  As a final demonstration of
the backquote macro, we shall define the first and push mac-
ros  using  all  the power at our disposal, defmacro and the
backquote macro.

->(_d_e_f_m_a_c_r_o _f_i_r_s_t (_l_i_s_t) `(_c_a_r ,_l_i_s_t))
first
->(_d_e_f_m_a_c_r_o _p_u_s_h (_v_a_l_u_e _s_t_a_c_k) `(_s_e_t_q ,_s_t_a_c_k (_c_o_n_s ,_v_a_l_u_e ,_s_t_a_c_k)))
stack




8888....3333....4444....  sssshhhhaaaarrrrpppp ssssiiiiggggnnnn cccchhhhaaaarrrraaaacccctttteeeerrrr mmmmaaaaccccrrrroooo

     The sharp sign macro can perform a number of  different
functions   at  read time.  The character directly following
the sharp sign determines which function will be  done,  and
following lisp s-expressions may serve as arguments.




8888....3333....4444....1111....  ccccoooonnnnddddiiiittttiiiioooonnnnaaaallll iiiinnnncccclllluuuussssiiiioooonnnn

If you plan to run one source file in more than one environ-
ment then you may want to some pieces of code to be included
or not included depending on the environment. The C language
uses  "#ifdef" and "#ifndef" for this purpose, and lisp uses
"#+" and "#-".  The environment that the  sharp  sign  macro
checks  is  the  (_s_t_a_t_u_s _f_e_a_t_u_r_e_s) list which is initialized


                                     Printed: March 23, 1982







FFFFuuuunnnnccccttttiiiioooonnnnssss aaaannnndddd MMMMaaaaccccrrrroooossss                                     8888----6666


when the Lisp system is built  and which may be  altered  by
(_s_s_t_a_t_u_s _f_e_a_t_u_r_e _f_o_o)  and  (_s_s_t_a_t_u_s _n_o_f_e_a_t_u_r_e _b_a_r) The form
of conditional inclusion is
                        _#_+_w_h_e_n _w_h_a_t
where _w_h_e_n is either a symbol  or  an  expression  involving
symbols  and the functions _a_n_d, _o_r, and _n_o_t.  The meaning is
that _w_h_a_t will only be read in if _w_h_e_n is true.  A symbol in
_w_h_e_n  is  true  only  if it appears in the (_s_t_a_t_u_s _f_e_a_t_u_r_e_s)
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
          (_p_a_t_o_m "_H_e _i_s ")
          (_p_r_i_n_t (_c_d_r (_a_s_s_o_c _n_a_m_e _a_g_e_f_i_l_e)))
          (_p_a_t_o_m "_y_e_a_r_s _o_l_d")
          (_t_e_r_p_r))
    ____________________________________________________



The form
                        _#_-_w_h_e_n _w_h_a_t
is equivalent to
                     _#_+_(_n_o_t _w_h_e_n_) _w_h_a_t




8888....3333....4444....2222....  ffffiiiixxxxnnnnuuuummmm cccchhhhaaaarrrraaaacccctttteeeerrrr eeeeqqqquuuuiiiivvvvaaaalllleeeennnnttttssss

When working with fixnum equivalents  of  characters  it  is
often hard to remember the number corresponding to a charac-
ter.  The form
                            _#_/_c
is equivalent to the fixnum representation of character c.








9

9                                     Printed: March 23, 1982







FFFFuuuunnnnccccttttiiiioooonnnnssss aaaannnndddd MMMMaaaaccccrrrroooossss                                     8888----7777



    ____________________________________________________

    ; a function  which returns t if the user types y else it returns nil.
    ;
    -> (_d_e_f_u_n _y_e_s_o_r_n_o _n_i_l
          (_p_r_o_g_n (_a_n_s)
                 (_s_e_t_q _a_n_s (_t_y_i))
                 (_c_o_n_d ((_e_q_u_a_l _a_n_s #/_y) _t)
                       (_t _n_i_l))))
    ____________________________________________________







8888....3333....4444....3333....  rrrreeeeaaaadddd ttttiiiimmmmeeee eeeevvvvaaaalllluuuuaaaattttiiiioooonnnn

Occasionally you want  to  express  a  constant  as  a  lisp
expression,  yet  you  don't  want  to  pay  the  penalty of
evaluating this expression each time it is referenced.   The
form
                        _#_._e_x_p_r_e_s_s_i_o_n
evaluates the expression at read time and returns its value.


    ____________________________________________________

    ; a function to test if any of bits 1 3 or 12 are set in a fixnum.
    ;
    -> (_d_e_f_u_n _t_e_s_t_i_t (_n_u_m)
          (_c_o_n_d ((_z_e_r_o_p (_b_o_o_l_e _1 _n_u_m #.(+ (_l_s_h _1 _1) (_l_s_h _1 _3) (_l_s_h _1 _1_2))))
                 _n_i_l)
                (_t _t)))
    ____________________________________________________







8888....4444....  ffffoooorrrreeeeiiiiggggnnnn ssssuuuubbbbrrrroooouuuuttttiiiinnnneeeessss aaaannnndddd ffffuuuunnnnccccttttiiiioooonnnnssss

     FRANZ LISP has the ability to dynamically  load  object
files  produced  by  other compilers and then call functions
defined in those files.  These functions are called  _f_o_r_e_i_g_n
functions.   There  are  four types of foreign functions and
they are characterized by the type of result they return:

9

9                                     Printed: March 23, 1982







FFFFuuuunnnnccccttttiiiioooonnnnssss aaaannnndddd MMMMaaaaccccrrrroooossss                                     8888----8888


subroutine
     This does not return anything. The lisp  system  always
     returns t after calling a subroutine.

function
     This returns whatever the function returns.  This  must
     be  a valid lisp object or it may cause the lisp system
     to fail.

integer-function
     This returns an integer which  the  lisp  system  makes
     into a fixnum and returns.

real-function
     This returns a double precision real number  which  the
     lisp system makes into a flonum and returns.

A foreign function is accessed through a binary object  just
like  a  compiled lisp function.  The difference is that the
discipline field for a binary object of a  foreign  function
is  a string whose first character is either s for a subrou-
tine, f for a function, i for an integer-function or r for a
real-function.   Two  functions are provided for the setting
up of foreign functions.  _C_f_a_s_l loads an  object  file  into
the  lisp  system  and  sets  up one foreign function binary
object.  If there is more than one  function  in  an  object
file, _g_e_t_a_d_d_r_e_s_s can be used to set up further foreign func-
tion objects.

     Foreign  functions are called  just  like  other  func-
tions,  e.g  (_f_u_n_n_a_m_e _a_r_g_1 _a_r_g_2).   When  one is called, the
arguments are evaluated and then examined.  List,  hunk  and
symbol  arguments  are passed unchanged to the foreign func-
tion.  Fixnum and flonum arguments are copied  into  a  tem-
porary  location  and a pointer to the value is passed (this
is  because  Fortran  uses  call  by  reference  and  it  is
dangerous to modify the contents of a fixnum or flonum which
something else might point to).  If an array  object  is  an
argument  the  data  field of the array  object is passed to
the foreign function (this is the easiest way to send  large
amounts  of data to and receive large amounts of data from a
foreign function).  If a binary object is an  argument,  the
entry field of that object is passed to the foreign function
(the entry field is the  address  of  a  function,  so  this
amounts to passing a function as an argument).

     The method a foreign function uses to access the  argu-
ments  provided  by lisp is dependent on the language of the
foreign function.  The following scripts demonstrate how how
lisp  can  interact with three languages: C, Pascal and For-
tran.  C and Pascal have pointer types and the first  script
shows  how  to use pointers to extract information from lisp
objects.  There are two functions defined for each language.


                                     Printed: March 23, 1982







FFFFuuuunnnnccccttttiiiioooonnnnssss aaaannnndddd MMMMaaaaccccrrrroooossss                                     8888----9999


The  first  (cfoo  in C, pfoo in Pascal) is given four argu-
ments, a fixnum, a flonum-block array, a hunk  of  at  least
two  fixnums  and a list of at least two fixnums.  To demon-
strate that the  values  were  passed,  each  ?foo  function
prints  its arguments (or parts of them).  The ?foo function
then modifies the second element of the  flonum-block  array
and  returns  a 3 to lisp.  The second function (cmemq in C,
pmemq in Pascal) acts  just  like  the  lisp  _m_e_m_q  function
(except it won't work for fixnums whereas the lisp _m_e_m_q will
work for small fixnums).  In the script, typed input  is  in
bbbboooolllldddd,  computer  output  is  in  roman  and  comments are in
_i_t_a_l_i_c.


____________________________________________________________

_T_h_e_s_e _a_r_e _t_h_e _C _c_o_d_e_d _f_u_n_c_t_i_o_n_s
% ccccaaaatttt cccchhhh8888aaaauuuuxxxxcccc....cccc
/* demonstration of c coded foreign integer-function */

/* the following will be used to extract fixnums out of a list of fixnums */
struct listoffixnumscell
{    struct listoffixnumscell *cdr;
     int *fixnum;
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


                                     Printed: March 23, 1982







FFFFuuuunnnnccccttttiiiioooonnnnssss aaaannnndddd MMMMaaaaccccrrrroooossss                                    8888----11110000


_T_h_e_s_e _a_r_e _t_h_e _P_a_s_c_a_l _c_o_d_e_d _f_u_n_c_t_i_o_n_s
% ccccaaaatttt cccchhhh8888aaaauuuuxxxxpppp....pppp
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
   writeln(' ( ', d.fixnum^, d.cdr^.fixnum^, ' ...) ');
   b[1] := 3.1415926;
   pfoo := 3
end ;

{ the function pmemq looks for the lisp pointer given as the first argument
  in the list pointed to by the second argument.
  Note that we declare " a : integer " instead of " var a : integer " since
  we are interested in the pointer value instead of what it points to (which
  could be any lisp object)
}
function pmemq( a : integer; list : plistcell) : plistcell;
begin
 while (list <> nil) and (list^.car <> a) do list := list^.cdr;
 pmemq := list;
end ;


_T_h_e _f_i_l_e_s _a_r_e _c_o_m_p_i_l_e_d
% cccccccc ----cccc cccchhhh8888aaaauuuuxxxxcccc....cccc
1.0u 1.2s 0:15 14% 30+39k 33+20io 147pf+0w
% ppppcccc ----cccc cccchhhh8888aaaauuuuxxxxpppp....pppp
3.0u 1.7s 0:37 12% 27+32k 53+32io 143pf+0w


% lllliiiisssspppp
Franz Lisp, Opus 33b
_F_i_r_s_t _t_h_e _f_i_l_e_s _a_r_e _l_o_a_d_e_d _a_n_d _w_e _s_e_t _u_p _o_n_e  _f_o_r_e_i_g_n  _f_u_n_c_-
_t_i_o_n  _b_i_n_a_r_y.  _W_e _h_a_v_e _t_w_o _f_u_n_c_t_i_o_n_s _i_n _e_a_c_h _f_i_l_e _s_o _w_e _m_u_s_t
_c_h_o_o_s_e _o_n_e _t_o _t_e_l_l _c_f_a_s_l _a_b_o_u_t.  _T_h_e _c_h_o_i_c_e _i_s _a_r_b_i_t_r_a_r_y.
-> ((((ccccffffaaaassssllll ''''cccchhhh8888aaaauuuuxxxxcccc....oooo ''''____ccccffffoooooooo ''''ccccffffoooooooo """"iiiinnnntttteeeeggggeeeerrrr----ffffuuuunnnnccccttttiiiioooonnnn""""))))


                                     Printed: March 23, 1982







FFFFuuuunnnnccccttttiiiioooonnnnssss aaaannnndddd MMMMaaaaccccrrrroooossss                                    8888----11111111


/usr/lib/lisp/nld -N -A /usr/local/lisp -T 63000 ch8auxc.o -e _cfoo -o /tmp/Li7055.0  -lc
#63000-"integer-function"
-> ((((ccccffffaaaassssllll ''''cccchhhh8888aaaauuuuxxxxpppp....oooo ''''____ppppffffoooooooo ''''ppppffffoooooooo """"iiiinnnntttteeeeggggeeeerrrr----ffffuuuunnnnccccttttiiiioooonnnn"""" """"----llllppppcccc""""))))
/usr/lib/lisp/nld -N -A /tmp/Li7055.0 -T 63200 ch8auxp.o -e _pfoo -o /tmp/Li7055.1 -lpc -lc
#63200-"integer-function"
_H_e_r_e _w_e _s_e_t _u_p _t_h_e _o_t_h_e_r _f_o_r_e_i_g_n _f_u_n_c_t_i_o_n _b_i_n_a_r_y _o_b_j_e_c_t_s
-> ((((ggggeeeettttaaaaddddddddrrrreeeessssssss ''''____ccccmmmmeeeemmmmqqqq ''''ccccmmmmeeeemmmmqqqq """"ffffuuuunnnnccccttttiiiioooonnnn"""" ''''____ppppmmmmeeeemmmmqqqq ''''ppppmmmmeeeemmmmqqqq """"ffffuuuunnnnccccttttiiiioooonnnn""""))))
#6306c-"function"
_W_e _w_a_n_t _t_o _c_r_e_a_t_e _a_n_d _i_n_i_t_i_a_l_i_z_e _a_n _a_r_r_a_y  _t_o  _p_a_s_s  _t_o  _t_h_e
_c_f_o_o  _f_u_n_c_t_i_o_n.  _I_n _t_h_i_s _c_a_s_e _w_e _c_r_e_a_t_e _a_n _u_n_n_a_m_e_d _a_r_r_a_y _a_n_d
_s_t_o_r_e _i_t _i_n _t_h_e _v_a_l_u_e _c_e_l_l _o_f _t_e_s_t_a_r_r.  _W_h_e_n  _w_e  _c_r_e_a_t_e  _a_n
_a_r_r_a_y  _t_o  _p_a_s_s  _t_o  _t_h_e  _P_a_s_c_a_l _p_r_o_g_r_a_m _w_e _w_i_l_l _u_s_e _a _n_a_m_e_d
_a_r_r_a_y _j_u_s_t _t_o _d_e_m_o_n_s_t_r_a_t_e _t_h_e _d_i_f_f_e_r_e_n_t _w_a_y _t_h_a_t  _n_a_m_e_d  _a_n_d
_u_n_n_a_m_e_d _a_r_r_a_y_s _a_r_e _c_r_e_a_t_e_d _a_n_d _a_c_c_e_s_s_e_d.
-> ((((sssseeeettttqqqq tttteeeessssttttaaaarrrrrrrr ((((aaaarrrrrrrraaaayyyy nnnniiiillll fffflllloooonnnnuuuummmm----bbbblllloooocccckkkk 2222))))))))
array[2]
-> ((((ssssttttoooorrrreeee ((((ffffuuuunnnnccccaaaallllllll tttteeeessssttttaaaarrrrrrrr 0000)))) 1111....222233334444))))
1.234
-> ((((ssssttttoooorrrreeee ((((ffffuuuunnnnccccaaaallllllll tttteeeessssttttaaaarrrrrrrr 1111)))) 5555....666677778888))))
5.678
-> ((((ccccffffoooooooo 333388885555 tttteeeessssttttaaaarrrrrrrr ((((hhhhuuuunnnnkkkk 11110000 11111111 11113333 11114444)))) ''''((((11115555 11116666 11117777))))))))
a: 385, b[0]: 1.234000, b[1]: 5.678000
 c (first): 10   c (second): 11
 ( 15 16 ... )
 3
_N_o_t_e _t_h_a_t _c_f_o_o _h_a_s _r_e_t_u_r_n_e_d _3 _a_s _i_t _s_h_o_u_l_d.  _I_t _a_l_s_o _h_a_d _t_h_e
_s_i_d_e  _e_f_f_e_c_t  _o_f  _c_h_a_n_g_i_n_g  _t_h_e _s_e_c_o_n_d _v_a_l_u_e _o_f _t_h_e _a_r_r_a_y _t_o
_3._1_4_1_5_9_2_6  _w_h_i_c_h _c_h_e_c_k _n_e_x_t.
-> ((((ffffuuuunnnnccccaaaallllllll tttteeeessssttttaaaarrrrrrrr 1111))))
3.1415926


_I_n _p_r_e_p_a_r_a_t_i_o_n _f_o_r _c_a_l_l_i_n_g _p_f_o_o _w_e _c_r_e_a_t_e _a_n _a_r_r_a_y.
-> ((((aaaarrrrrrrraaaayyyy tttteeeesssstttt fffflllloooonnnnuuuummmm----bbbblllloooocccckkkk 2222))))
array[2]
-> ((((ssssttttoooorrrreeee ((((tttteeeesssstttt 0000)))) 1111....222233334444))))
1.234
-> ((((ssssttttoooorrrreeee ((((tttteeeesssstttt 1111)))) 5555....666677778888))))
5.678
-> ((((ppppffffoooooooo 333388885555 ((((ggggeeeettttdddd ''''tttteeeesssstttt)))) ((((hhhhuuuunnnnkkkk 11110000 11111111 11113333 11114444)))) ''''((((11115555 11116666 11117777))))))))
 a:       385 b[0]:  1.23400000000000E+00 b[1]:  5.67800000000000E+00
 c (first):        10 c (second):        11
 (         15        16 ...)
3
-> ((((tttteeeesssstttt 1111))))
3.1415926

 _N_o_w _t_o _t_e_s_t _o_u_t _t_h_e _m_e_m_q'_s
-> ((((ccccmmmmeeeemmmmqqqq ''''aaaa ''''((((bbbb cccc aaaa dddd eeee ffff))))))))
(_a _d _e _f)
-> ((((ppppmmmmeeeemmmmqqqq ''''eeee ''''((((aaaa dddd ffff gggg aaaa xxxx))))))))
_n_i_l
____________________________________________________________


                                     Printed: March 23, 1982







FFFFuuuunnnnccccttttiiiioooonnnnssss aaaannnndddd MMMMaaaaccccrrrroooossss                                    8888----11112222


     The Fortran example will be much shorter since in  For-
tran   you  can't  follow  pointers  as  you  can  in  other
languages.  The Fortran function ffoo is given  three  argu-
ments:  a  fixnum, a fixnum-block array and a flonum.  These
arguments are printed out to verify that they  made  it  and
then the first value of the array is modified.  The function
returns a double precision value which  is  converted  to  a
flonum  by  lisp  and  printed.   Note  that the entry point
corresponding to the Fortran  function  ffoo  is  _ffoo_  as
opposed to the C and Pascal convention of preceding the name
with an underscore.

____________________________________________________________


% ccccaaaatttt cccchhhh8888aaaauuuuxxxxffff....ffff
        double precision function ffoo(a,b,c)
        integer a,b(10)
        double precision c
        print 2,a,b(1),b(2),c
2       format(' a=',i4,', b(1)=',i5,', b(2)=',i5,' c=',f6.4)
        b(1) = 22
        ffoo = 1.23456
        return
        end
% ffff77777777 ----cccc cccchhhh8888aaaauuuuxxxxffff....ffff
ch8auxf.f:
   ffoo:
0.9u 1.8s 0:12 22% 20+22k 54+48io 158pf+0w
% lllliiiisssspppp
Franz Lisp, Opus 33b
-> ((((ccccffffaaaassssllll ''''cccchhhh8888aaaauuuuxxxxffff....oooo ''''____ffffffffoooooooo____ ''''ffffffffoooooooo """"rrrreeeeaaaallll----ffffuuuunnnnccccttttiiiioooonnnn"""" """"----llllFFFF77777777 ----llllIIII77777777""""))))
/usr/lib/lisp/nld -N -A /usr/local/lisp -T 63000 ch8auxf.o -e _ffoo_
-o /tmp/Li11066.0 -lF77 -lI77 -lc
#6307c-"real-function"

-> ((((aaaarrrrrrrraaaayyyy tttteeeesssstttt ffffiiiixxxxnnnnuuuummmm----bbbblllloooocccckkkk 2222))))
array[2]
-> ((((ssssttttoooorrrreeee ((((tttteeeesssstttt 0000)))) 11110000))))
10
-> ((((ssssttttoooorrrreeee ((((tttteeeesssstttt 1111)))) 11111111))))
11
-> ((((ffffffffoooooooo 333388885555 ((((ggggeeeettttdddd ''''tttteeeesssstttt)))) 5555....666677778888))))
 a= 385, b(1)=   10, b(2)=   11 c=5.6780
1.234559893608093
-> ((((tttteeeesssstttt 0000))))
22

____________________________________________________________



9

9                                     Printed: March 23, 1982



