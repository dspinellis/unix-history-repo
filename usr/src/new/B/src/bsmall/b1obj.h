/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b1obj.h,v 1.1 84/06/28 00:48:43 timo Exp $ */

/* B values, locations, environments: the B abstract machine */

/****************************** general ******************************/

typedef intlet relation; /* < 0, == 0, > 0 */
relation compare();

#define Is_text(v) (Type(v) == Tex)
#define Is_number(v) (Type(v) == Num)
#define Is_compound(v) (Type(v) == Com)
#define Is_list(v) (Type(v) == Lis || Type(v) == ELT)
#define Is_table(v) (Type(v) == Tab || Type(v) == ELT)
#define Is_tlt(v) (Type(v)==Tex || Type(v)==Lis || Type(v)==Tab || Type(v)==ELT)
#define Is_ELT(v) (Type(v) == ELT)

#define Is_simploc(v) (Type(v) == Sim)
#define Is_tbseloc(v) (Type(v) == Tse)
#define Is_trimloc(v) (Type(v) == Tri)
#define Is_refinement(v) (Type(v) == Ref)
#define Is_formal(v) (Type(v) == For)
#define Is_shared(v) (Type(v) == Glo)
#define Is_filed(v) (Type(v) == Per)
#define Is_function(v) (Type(v) == Fun)
#define Is_predicate(v) (Type(v) == Prd)
#define Is_howto(v) (Type(v) == How)

value grab_num();
value regrab_num();
value grab_rat();
value grab_approx();
value grab_tex();
value grab_com();
value grab_elt();
value grab_lis();
value grab_tab();
value grab_sim();
value grab_tri();
value grab_tse();
value grab_how();
value grab_for();
value grab_glo();
value grab_per();
value grab_fun();
value grab_prd();
value grab_ref();

value copy();
/* Procedure release(); */
/* Procedure uniql(); */
/* Procedure uniq_assoc(); */
double hash();

/****************************** Texts ******************************/
string strcpy(), strncpy(), strcat(), sprintf(), index();

bool character();

value mk_text();
char charval();
string strval();

value concat();
value behead();
value curtail();
value trim();
value repeat();

value adjleft();
value centre();
value adjright();

value convert();

/****************************** Numbers ******************************/

/* Predicates */
bool integral();	    /* is the value an integer? */
bool large();	    /* can a number be represented by a C int? */

/* Constants */
extern value zero, one;

/* Conversion of abstract values to concrete objects */
double numval();     /* numeric value of any number */
int intval();        /* numeric value of integral number */
intlet propintlet(); /* converts int to intlet */
string convnum();    /* character string approximation of any number */
int numcomp();       /* Comparison of two numbers: yields -1, 0 or 1 */
double numhash();    /* Hashes any abstract number to a 'double' */

/* Conversion of concrete objects to abstract numbers */
value numconst();    /* string argument */
value mk_integer();  /* int argument */

/* Functions on numbers */
value sum();
value diff();
value negated();
value prod();
value quot();
value modulo();
value floorf();
value ceilf();
value round1();
value round2();
value mod();
value power();
value absval();
value signum();
value numerator();
value denominator();
value approximate();
value random();
value root1();
value sin1();
value cos1();
value tan1();
value atn1();
value exp1();
value log1();
value root2();
value atn2();
value log2();
value pi();
value e();

/****************************** Compounds ******************************/
#define Nfields(c) Length(c)
#define Field(c, i) ((Ats(c)+(i)))
#define k_Overfields for (k= 0; k < len; k++)
#define Lastfield(k) ((k) == len-1)

#define mk_compound(len) grab_com(len)

value* field();
/* Procedure put_in_field(); */

/****************************** Lists ******************************/
value mk_numrange();
value mk_charrange();

/* Procedurre insert(); */
/* Procedure remove(); */

/****************************** Tables ******************************/

value keys();
bool in_keys();
value associate();

/* Procedure replace(); */
/* Procedure delete(); */

value* adrassoc();
value* key();
value* assoc();

/****************************** Texts, Lists, and Tables *******************/
value mk_elt();

bool in();

value size();
value size2();
value min1();
value min2();
value max1();
value max2();
value th_of();
value thof();

int length(); /* The same as size, temporary until part2 is written in B */
bool empty(); /* whether #v=0: also temporary */

/****************************** Other kinds of value ************************/

#define Simploc(l) ((simploc *)Ats(l))
#define Tbseloc(l) ((tbseloc *)Ats(l))
#define Trimloc(l) ((trimloc *)Ats(l))
#define Funprd(f)  ((funprd *)Ats(f))
#define How_to(u)  ((how *)Ats(u))
#define Formal(p)  ((formal *)Ats(f))
#define Refinement(r) ((ref *)Ats(r))

loc mk_simploc();
loc mk_trimloc();
loc mk_tbseloc();

value mk_per();
fun mk_fun();
prd mk_prd();
value mk_how();
value mk_ref();

