/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b.h,v 1.4 85/08/22 16:41:03 timo Exp $
*/

/* b.h: general */

#define MESS(nr, text) nr

#include <stdio.h>
#include <math.h>

#define Forward
#define Visible
#define Hidden static
#define Procedure

#define EQ ==
#define NE !=

/* The following are not intended as pseudo-encapsulation, */
/* but to emphasize intention. */

typedef char literal;
typedef char bool;
typedef char *txptr;
typedef char *string; /* Strings are always terminated with a char '\0'. */

#define Yes ((bool) 1)
#define No  ((bool) 0)
typedef short intlet;
extern bool bugs, testing;

/************************************************************************/
/*                                                                      */
/* Values                                                               */
/*                                                                      */
/* There are different modules for values, however all agree that       */
/* the first field of a value is its type, and the second its reference */
/* count. All other fields depend on the module.                        */
/*                                                                      */
/************************************************************************/

/*
 * "SMALL INTEGERS":
 *
 * When a "value" pointer has its low bit set, it is not a pointer.
 * By casting to int and shifting one bit to the right, it is converted
 * to its "int" value.  This can save a lot of heap space used for
 * small integers.
 * Sorry, you have to change this on machines with word rather than byte
 * addressing (maybe you can use the sign bit as tag).
 */

#define IsSmallInt(v) (((int)(v)) & 1)
#define SmallIntVal(v) (((int)(v) & ~1) / 2)
#define MkSmallInt(i) ((value)((i)*2 | 1))
	/* (Can't use << and >> because their effect on negative numbers
		is not defined.) */

#ifdef IBMPC
#define HEADER literal type, refcnt; intlet len
#else
#define HEADER literal type; intlet refcnt, len
#endif

typedef struct value {HEADER; string *cts;} *value;
typedef value parsetree;


#define Dummy NULL
#define Dumval ((value) Dummy)
#define Vnil ((value) NULL)
#define Pnil ((value *) NULL)
#define NilTree ((parsetree) NULL)

/* Types: */
#define Num '0'
#define Tex '"'
#define Com ','
#define Lis 'L'
#define Tab 'M'
#define ELT '}'
/* parsetree node */
#define Ptn 'T'
/* locations: */
#define Sim 'S'
#define Tri '@'
#define Tse '['
#define Per 'p'
/* units: */
#define How 'h'
#define For 'f'
#define Ref 'r'
#define Fun '+'
#define Prd 'i'

/* targets */
#define Tar 't'

#ifdef INTEGRATION
#define Nod 'N'
#define Pat 'P'
#endif INTEGRATION

#define Type(v) (IsSmallInt(v) ? Num : (v)->type)
#define Length(v) ((v)->len)
#define Refcnt(v) ((v)->refcnt)
#define Unique(v) ((v)->refcnt==1)

#define Overall for (k= 0; k < len; k++)

#define k_Over_len for (k= 0; k < len; k++)
#define Last(k)	(k == len-1)

#define Ats(v) ((value *)&((v)->cts))
#define Str(v) ((string)&((v)->cts)) /* only for use in part1 */

/* Environments and context */

typedef value envtab;
typedef struct ec{envtab tab; struct ec *inv_env;} envchain;
typedef envchain *env;

typedef struct {
	value uname;
	env curnv;
	value r_names, *bndtgs;
	literal cntxt, resexp;
	parsetree cur_line;
	value cur_lino;
} context;

#define Enil ((env) NULL)

/* contexts: */
#define In_command 'c'
#define In_read '?'
#define In_unit 'u'
#define In_edval 'e'
#define In_tarval 't'
#define In_formal 'f'
#define In_prmnv 'p'

/* results */
#define Ret 'V'
#define Rep '+'
#define Voi ' '

/* adicity */
#define Zer '0'
#define Mon '1'
#define Dya '2'

/************************************************************************/
/*                                                                      */
/* A function or predicate is modelled as a compound consisting of      */
/* (i)  Zer/Mon/Dya for zero-, mon- or dyadicity;                      */
/* (ii) If a predefined function, an identifying number, otherwise -1  */
/* (iii)  If a user-defined function/predicate, its parse-tree           */
/*                                                                      */
/************************************************************************/

typedef struct{parsetree unit; bool unparsed, filed; parsetree code;} how;
typedef struct{parsetree unit; bool unparsed, filed; parsetree code;
	literal adic; intlet pre;} funprd;
/* The first four fields of hows and funprds must be the same. */
#define Use (-1) /* funprd.pre==Use for user-defined funprds */

typedef struct{context con; parsetree fp;} formal;
typedef struct{parsetree rp;} ref;
typedef struct{parsetree val;} per;

/************************************************************************/
/*                                                                      */
/* Locations                                                            */
/*                                                                      */
/* A simple location is modelled as a pair basic-identifier and         */
/*     environment, where a basic-identifier is modelled as a text      */
/*     and an environment as a pointer to a pair (T, E), where T is a   */
/*     table with basic-identifiers as keys and content values as       */
/*     associates, and E is the invoking environment or nil.            */
/*                                                                      */
/* A trimmed-text location is modelled as a triple (R, B, C).           */
/*                                                                      */
/* A compound location is modelled as a compound whose fields are       */
/*     locations, rather than values.                                   */
/*                                                                      */
/* A table-selection location is modelled as a pair (R, K).             */
/*                                                                      */
/************************************************************************/

typedef value loc;
#define Lnil ((loc) NULL)

typedef value basidf;
typedef struct{basidf i; env e;} simploc;
typedef struct{loc R; value B, C;} trimloc;
typedef struct{loc R; value K;} tbseloc;

/* Functions and Predicates */
typedef value fun;
typedef value prd;

char *malloc(), *realloc();
char *getenv();
