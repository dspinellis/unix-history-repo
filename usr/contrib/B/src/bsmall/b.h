/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b.h,v 1.1 84/06/28 00:48:39 timo Exp $ */

/* b.h: general */

#include <stdio.h>
#include <math.h>
#include <setjmp.h>

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
extern bool bugs;

/************************************************************************/
/*                                                                      */
/* Values                                                               */
/*                                                                      */
/* There are different modules for values, however all agree that       */
/* the first field of a value is its type, and the second its reference */
/* count. All other fields depend on the module.                        */
/*                                                                      */
/************************************************************************/

typedef struct{literal type; intlet refcnt, len; string *cts;} *value;

#define Dummy 0
#define Dumval ((value) Dummy)
#define Vnil ((value) 0)
#define Pnil ((value *) 0)

/* Types: */
#define Num '0'
#define Tex '"'
#define Com ','
#define Lis 'L'
#define Tab 'M'
#define ELT '}'
/* locations: */
#define Sim 'S'
#define Tri '@'
#define Tse '['
#define Glo 'g'
#define Per 'p'
/* units: */
#define How 'h'
#define For 'f'
#define Ref 'r'
#define Fun '+'
#define Prd 'i'

#define Type(v) ((v)->type)
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

typedef struct{env curnv; value *bndtgs;
	literal cntxt, resexp; value uname; literal utype;
	intlet cur_ilev, lino; txptr tx, ceol;} context;

#define Enil ((env) 0)

/* contexts: */
#define In_command 'c'
#define In_read '?'
#define In_unit 'u'
#define In_value 'v'
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

/* funprd.def */
#define Pre 'P'
#define Use 'U'

/************************************************************************/
/*                                                                      */
/* A function or predicate is modelled as a compound consisting of      */
/* (i)   two short integers for (L, H) priority                         */
/*           (relevant only for functions);                             */
/* (ii)  Zer/Mon/Dya for zero-, mon- or dyadicity;                      */
/* (iii) Pre/Use for pre- or user-definedness;                          */
/* (iv)  if Pre, a literal to switch on;                                */
/*       if Use, a pointer to the yield/test-unit text.                 */
/*                                                                      */
/************************************************************************/

typedef struct{envtab reftab; txptr fux, lux; bool filed;} how;
typedef struct{envtab reftab; txptr fux, lux; bool filed;
	intlet L, H; literal adic, def;} funprd;
/* The first four fields should have the same structure as those of 'hows' */

typedef struct{context con; txptr ftx;} formal;
typedef struct{txptr rp; intlet rlino;} ref;

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

typedef value basidf;
typedef struct{basidf i; env e;} simploc;
typedef struct{loc R; intlet B, C;} trimloc;
typedef struct{loc R; value K;} tbseloc;

/* Functions and Predicates */
typedef value fun;
typedef value prd;

char *malloc(), *realloc();
char *getenv();
