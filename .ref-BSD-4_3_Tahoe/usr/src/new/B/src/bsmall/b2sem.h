/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2sem.h,v 1.1 84/06/28 00:48:50 timo Exp $ */

/* bsem.h: semantics */

typedef bool outcome;
#define Succ Yes
#define Fail No
#define Und ((bool) '?') /* absence of REPORTed outcome */

#define Ifxeq(v) (xeq ? (v) : Dumval)

extern bool xeq;

extern envtab prmnvtab;
extern envchain prmnvchain;
extern env prmnv;

/* Locations */
loc local_loc();
loc global_loc();

value content();
/* Procedure check_location(); */
loc trim_loc();
loc tbsel_loc();
/* Procedure put(); */
/* Procedure l_delete(); */
/* Procedure l_insert(); */
/* Procedure l_remove(); */
/* Procedure choose(); */
/* Procedure draw(); */
/* Procedure bind(); */

/* Functions and Predicates */
bool is_zerfun();
bool is_monfun();
bool is_dyafun();
bool is_zerprd();
bool is_monprd();
bool is_dyaprd();
value montor();
value dyator();
value formula();
outcome proposition();
/* Procedure initfprs(); */

/* Expressions: */
value expr();
value obasexpr();
value tag();
value constant();
value conversion();
/* Procedure trimbc(); */
/* Procedure inittors(); */

/* Targets: */
loc targ();
loc bastarg();

/* Tests: */
outcome test();
bool relop();
outcome comparison();

/* Commands: */
/* Procedure command(); */
/* Procedure comm_suite(); */
/* Procedure initcom(); */

/* B units */

extern value resval; extern outcome resout;
extern bool terminated;

bool unit();
/* Procedure getunit(); */
/* Procedure ytu_heading(); */
bool udc();
/* Procedure udfpr(); */
bool ref_com();
/* Procedure ref_et(); */
value eva_formal();
loc loc_formal();
/* Procedure inithow(); */
