/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3sem.h,v 1.4 85/08/22 16:44:32 timo Exp $
*/

/* bsem.h: semantics */

typedef bool outcome;
#define Und ((bool) '?') /* absence of REPORTed outcome */

/* Locations */
loc local_loc();
loc global_loc();
value content();
/* Procedure check_location(); */
loc trim_loc();
loc tbsel_loc();
/* Procedure put(); */
/* Procedure putcheck(); */
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
/* Procedure initfpr(); */

/* Elaboration: */
value pre_fun();
outcome pre_prop();

/* B units */

extern value resval;
extern bool terminated;

/* Procedure udfpr(); */
bool ref_com();
/* Procedure ref_et(); */
