/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b3env.h,v 1.4 85/08/22 16:43:37 timo Exp $
*/

/* environments and context */

value* lookup();
bool in_env();
value* envassoc();

extern env curnv; extern value r_names, *bndtgs;
extern literal cntxt, resexp; extern value uname;
extern intlet lino;
extern intlet f_lino;

extern context read_context;

extern envtab prmnvtab;
extern envchain prmnvchain;
extern env prmnv;

/* Procedure sv_context(); */
/* Procedure set_context(); */
/* Procedure initenv(); */
/* Procedure re_env(); */
/* Procedure setprmnv(); */
/* Procedure extbnd_tags(); */
/* Procedure e_replace(); */
/* Procedure e_delete(); */
