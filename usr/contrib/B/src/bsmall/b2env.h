/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1984. */
/* $Header: b2env.h,v 1.1 84/06/28 00:48:44 timo Exp $ */

/* environments and context */

/* Procedure sv_context(); */
/* Procedure set_context(); */
/* Procedure initenv(); */
/* Procedure re_env(); */
/* Procedure setprmnv(); */
/* Procedure extbnd_tags(); */
/* Procedure restore_env(); */
value* lookup();
/* Procedure e_replace(); */
/* Procedure e_delete(); */
bool in_env();
value* envassoc();

extern env curnv; extern value *bndtgs;
extern literal cntxt, resexp; extern value uname; extern literal utype;
extern intlet cur_ilev, lino; extern txptr tx, ceol;

extern context read_context;
extern context how_context;
