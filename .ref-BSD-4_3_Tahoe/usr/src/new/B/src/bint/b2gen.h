/* Copyright (c) Stichting Mathematisch Centrum, Amsterdam, 1985. */

/*
  $Header: b2gen.h,v 1.4 85/08/22 16:42:31 timo Exp $
*/

#define Is_node(t) ((t) != NilTree && Is_parsetree(t))

extern int nextvarnumber; /* Counts local targets (including formals) */
extern value formals, locals, globals, mysteries, refinements;
extern string gentab[];

#define NTYPES (FORMAL+1)

struct state {
	parsetree h_last;
	parsetree *h_wanthere;
	parsetree h_bpchain;
};

#define f_expr(p) fix(p, 'v') /* "evaluate" */
#define f_targ(p) fix(p, 'l') /* "locate" */

value copydef();
bool modify_tag();
