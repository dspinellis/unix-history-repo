/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)var.h	3.8 (Berkeley) %G%
 */

struct var {
	struct var *r_left;
	struct var *r_right;
	char *r_name;
	struct value r_val;
};

struct var *var_set1();
struct var *var_setstr1();
struct var *var_setnum1();
struct var **var_lookup1();

#define var_set(n, v)		var_set1(&var_head, n, v)
#define var_setstr(n, s)	var_setstr1(&var_head, n, s)
#define var_setnum(n, i)	var_setnum1(&var_head, n, i)
#define var_unset(n)		var_unset1(&var_head, n)
#define var_lookup(n)		(*var_lookup1(&var_head, n))
#define var_walk(f, a)		var_walk1(var_head, f, a)

struct var *var_head;		/* secret, shhh */
