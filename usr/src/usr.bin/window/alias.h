/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)alias.h	3.8 (Berkeley) 6/6/90
 */

#define alias var
#define a_name r_name
#define a_buf r_val.v_str
#define a_flags r_val.v_type

	/* a_flags bits, must not interfere with v_type values */
#define A_INUSE		0x010	/* already inuse */

#define alias_set(n, s)		var_setstr1(&alias_head, n, s)
#define alias_walk(f, a)	var_walk1(alias_head, f, a)
#define alias_unset(n)		var_unset1(&alias_head, n)
#define alias_lookup(n)		(*var_lookup1(&alias_head, n))

struct var *alias_head;
