/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)alias.h	3.6 (Berkeley) %G%
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
