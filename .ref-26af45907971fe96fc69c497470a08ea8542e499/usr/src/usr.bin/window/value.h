/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)value.h	3.9 (Berkeley) %G%
 */

struct value {
	char v_type;
	union {
		int V_num;
		char *V_str;
	} v_un;
};
#define v_num	v_un.V_num
#define v_str	v_un.V_str

#define V_NUM	1
#define V_STR	2
#define V_ERR	3

#define val_free(v)	((v).v_type == V_STR ? str_free((v).v_str) : 0)
