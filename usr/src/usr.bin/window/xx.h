/*
 * Copyright (c) 1989 Regents of the University of California.
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
 *	@(#)xx.h	3.2 (Berkeley) %G%
 */

struct xx {
	enum { xc_move, xc_scroll, xc_inschar, xc_insspace, xc_delchar,
		xc_clear, xc_clreos, xc_clreol, xc_write } cmd;
	int arg0;
	int arg1;
	int arg2;
	int arg3;
	char *buf;
	struct xx *link;
};

struct xx *xxalloc();

struct xx *xx_head, *xx_tail;
struct xx *xx_freelist;

char *xxbuf, *xxbufp, *xxbufe;
int xxbufsize;
