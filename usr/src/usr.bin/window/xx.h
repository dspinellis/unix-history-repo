/*
 * Copyright (c) 1989 Regents of the University of California.
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
 *	@(#)xx.h	3.5 (Berkeley) 6/6/90
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

#define char_sep '\0'
