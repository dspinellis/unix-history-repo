/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)conv.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/types.h>
#include <ctype.h>
#include "hexdump.h"

conv_c(pr, p)
	PR *pr;
	u_char *p;
{
	extern int deprecated;
	char buf[10], *str;

	switch(*p) {
	case '\0':
		str = "\\0";
		goto strpr;
	/* case '\a': */
	case '\007':
		if (deprecated)		/* od didn't know about \a */
			break;
		str = "\\a";
		goto strpr;
	case '\b':
		str = "\\b";
		goto strpr;
	case '\f':
		str = "\\f";
		goto strpr;
	case '\n':
		str = "\\n";
		goto strpr;
	case '\r':
		str = "\\r";
		goto strpr;
	case '\t':
		str = "\\t";
		goto strpr;
	case '\v':
		if (deprecated)
			break;
		str = "\\v";
		goto strpr;
	default:
		break;
	}
	if (isprint(*p)) {
		*pr->cchar = 'c';
		(void)printf(pr->fmt, *p);
	} else {
		(void)sprintf(str = buf, "%03o", (int)*p);
strpr:		*pr->cchar = 's';
		(void)printf(pr->fmt, str);
	}
}

conv_u(pr, p)
	PR *pr;
	u_char *p;
{
	extern int deprecated;
	static char *list[] = {
		"nul", "soh", "stx", "etx", "eot", "enq", "ack", "bel",
		 "bs",  "ht",  "lf",  "vt",  "ff",  "cr",  "so",  "si",
		"dle", "dcl", "dc2", "dc3", "dc4", "nak", "syn", "etb",
		"can",  "em", "sub", "esc",  "fs",  "gs",  "rs",  "us",
	};

						/* od used nl, not lf */
	if (*p <= 0x1f) {
		*pr->cchar = 's';
		if (deprecated && *p == 0x0a)
			(void)printf(pr->fmt, "nl");
		else
			(void)printf(pr->fmt, list[*p]);
	} else if (*p == 0x7f) {
		*pr->cchar = 's';
		(void)printf(pr->fmt, "del");
	} else if (deprecated && *p == 0x20) {	/* od replace space with sp */
		*pr->cchar = 's';
		(void)printf(pr->fmt, " sp");
	} else if (isprint(*p)) {
		*pr->cchar = 'c';
		(void)printf(pr->fmt, *p);
	} else {
		*pr->cchar = 'x';
		(void)printf(pr->fmt, (int)*p);
	}
}
