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
 */

#ifndef lint
static char sccsid[] = "@(#)context.c	3.12 (Berkeley) 6/6/90";
#endif /* not lint */

#include "value.h"
#include "string.h"
#include "context.h"

/*
 * Context push/pop for nested command files.
 */

char *malloc();

cx_alloc()
{
	register struct context *xp;

	if (cx.x_type != 0) {
		xp = (struct context *)
			malloc((unsigned) sizeof (struct context));
		if (xp == 0)
			return -1;
		*xp = cx;
		cx.x_link = xp;
		cx.x_type = 0;
	}
	cx.x_erred = 0;
	cx.x_synerred = 0;
	cx.x_abort = 0;
	return 0;
}

cx_free()
{
	struct context *xp;

	if ((xp = cx.x_link) != 0) {
		cx = *xp;
		free((char *)xp);
	} else
		cx.x_type = 0;
}

cx_beginfile(filename)
char *filename;
{
	if (cx_alloc() < 0)
		return -1;
	cx.x_type = X_FILE;
	if ((cx.x_filename = str_cpy(filename)) == 0)
		goto bad;
	cx.x_fp = fopen(filename, "r");
	if (cx.x_fp == 0)
		goto bad;
	cx.x_bol = 1;
	cx.x_lineno = 0;
	cx.x_errwin = 0;
	cx.x_noerr = 0;
	return 0;
bad:
	if (cx.x_filename != 0)
		str_free(cx.x_filename);
	cx_free();
	return -1;
}

cx_beginbuf(buf, arg, narg)
char *buf;
struct value *arg;
int narg;
{
	if (cx_alloc() < 0)
		return -1;
	cx.x_type = X_BUF;
	cx.x_bufp = cx.x_buf = buf;
	cx.x_arg = arg;
	cx.x_narg = narg;
	return 0;
}

cx_end()
{
	switch (cx.x_type) {
	case X_BUF:
		break;
	case X_FILE:
		(void) fclose(cx.x_fp);
		str_free(cx.x_filename);
		break;
	}
	cx_free();
}
