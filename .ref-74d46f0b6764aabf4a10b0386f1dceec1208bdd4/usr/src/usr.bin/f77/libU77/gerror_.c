/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)gerror_.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * Return a standard error message in a character string.
 *
 * calling sequence:
 *	call gerror (string)
 * or
 *	character*20 gerror, string
 *	string = gerror()
 * where:
 *	'string' will receive the standard error message
 */

#include	<stdio.h>
#include	"../libI77/f_errno.h"

extern int sys_nerr;
extern char *f_errlist[];
extern int f_nerr;

gerror_(s, len)
char *s; long len;
{
	char *mesg;
	char *strerror();

	if (errno >=0 && errno < sys_nerr)
		mesg = strerror(errno);
	else if (errno >= F_ER && errno < (F_ER + f_nerr))
		mesg = f_errlist[errno - F_ER];
	else
		mesg = "unknown error number";
	b_char(mesg, s, len);
}
