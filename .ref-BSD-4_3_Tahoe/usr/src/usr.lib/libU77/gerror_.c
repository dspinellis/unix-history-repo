/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)gerror_.c	5.1	6/7/85
 */

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

extern char *sys_errlist[];
extern int sys_nerr;
extern char *f_errlist[];
extern int f_nerr;

gerror_(s, len)
char *s; long len;
{
	char *mesg;

	if (errno >=0 && errno < sys_nerr)
		mesg = sys_errlist[errno];
	else if (errno >= F_ER && errno < (F_ER + f_nerr))
		mesg = f_errlist[errno - F_ER];
	else
		mesg = "unknown error number";
	b_char(mesg, s, len);
}
