/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)getenv_.c	5.2 (Berkeley) 4/12/91";
#endif /* not lint */

/*
 * return environment variables
 *
 * calling sequence:
 *	character*20 evar
 *	call getenv (ENV_NAME, evar)
 * where:
 *	ENV_NAME is the name of an environment variable
 *	evar is a character variable which will receive
 *		the current value of ENV_NAME,
 *		or all blanks if ENV_NAME is not defined
 */

extern char **environ;

getenv_(fname, value, flen, vlen)
char *value, *fname;
long int vlen, flen;
{
	register char *ep, *fp;
	register char **env = environ;
	int i;

	while (ep = *env++) {
		for (fp=fname, i=0; i <= flen; i++) {
			if (i == flen || *fp == ' ') {
				if (*ep++ == '=') {
					b_char(ep, value, vlen);
					return(0);
				}
				else break;
			}
			else if (*ep++ != *fp++) break;
		}
	}
	b_char(" ", value, vlen);
	return(0);
}
