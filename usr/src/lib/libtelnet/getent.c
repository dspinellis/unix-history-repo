/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)getent.c	8.1 (Berkeley) %G%";
#endif /* not lint */

static char *area;

/*ARGSUSED*/
getent(cp, name)
char *cp, *name;
{
	char *dba[2];

	dba[0] = "/etc/gettytab";
	dba[1] = 0;
	return((cgetent(&area, dba, name) == 0) ? 1 : 0);
}

#ifndef	__svr4__
/*ARGSUSED*/
char *
getstr(id, cpp)
char *id, **cpp;
{
	char *answer;
	return((cgetstr(area, id, &answer) > 0) ? answer : 0);
}
#endif
