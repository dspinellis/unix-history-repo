/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)getent.c	8.2 (Berkeley) %G%";
#endif /* not lint */

static char *area;

/*ARGSUSED*/
getent(cp, name)
char *cp, *name;
{
#ifdef	HAS_CGETENT
	char *dba[2];

	dba[0] = "/etc/gettytab";
	dba[1] = 0;
	return((cgetent(&area, dba, name) == 0) ? 1 : 0);
#else
	return(0);
#endif
}

#ifndef	SOLARIS
/*ARGSUSED*/
char *
getstr(id, cpp)
char *id, **cpp;
{
# ifdef	HAS_CGETENT
	char *answer;
	return((cgetstr(area, id, &answer) > 0) ? answer : 0);
# else
	return(0);
# endif
}
#endif
