/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)getent.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*ARGSUSED*/
getent(cp, name)
char *cp, *name;
{
	return(0);
}

#ifndef	__svr4__
/*ARGSUSED*/
char *
getstr(cp, cpp)
char *cp, **cpp;
{
	return(0);
}
#endif
