/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)label.c	8.1 (Berkeley) 6/4/93";
#endif /* not lint */

label_(s, len)
register char *s;
long len;
{
	char buf[260];
	register char *cp, *cend;

	cp = buf;
	cend = cp + (len < 255 ? len : 255 );
	while ( cp < cend ) 
		*cp++ = *s++;
	*cp = 0;
	label( buf );
}
