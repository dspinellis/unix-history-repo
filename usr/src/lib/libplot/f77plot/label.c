/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)label.c	5.2 (Berkeley) %G%";
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
