/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
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
static char sccsid[] = "@(#)systemname.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

# include	"trek.h"

/*
**  RETRIEVE THE STARSYSTEM NAME
**
**	Very straightforward, this routine just gets the starsystem
**	name.  It returns zero if none in the specified quadrant
**	(which, by the way, is passed it).
**
**	This routine knows all about such things as distressed
**	starsystems, etc.
*/

char *systemname(q1)
struct quad	*q1;
{
	register struct quad	*q;
	register int		i;

	q = q1;

	i = q->qsystemname;
	if (i & Q_DISTRESSED)
		i = Event[i & Q_SYSTEM].systemname;

	i &= Q_SYSTEM;
	if (i == 0)
		return (0);
	return (Systemname[i]);
}
