/*-
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)open.c	8.1 (Berkeley) %G%";
#endif /* not lint */

int xnow;
int ynow;
float boty = 0.;
float botx = 0.;
float oboty = 0.;
float obotx = 0.;
float scalex = 1.;
float scaley = 1.;
int vti = -1;

openvt ()
{
		vti = open("/dev/vt0",1);
		return;
}
openpl()
{
	vti = open("/dev/vt0",1);
	return;
}
