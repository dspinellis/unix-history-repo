/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)setegid.c	5.1 (Berkeley) %G%";
#endif not lint

setegid(egid)
	int egid;
{

	return (setregid(-1, egid));
}
