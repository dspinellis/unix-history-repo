/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)iargc_.c	5.1	%G%
 */

/*
 * return the number of args on the command line following the command name
 *
 * calling sequence:
 *	nargs = iargc()
 * where:
 *	nargs will be set to the number of args
 */

extern int xargc;

long iargc_()
{
	return ((long)(xargc - 1));
}
