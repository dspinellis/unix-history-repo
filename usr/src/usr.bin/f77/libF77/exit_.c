/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)exit_.c	5.2 (Berkeley) %G%";
#endif /* not lint */

exit_(n)
long *n;
{
	int	exitcode;

#if	vax
	if (nargs() == 0)
		exitcode = 0;
	else
#endif	vax
		exitcode = *n;	/* take any segmentation violation here */
	f_exit();
	_cleanup();
	exit(exitcode);
}
