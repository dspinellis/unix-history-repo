/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)exit_.c	5.1	6/7/85
 */


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
