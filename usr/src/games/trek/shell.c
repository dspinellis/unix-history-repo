/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
static char sccsid[] = "@(#)shell.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
**  CALL THE SHELL
*/

shell()
{
	int		i;
	register int	pid;
	register int	sav2, sav3;

	if (!(pid = fork()))
	{
		setuid(getuid());
		nice(0);
		execl("/bin/csh", "-", 0);
		syserr("cannot execute /bin/csh");
	}
	sav2 = signal(2, 1);
	sav3 = signal(3, 1);
	while (wait(&i) != pid) ;
	signal(2, sav2);
	signal(3, sav3);
}
