#ifndef lint
static char sccsid[] = "@(#)makpipe.c	4.3	(Berkeley)	5/15/86";
#endif not lint

#include "stdio.h"

makpipe()
{
	int f[2];

	pipe(f);
	if (fork()==0) {
		close(f[1]);
		close(0);
		dup(f[0]);
		close(f[0]);
#if BSD4_2
		execl ("/bin/sh", "sh", "-i", 0);
		execl ("/usr/ucb/bin/sh", "sh", "-i", 0);
#else
		execlp("/bin/csh", "csh", "-if", 0);
		/*execl ("/usr/ucb/bin/csh", "csh", "-if", 0);*/
#endif
		write(2, "Exec error\n", 11);
	}
	close(f[0]);
	sleep(2);	/* so shell won't eat up too much input */
	return(f[1]);
}
