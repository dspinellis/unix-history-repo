/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * isintract() determines whether a process is a foreground job, and
 * standard input and output streams are connected to a terminal.
 * Returns integer YES if it is, otherwise NO.
 */
#include <stdio.h>
#include <signal.h>
#include <sys/ioctl.h>
#include "system.h"
#include "yesno.h"

isintract()
{
	int intract = NO;		/* is interactive? */
#ifdef V4BSD
	int isatty();			/* is stream a terminal? */
	int tpgrp;			/* terminal process group */
	int (*tstpstat)();		/* stop signal status */
	int (*ttinstat)();		/* background read status */
	int (*ttoustat)();		/* background write status */

	tstpstat = signal(SIGTSTP, SIG_IGN);
	ttinstat = signal(SIGTTIN, SIG_IGN);
	ttoustat = signal(SIGTTOU, SIG_IGN);
	if (isatty(fileno(stdin)) && isatty(fileno(stdout)))
		{
		ioctl(fileno(stdin), TIOCGPGRP, &tpgrp);
		if (tpgrp == getpgrp(0))
			intract = YES;
		}
	signal(SIGTSTP, tstpstat);
	signal(SIGTTIN, ttinstat);
	signal(SIGTTOU, ttoustat);
#else
	int isatty();			/* is stream a terminal? */

	if (isatty(fileno(stdin)) && isatty(fileno(stdout)))
		intract = YES;
#endif
	return(intract);
}
