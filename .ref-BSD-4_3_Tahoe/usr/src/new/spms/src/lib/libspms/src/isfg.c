/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * isfg() determines whether a process is a foreground job and standard
 * input stream is connected to a terminal. Returns integer YES if it is,
 * otherwise NO.
 */
#include <stdio.h>
#include <signal.h>
#include <sys/ioctl.h>
#include "system.h"
#include "yesno.h"

isfg()
{
	int isfg = NO;			/* is foreground? */
#ifdef V4BSD
	int isatty();			/* is stream a terminal? */
	int tpgrp;			/* terminal process group */
	int (*tstpstat)();		/* stop signal status */
	int (*ttinstat)();		/* background read status */
	int (*ttoustat)();		/* background write status */

	tstpstat = signal(SIGTSTP, SIG_IGN);
	ttinstat = signal(SIGTTIN, SIG_IGN);
	ttoustat = signal(SIGTTOU, SIG_IGN);
	if (isatty(fileno(stdin)))
		{
		ioctl(fileno(stdin), TIOCGPGRP, &tpgrp);
		if (tpgrp == getpgrp(0))
			isfg = YES;
		}
	signal(SIGTSTP, tstpstat);
	signal(SIGTTIN, ttinstat);
	signal(SIGTTOU, ttoustat);
#else
	int isatty();			/* is stream a terminal? */

	if (isatty(fileno(stdin)))
		isfg = YES;
#endif
	return(isfg);
}
