/*-
 * Copyright (c) 1983, 1985
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)msgs.c	5.1 (Berkeley) 6/6/85";
#endif not lint

/* 
 * A package to display what is happening every MSG_INTERVAL seconds
 * if we are slow connecting.
 */

#include <signal.h>
#include <stdio.h>
#include <sys/time.h>
#include "talk.h"

#define MSG_INTERVAL 4

char	*current_state;
int	current_line = 0;

static	struct itimerval itimer;
static	struct timeval wait = { MSG_INTERVAL , 0};

void
disp_msg()
{

	message(current_state);
}

void
start_msgs()
{

	message(current_state);
	signal(SIGALRM, disp_msg);
	itimer.it_value = itimer.it_interval = wait;
	setitimer(ITIMER_REAL, &itimer, (struct itimerval *)0);
}

void
end_msgs()
{

	signal(SIGALRM, SIG_IGN);
	timerclear(&itimer.it_value);
	timerclear(&itimer.it_interval);
	setitimer(ITIMER_REAL, &itimer, (struct itimerval *)0);
}
