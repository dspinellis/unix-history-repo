/*
 * Copyright (c) 1983 Regents of the University of California.
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
static char sccsid[] = "@(#)msgs.c	5.3 (Berkeley) %G%";
#endif /* not lint */

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

disp_msg()
{

	message(current_state);
}

start_msgs()
{
	struct itimerval itimer;

	message(current_state);
	signal(SIGALRM, disp_msg);
	itimer.it_value.tv_sec = itimer.it_interval.tv_sec = MSG_INTERVAL;
	itimer.it_value.tv_usec = itimer.it_interval.tv_usec = 0;
	setitimer(ITIMER_REAL, &itimer, (struct timerval *)0);
}

end_msgs()
{
	struct itimerval itimer;

	timerclear(&itimer.it_value);
	timerclear(&itimer.it_interval);
	setitimer(ITIMER_REAL, &itimer, (struct timerval *)0);
	signal(SIGALRM, SIG_DFL);
}
