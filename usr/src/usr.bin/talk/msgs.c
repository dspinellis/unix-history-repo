/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)msgs.c	5.1 (Berkeley) %G%";
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
#define LONG_TIME 100000

char	*current_state;
int	current_line = 0;

static	struct itimerval itimer;
static	struct timeval wait = { MSG_INTERVAL , 0};
static	struct timeval undo = { LONG_TIME, 0};
	
disp_msg()
{

	message(current_state);
}

start_msgs()
{

	message(current_state);
	signal(SIGALRM, disp_msg);
	itimer.it_value = itimer.it_interval = wait;
	setitimer(ITIMER_REAL, &itimer, (struct timerval *)0);
}

end_msgs()
{

	signal(SIGALRM, SIG_IGN);
	timerclear(&itimer.it_value);
	timerclear(&itimer.it_interval);
	setitimer(ITIMER_REAL, &itimer, (struct timerval *)0);
}
