/*****************************************************************************
 * Copyright 1990, 1992 Free Software Foundation, Inc.
 *
 * This code was donated by Intel Corp.
 *
 * Intel hereby grants you permission to copy, modify, and 
 * distribute this software and its documentation.  Intel grants
 * this permission provided that the above copyright notice 
 * appears in all copies and that both the copyright notice and
 * this permission notice appear in supporting documentation.  In
 * addition, Intel grants this permission provided that you
 * prominently mark as not part of the original any modifications
 * made to this software or documentation, and that the name of 
 * Intel Corporation not be used in advertising or publicity 
 * pertaining to distribution of the software or the documentation 
 * without specific, written prior permission.  
 *
 * Intel Corporation does not warrant, guarantee or make any 
 * representations regarding the use of, or the results of the use
 * of, the software and documentation in terms of correctness, 
 * accuracy, reliability, currentness, or otherwise; and you rely
 * on the software, documentation and results solely at your own risk.
 *****************************************************************************/

static char rcsid[] =
	"$Id";


/******************************************************************************
 * send_break:
 *
 *	This function sends a a "break" to the specified tty.  This is useful
 *	if the tty is attached to an i960 board equipped with a break-detector
 *	circuit that causes a hard reset of the board.
 ******************************************************************************/

#include <stdio.h>
#include "ttycntl.h"

#ifdef USG
	send_break( fd )
	    int fd;	/* File descriptor of i960 tty */
	{
		TTY_FLUSH(fd);
		ioctl( fd, TCSBRK, 0 );
	}

#else	/* BSD Unix */

#	include <signal.h>
#	include <sys/time.h>

	static void
	alarm_handler()
	{
		return;
	}

	send_break( fd )
	    int fd;	/* File descriptor of i960 tty */
	{
		struct itimerval t;
		void (*old_alarm)();    /* Alarm signal handler on entry */

		old_alarm = signal( SIGALRM, alarm_handler );

		/* Set timer for 1/4 second break */
		t.it_value.tv_sec = 0;
		t.it_value.tv_usec = 250000;
		t.it_interval.tv_sec = t.it_interval.tv_usec = 0;

		/* Assert break for the duration of the timer */
		ioctl( fd, TIOCSBRK, 0 );
		setitimer( ITIMER_REAL, &t, 0 );
		sigpause(0);
		ioctl( fd, TIOCCBRK, 0 );

		signal( SIGALRM, old_alarm );
	}
#endif
