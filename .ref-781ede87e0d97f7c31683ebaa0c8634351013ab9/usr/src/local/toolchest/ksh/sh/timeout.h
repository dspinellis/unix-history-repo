/*

 *      Copyright (c) 1984, 1985, 1986 AT&T
 *      All Rights Reserved

 *      THIS IS UNPUBLISHED PROPRIETARY SOURCE 
 *      CODE OF AT&T.
 *      The copyright notice above does not 
 *      evidence any actual or intended
 *      publication of such source code.

 */
/* @(#)timeout.h	1.1 */

/*
 *	UNIX shell
 *
 *	S. R. Bourne
 *	AT&T Bell Laboratories
 *
 */
#ifdef IH
#define TIMEOUT		7000	/* seconds elapsing before termination */
#else
#define TIMEOUT		0	/* seconds elapsing before termination */
#endif
#define TGRACE		60	/* grace period before termination */
				/* The time_warn message contains this number */
extern long	timeout;
extern MSG	timed_out;
extern MSG	time_warn;
