/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kernel.h	8.2 (Berkeley) %G%
 */

/* Global variables for the kernel. */

/* 1.1 */
extern long hostid;
extern char hostname[MAXHOSTNAMELEN];
extern int hostnamelen;

/* 1.2 */
extern volatile struct timeval mono_time;
extern struct timeval boottime;
extern struct timeval runtime;
extern volatile struct timeval time;
extern struct timezone tz;			/* XXX */

extern int tick;			/* usec per tick (1000000 / hz) */
extern int hz;				/* system clock's frequency */
extern int stathz;			/* statistics clock's frequency */
extern int profhz;			/* profiling clock's frequency */
extern int lbolt;			/* once a second sleep address */
