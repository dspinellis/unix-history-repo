/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kernel.h	7.10 (Berkeley) %G%
 */

/* Global variables for the kernel. */
long rmalloc();

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

#if defined(COMPAT_43) && (defined(vax) || defined(tahoe))
double	avenrun[3];
#endif

#ifdef GPROF
extern int profiling;
extern u_short *kcount;
#endif
