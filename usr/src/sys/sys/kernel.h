/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kernel.h	7.6 (Berkeley) %G%
 */

/* Global variables for the kernel. */
long rmalloc();

/* 1.1 */
extern long hostid;
extern char hostname[MAXHOSTNAMELEN];
extern int hostnamelen;

/* 1.2 */
extern struct timeval mono_time;
extern struct timeval boottime;
extern struct timeval time;
extern struct timezone tz;			/* XXX */

extern int hz;				/* clock frequency */
extern int phz;				/* alternate clock's frequency */
extern int tick;			/* usec per tick */
extern int profhz;			/* profiling clock's frequency */
extern int lbolt;			/* once a second sleep address */
extern int realitexpire();

struct clockinfo {
	int hz;		/* clock frequency */
	int tick;	/* micro-seconds per hz tick */
	int phz;	/* alternate clock frequency */
	int profhz;	/* profiling clock frequency */
};

struct loadavg {
	fixpt_t ldavg[3];
	long fscale;
};
extern struct loadavg averunnable;
#if defined(COMPAT_43) && (defined(vax) || defined(tahoe))
double	avenrun[3];
#endif /* COMPAT_43 */

#ifdef GPROF
extern u_long s_textsize;
extern int profiling;
extern u_short *kcount;
extern char *s_lowpc;
#endif
