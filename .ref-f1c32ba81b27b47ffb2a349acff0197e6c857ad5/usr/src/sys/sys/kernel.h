/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kernel.h	7.4 (Berkeley) %G%
 */

/* Global variables for the kernel. */
long rmalloc();

/* 1.1 */
long hostid;
char hostname[MAXHOSTNAMELEN];
int hostnamelen;

/* 1.2 */
struct timeval boottime;
struct timeval time;
struct timezone tz;			/* XXX */

int hz;					/* clock frequency */
int phz;				/* alternate clock's frequency */
int tick;
int lbolt;				/* once a second sleep address */
int realitexpire();

fixpt_t	averunnable[3];
#if defined(COMPAT_43) && (defined(vax) || defined(tahoe))
double	avenrun[3];
#endif /* COMPAT_43 */

#ifdef GPROF
u_long s_textsize;
int profiling;
u_short *kcount;
char *s_lowpc;
#endif
