/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kernel.h	7.3 (Berkeley) %G%
 */

/*
 * Global variables for the kernel
 */

long	rmalloc();

/* 1.1 */
long	hostid;
char	hostname[MAXHOSTNAMELEN];
int	hostnamelen;

/* 1.2 */
struct	timeval boottime;
struct	timeval time;
struct	timezone tz;			/* XXX */
int	hz;
int	phz;				/* alternate clock's frequency */
int	tick;
int	lbolt;				/* awoken once a second */
int	realitexpire();

fixpt_t	averunnable[3];
#if defined(COMPAT_43) && (defined(vax) || defined(tahoe))
double	avenrun[3];
#endif /* COMPAT_43 */

#ifdef GPROF
extern	int profiling;
extern	char *s_lowpc;
extern	u_long s_textsize;
extern	u_short *kcount;
#endif
