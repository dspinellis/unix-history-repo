/*
**  Sendmail
**  Copyright (c) 1983  Eric P. Allman
**  Berkeley, California
**
**  Copyright (c) 1983 Regents of the University of California.
**  All rights reserved.  The Berkeley software License Agreement
**  specifies the terms and conditions for redistribution.
**
**	@(#)trace.h	5.1 (Berkeley) 7/14/85
*/

/*
**  Trace Package.
*/

typedef u_char	*TRACEV;

extern TRACEV	tTvect;			/* current trace vector */

# ifndef tTVECT
# define tTVECT		tTvect
# endif tTVECT

# define tTf(flag, level)	(tTVECT[flag] >= level)
# define tTlevel(flag)		(tTVECT[flag])
