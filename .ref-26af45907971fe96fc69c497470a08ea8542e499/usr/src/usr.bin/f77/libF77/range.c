/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)range.c	5.5 (Berkeley) %G%";
#endif /* not lint */

/*
 * routines to return extreme values
 * VERY MACHINE DEPENDENT
 */

union fi
{	float	f;
	long	i;
} ;

union dj
{	double	d;
	long	j[2];
} ;

float
flmax_()
{
	union fi n;
#if	pdp11
	n.i = 0x7fffffffL;
#else	pdp11
#if	vax
	n.i = 0xffff7fff;
#else	vax
#if	tahoe
	n.i = 0x7fffffffL;
#else	tahoe
#if	hp300
	n.i = 0x7fffffffL;
#else	hp300
	UNKNOWN MACHINE!
#endif	hp300
#endif	tahoe
#endif	vax
#endif	pdp11
	return(n.f);
}

double
dflmax_()
{
	union dj n;
#if	pdp11
	n.j[0] = 0x7fffffffL;
	n.j[1] = 0xffffffffL;
#else	pdp11
#if	vax
	n.j[0] = 0xffff7fff;
	n.j[1] = 0xffffffff;
#else	vax
#if	tahoe
	n.j[0] = 0x7fffffffL;
	n.j[1] = 0xffffffffL;
#else	tahoe
#if	hp300
	n.j[0] = 0x7fffffffL;
	n.j[1] = 0xffffffffL;
#else	hp300
	UNKNOWN MACHINE!
#endif	hp300
#endif	tahoe
#endif	vax
#endif	pdp11
	return(n.d);
}

float
flmin_()
{
	union fi n;
#if	pdp11
	n.i = 0x00800000L;
#else	pdp11
#if	vax
	n.i = 0x00000080;
#else	vax
#if	tahoe
	n.i = 0x00800000L;
#else	tahoe
#if	hp300
	n.i = 0x00800000L;
#else	hp300
	UNKNOWN MACHINE!
#endif	hp300
#endif	tahoe
#endif	vax
#endif	pdp11
	return(n.f);
}

double
dflmin_()
{
	union dj n;
#if	pdp11
	n.j[0] = 0x00800000L;
	n.j[1] = 0;
#else	pdp11
#if	vax
	n.j[0] = 0x00000080;
	n.j[1] = 0;
#else	vax
#if	tahoe
	n.j[0] = 0x00800000L;
	n.j[1] = 0;
#else	tahoe
#if	hp300
	n.j[0] = 0x00800000L;
	n.j[1] = 0;
#else	hp300
	UNKNOWN MACHINE!
#endif	hp300
#endif	tahoe
#endif	vax
#endif	pdp11
	return(n.d);
}

long int
inmax_()
{
	return(0x7fffffffL);
}


float
ffrac_()
{
	union fi n;
#if	pdp11
	n.i = 0x35000000L;
#else	pdp11
#if	vax
	n.i = 0x00003500;
#else	vax
#if	tahoe
	n.i = 0x35000000L;
#else	tahoe
#if	hp300
	n.i = 0x35000000L;
#else	hp300
	UNKNOWN MACHINE!
#endif	hp300
#endif	tahoe
#endif	vax
#endif	pdp11
	return(n.f);
}

double
dffrac_()
{
	union dj n;
#if	pdp11
	n.j[0] = 0x25000000L;
	n.j[1] = 0;
#else	pdp11
#if	vax
	n.j[0] = 0x00002500;
	n.j[1] = 0;
#else	vax
#if	tahoe
	n.j[0] = 0x25000000L;
	n.j[1] = 0;
#else	tahoe
#if	hp300
	n.j[0] = 0x25000000L;
	n.j[1] = 0;
#else	hp300
	UNKNOWN MACHINE!
#endif	hp300
#endif	tahoe
#endif	vax
#endif	pdp11
	return(n.d);
}
