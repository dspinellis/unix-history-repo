/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)rand_.c	5.2	%G%
 *
 * Routines to return random values
 *
 * calling sequence:
 *	double precision d, drand
 *	i = irand(iflag)
 *	x = rand(iflag)
 *	d = drand(iflag)
 * where:
 *	If arg is 1, generator is restarted. If arg is 0, next value
 *	is returned. Any other arg is a new seed for the generator.
 *	Integer values will range from 0 thru 2147483647.
 *	Real values will range from 0.0 thru 1.0
 *	(see rand(3))
 */

#if	vax
#define	RANDMAX		2147483647
#else	vax
#if	pdp11
#define	RANDMAX		32767
#else	pdp11
	UNKNOWN MACHINE!
#endif	pdp11
#endif	vax

long irand_(iarg)
long *iarg;
{
	if (*iarg) srand((int)*iarg);
#if	pdp11
	return(( ((long)rand()) << 16) | rand());
#else	pdp11
	return( rand() );
#endif	pdp11
}

float rand_(iarg)
long *iarg;
{
	if (*iarg) srand((int)*iarg);
	return( (float)(rand())/(float)RANDMAX );
}

double drand_(iarg)
long *iarg;
{
	if (*iarg) srand((int)*iarg);
	return( (double)(rand())/(double)RANDMAX );
}
