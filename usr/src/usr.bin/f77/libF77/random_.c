/*
char id_random[] = "@(#)random_.c	1.1";
 *
 * Routines to return random values
 *
 * calling sequence:
 *	double precision d, drandm
 *	i = irandm(iflag)
 *	x = random(iflag)
 *	d = drandm(iflag)
 * where:
 *	If arg is nonzero, generator is restarted and value is returned.
 *	If arg is 0, next value is returned.
 *	Integer values will range from 0 thru 2147483647 (see random(3)).
 *	Real values will range from 0.0 thru 1.0 .
 */

#if	vax
#define	RANDMAX		2147483647
#else	vax
	UNKNOWN MACHINE!
#endif	vax

long irandm_(iarg)
long *iarg;
{
	if (*iarg) srandom((int)*iarg);
	return( random() );
}

float random_(iarg)
long *iarg;
{
	if (*iarg) srandom((int)*iarg);
	return( (float)(random())/(float)RANDMAX );
}

double drandm_(iarg)
long *iarg;
{
	if (*iarg) srandom((int)*iarg);
	return( (double)(random())/(double)RANDMAX );
}
