/*
char id_rand[] = "%W%";
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

#ifdef	VAX
#define	RANDMAX		2147483647
#endif
#ifdef	PDP11
#define	RANDMAX		32767
#endif

long irand_(iarg)
long *iarg;
{
	if (*iarg) srand((int)*iarg);
#ifdef	PDP11
	return(( ((long)rand()) << 16) | rand());
#else
	return( rand() );
#endif
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
