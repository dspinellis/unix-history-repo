/*
char id_range[] = "@(#)range.c	1.1";
 *
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
#ifdef	PDP11
	n.i = 0x7fffffffL;
#endif
#ifdef	VAX
	n.i = 0xffff7fff;
#endif
	return(n.f);
}

double
dflmax_()
{
	union dj n;
#ifdef	PDP11
	n.j[0] = 0x7fffffffL;
	n.j[1] = 0xffffffffL;
#endif
#ifdef	VAX
	n.j[0] = 0xffff7fff;
	n.j[1] = 0xffffffff;
#endif
	return(n.d);
}

float
flmin_()
{
	union fi n;
#ifdef	PDP11
	n.i = 0x00800000L;
#endif
#ifdef	VAX
	n.i = 0x00000080;
#endif
	return(n.f);
}

double
dflmin_()
{
	union dj n;
#ifdef	PDP11
	n.j[0] = 0x00800000L;
	n.j[1] = 0;
#endif
#ifdef	VAX
	n.j[0] = 0x00000080;
	n.j[1] = 0;
#endif
	return(n.d);
}

long int
inmax_()
{
	return(0x7fffffff);
}

