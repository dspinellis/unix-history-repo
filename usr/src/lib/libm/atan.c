
/*
	floating-point arctangent

	atan returns the value of the arctangent of its
	argument in the range [-pi/2,pi/2].

	atan2 returns the arctangent of arg1/arg2
	in the range [-pi,pi].

	there are no error returns.

	coefficients are #5077 from Hart & Cheney. (19.56D)
*/


double static sq2p1	 =2.414213562373095048802e0;
static double sq2m1	 = .414213562373095048802e0;
static double pio2	 =1.570796326794896619231e0;
static double pio4	 = .785398163397448309615e0;
static double p4	 = .161536412982230228262e2;
static double p3	 = .26842548195503973794141e3;
static double p2	 = .11530293515404850115428136e4;
static double p1	 = .178040631643319697105464587e4;
static double p0	 = .89678597403663861959987488e3;
static double q4	 = .5895697050844462222791e2;
static double q3	 = .536265374031215315104235e3;
static double q2	 = .16667838148816337184521798e4;
static double q1	 = .207933497444540981287275926e4;
static double q0	 = .89678597403663861962481162e3;


/*
	atan makes its argument positive and
	calls the inner routine satan.
*/

double
atan(arg)
double arg;
{
	double satan();

	if(arg>0)
		return(satan(arg));
	else
		return(-satan(-arg));
}


/*
	atan2 discovers what quadrant the angle
	is in and calls atan.
*/

double
atan2(arg1,arg2)
double arg1,arg2;
{
	double satan();

	if((arg1+arg2)==arg1)
		if(arg1 >= 0.) return(pio2);
		else return(-pio2);
	else if(arg2 <0.)
		if(arg1 >= 0.)
			return(pio2+pio2 - satan(-arg1/arg2));
		else
			return(-pio2-pio2 + satan(arg1/arg2));
	else if(arg1>0)
		return(satan(arg1/arg2));
	else
		return(-satan(-arg1/arg2));
}

/*
	satan reduces its argument (known to be positive)
	to the range [0,0.414...] and calls xatan.
*/

static double
satan(arg)
double arg;
{
	double	xatan();

	if(arg < sq2m1)
		return(xatan(arg));
	else if(arg > sq2p1)
		return(pio2 - xatan(1.0/arg));
	else
		return(pio4 + xatan((arg-1.0)/(arg+1.0)));
}

/*
	xatan evaluates a series valid in the
	range [-0.414...,+0.414...].
*/

static double
xatan(arg)
double arg;
{
	double argsq;
	double value;

	argsq = arg*arg;
	value = ((((p4*argsq + p3)*argsq + p2)*argsq + p1)*argsq + p0);
	value = value/(((((argsq + q4)*argsq + q3)*argsq + q2)*argsq + q1)*argsq + q0);
	return(value*arg);
}
