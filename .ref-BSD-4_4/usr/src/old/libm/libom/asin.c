/*	@(#)asin.c	4.1	12/25/82	*/

/*
	asin(arg) and acos(arg) return the arcsin, arccos,
	respectively of their arguments.

	Arctan is called after appropriate range reduction.
*/

#include	<errno.h>
int errno;
double atan();
double sqrt();
static double pio2	= 1.570796326794896619;

double
asin(arg) double arg; {

	double sign, temp;

	sign = 1.;
	if(arg <0){
		arg = -arg;
		sign = -1.;
	}

	if(arg > 1.){
		errno = EDOM;
		return(0.);
	}

	temp = sqrt(1. - arg*arg);
	if(arg > 0.7)
		temp = pio2 - atan(temp/arg);
	else
		temp = atan(arg/temp);

	return(sign*temp);
}

double
acos(arg) double arg; {

	if((arg > 1.) || (arg < -1.)){
		errno = EDOM;
		return(0.);
	}

	return(pio2 - asin(arg));
}
