/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)cabs.c	5.1	%G%
 */

double cabs(real, imag)
double real, imag;
{
double temp, sqrt();

if(real < 0)
	real = -real;
if(imag < 0)
	imag = -imag;
if(imag > real){
	temp = real;
	real = imag;
	imag = temp;
}
if((real+imag) == real)
	return(real);

temp = imag/real;
temp = real*sqrt(1.0 + temp*temp);  /*overflow!!*/
return(temp);
}
