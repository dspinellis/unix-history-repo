/*
 *	@(#)zabs.c	5.2	%G%
 */

#ifdef tahoe
/* THIS IS BASED ON TAHOE FP REPRESENTATION */
#include <tahoemath/FP.h>

double zabs(real, imag)
double real, imag;
{
double temp, sqrt();

if(real < 0)
	*(long int *)&real ^= SIGN_BIT;
if(imag < 0)
	*(long int *)&imag ^= SIGN_BIT;
if(imag > real){
	temp = real;
	real = imag;
	imag = temp;
}
if(imag == 0.)		/* if((real+imag) == real) */
	return(real);

temp = imag/real;
temp = real*sqrt(1.0 + temp*temp);  /*overflow!!*/
return(temp);
}
#endif tahoe
