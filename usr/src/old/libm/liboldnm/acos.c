/*
acos(arg) return the arccos,
	respectively of their arguments.

	Arctan is called after appropriate range reduction.
*/

#include	<errno.h>
int errno;
double atan();
double asin();
static double pio2	= 1.570796326794896619;

double
acos(arg) double arg; {

	asm("	bispsw	$0xe0");
	if(arg > 1.|| arg < -1.){
		errno = EDOM;
		return(0.);
	}

	return(pio2 - asin(arg));
}
