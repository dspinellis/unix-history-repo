/*	@(#)tan.c	4.1	12/25/82	*/

/*
	floating point tangent

	A series is used after range reduction.
	Coefficients are #4285 from Hart & Cheney. (19.74D)
*/

#include <errno.h>
#include <math.h>

int	errno;
static double invpi	  = 1.27323954473516268;
static double p0	 = -0.1306820264754825668269611177e+5;
static double p1	  = 0.1055970901714953193602353981e+4;
static double p2	 = -0.1550685653483266376941705728e+2;
static double p3	  = 0.3422554387241003435328470489e-1;
static double p4	  = 0.3386638642677172096076369e-4;
static double q0	 = -0.1663895238947119001851464661e+5;
static double q1	  = 0.4765751362916483698926655581e+4;
static double q2	 = -0.1555033164031709966900124574e+3;

double
tan(arg)
double arg;
{
	double modf();
	double sign, temp, e, x, xsq;
	int flag, i;

	flag = 0;
	sign = 1.;
	if(arg < 0.){
		arg = -arg;
		sign = -1.;
	}
	arg = arg*invpi;   /*overflow?*/
	x = modf(arg,&e);
	i = e;
	switch(i%4) {
	case 1:
		x = 1. - x;
		flag = 1;
		break;

	case 2:
		sign = - sign;
		flag = 1;
		break;

	case 3:
		x = 1. - x;
		sign = - sign;
		break;

	case 0:
		break;
	}

	xsq = x*x;
	temp = ((((p4*xsq+p3)*xsq+p2)*xsq+p1)*xsq+p0)*x;
	temp = temp/(((1.0*xsq+q2)*xsq+q1)*xsq+q0);

	if(flag == 1) {
		if(temp == 0.) {
			errno = ERANGE;
			if (sign>0)
				return(HUGE);
			return(-HUGE);
		}
		temp = 1./temp;
	}
	return(sign*temp);
}
