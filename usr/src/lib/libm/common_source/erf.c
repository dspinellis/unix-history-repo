/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)erf.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*
	C program for floating point error function

	erf(x) returns the error function of its argument
	erfc(x) returns 1.0-erf(x)

	erf(x) is defined by
	${2 over sqrt(pi)} int from 0 to x e sup {-t sup 2} dt$

	the entry for erfc is provided because of the
	extreme loss of relative accuracy if erf(x) is
	called for large x and the result subtracted
	from 1. (e.g. for x= 10, 12 places are lost).

	There are no error returns.

	Calls exp.

	Coefficients for large x are #5667 from Hart & Cheney (18.72D).
*/

#define M 7
#define N 9
static double torp = 1.1283791670955125738961589031;
static double p1[] = {
	0.804373630960840172832162e5,
	0.740407142710151470082064e4,
	0.301782788536507577809226e4,
	0.380140318123903008244444e2,
	0.143383842191748205576712e2,
	-.288805137207594084924010e0,
	0.007547728033418631287834e0,
};
static double q1[]  = {
	0.804373630960840172826266e5,
	0.342165257924628539769006e5,
	0.637960017324428279487120e4,
	0.658070155459240506326937e3,
	0.380190713951939403753468e2,
	0.100000000000000000000000e1,
	0.0,
};
static double p2[]  = {
	0.18263348842295112592168999e4,
	0.28980293292167655611275846e4,
	0.2320439590251635247384768711e4,
	0.1143262070703886173606073338e4,
	0.3685196154710010637133875746e3,
	0.7708161730368428609781633646e2,
	0.9675807882987265400604202961e1,
	0.5641877825507397413087057563e0,
	0.0,
};
static double q2[]  = {
	0.18263348842295112595576438e4,
	0.495882756472114071495438422e4,
	0.60895424232724435504633068e4,
	0.4429612803883682726711528526e4,
	0.2094384367789539593790281779e4,
	0.6617361207107653469211984771e3,
	0.1371255960500622202878443578e3,
	0.1714980943627607849376131193e2,
	1.0,
};

double
erf(arg) double arg;{
	double erfc();
	int sign;
	double argsq;
	double d, n;
	int i;

	sign = 1;
	if(arg < 0.){
		arg = -arg;
		sign = -1;
	}
	if(arg < 0.5){
		argsq = arg*arg;
		for(n=0,d=0,i=M-1; i>=0; i--){
			n = n*argsq + p1[i];
			d = d*argsq + q1[i];
		}
		return(sign*torp*arg*n/d);
	}
	if(arg >= 10.)
		return(sign*1.);
	return(sign*(1. - erfc(arg)));
}

double
erfc(arg) double arg;{
	double erf();
	double exp();
	double n, d;
	int i;

	if(arg < 0.)
		return(2. - erfc(-arg));
/*
	if(arg < 0.5)
		return(1. - erf(arg));
*/
	if(arg >= 10.)
		return(0.);

	for(n=0,d=0,i=N-1; i>=0; i--){
		n = n*arg + p2[i];
		d = d*arg + q2[i];
	}
	return(exp(-arg*arg)*n/d);
}
