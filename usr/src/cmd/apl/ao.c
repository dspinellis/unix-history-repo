#include "apl.h"

data
ex_add(d1, d2)
data d1, d2;
{

	if(fuzz(d1, -d2) == 0)
		return(zero);
	return(d1 + d2);
}

data
ex_sub(d1, d2)
data d1, d2;
{

	if(fuzz(d1, d2) == 0)
		return(zero);
	return(d1 - d2);
}

data
ex_mul(d1, d2)
data d1, d2;
{
	return(d1 * d2);
}

data
ex_div(d1, d2)
data d1, d2;
{

	/* 0 div 0 is NOT 1 */
	if(d2 == zero)
		error("div D");
	return(d1 / d2);
}

data
ex_mod(d1, d2)
data d1, d2;
{
	double f;

	/* see 0 div 0 comment */
	if(d1 == zero)
		error("mod D");
	if(d1 < zero)
		d1 = -d1;
	f = d2;
	d2 = d2 - d1 * floor(f/d1);
	return(d2);
}

data
ex_min(d1, d2)
data d1, d2;
{

	if(d1 < d2)
		return(d1);
	return(d2);
}

data
ex_max(d1, d2)
data d1, d2;
{

	if(d1 > d2)
		return(d1);
	return(d2);
}

data
ex_pwr(d1, d2)
data d1, d2;
{
	register s;
	double f1;

	s = 0;
	f1 = d1;
	if(f1 > 0.) {
		f1 = d2 * log(f1);
		goto chk;
	}
	if(f1 == 0.) {
		if(d2 == zero)
			goto bad;
		return(zero);
	}
	/* should check rational d2 here */
	goto bad;

chk:
	if(f1 < maxexp) {
		d1 = exp(f1);
		if(s)
			d1 = -d1;
		return(d1);
	}
bad:
	error("pwr D");
}

data
ex_log(d1, d2)
data d1, d2;
{
	double f1, f2;

	f1 = d1;
	f2 = d2;
	if(f1 <= 0. || f2 <= 0.)
		error("log D");
	d1 = log(f2)/log(f1);
	return(d1);
}

data
ex_cir(d1, d2)
data d1, d2;
{
	double f, f1;

	switch(fix(d1)) {

	default:
		error("crc D");

	case -7:
		/* arc tanh */
		f = d2;
		if(f <= -1. || f >= 1.)
			error("tanh D");
		f = log((1.+f)/(1.-f))/2.;
		goto ret;

	case -6:
		/* arc cosh */
		f = d2;
		if(f < 1.)
			error("cosh D");
		f = log(f+sqrt(f*f-1.));
		goto ret;

	case -5:
		/* arc sinh */
		f = d2;
		f = log(f+sqrt(f*f+1.));
		goto ret;

	case -4:
		/* sqrt(-1 + x*x) */
		f = -one + d2*d2;
		goto sq;

	case -3:
		/* arc tan */
		f = d2;
		f = atan(f);
		goto ret;

	case -2:
		/* arc cos */
		f = d2;
		if(f < -1. || f > 1.)
			error("arc cos D");
		f = atan2(sqrt(1.-f*f), f);
		goto ret;

	case -1:
		/* arc sin */
		f = d2;
		if(f < -1. || f > 1.)
			error("arc sin D");
		f = atan2(f, sqrt(1.-f*f));
		goto ret;

	case 0:
		/* sqrt(1 - x*x) */
		f = one - d2*d2;
		goto sq;

	case 1:
		/* sin */
		f = d2;
		f = sin(f);
		goto ret;

	case 2:
		/* cos */
		f = d2;
		f = cos(f);
		goto ret;

	case 3:
		/* tan */
		f = d2;
		f1 = cos(f);
		if(f1 == 0.)
			f = exp(maxexp); else
			f = sin(f)/f1;
		goto ret;

	case 4:
		/* sqrt(1 + x*x) */
		f = one + d2*d2;
		goto sq;

	case 5:
		/* sinh */
		f = d2;
		if(f < -maxexp || f > maxexp)
			error("sinh D");
		f = exp(f);
		f = (f-1./f)/2.;
		goto ret;

	case 6:
		/* cosh */
		f = d2;
		if(f < -maxexp || f > maxexp)
			error("cosh D");
		f = exp(f);
		f = (f+1./f)/2.;
		goto ret;

	case 7:
		/* tanh */
		f = d2;
		if(f > maxexp) {
			f = 1.;
			goto ret;
		}
		if(f < -maxexp) {
			f = -1.;
			goto ret;
		}
		f = exp(f);
		f = (f-1./f)/(f+1./f);
		goto ret;
	}

sq:
	if(f < 0.)
		error("sqrt D");
	f = sqrt(f);

ret:
	d1 = f;
	return(d1);
}

data
ex_comb(d1, d2)
data d1, d2;
{
	double f;

	if(d1 > d2)
		return(zero);
	f = gamma(d2+1.) - gamma(d1+1.) - gamma(d2-d1+1.);
	if(f > maxexp)
		error("comb D");
	d1 = exp(f);
	return(d1);
}

data
ex_and(d1, d2)
data d1, d2;
{

	if(d1!=zero && d2!=zero)
		return(one);
	return(zero);
}

data
ex_or(d1, d2)
data d1, d2;
{

	if(d1!=zero || d2!=zero)
		return(one);
	return(zero);
}

data
ex_nand(d1, d2)
data d1, d2;
{

	if(d1!=zero && d2!=zero)
		return(zero);
	return(one);
}

data
ex_nor(d1, d2)
data d1, d2;
{

	if(d1!=zero || d2!=zero)
		return(zero);
	return(one);
}

data
ex_lt(d1, d2)
data d1, d2;
{

	if(fuzz(d1, d2) < 0)
		return(one);
	return(zero);
}

data
ex_le(d1, d2)
data d1, d2;
{

	if(fuzz(d1, d2) <= 0)
		return(one);
	return(zero);
}

data
ex_eq(d1, d2)
data d1, d2;
{

	if(fuzz(d1, d2) == 0)
		return(one);
	return(zero);
}

data
ex_ge(d1, d2)
data d1, d2;
{

	if(fuzz(d1, d2) >= 0)
		return(one);
	return(zero);
}

data
ex_gt(d1, d2)
data d1, d2;
{

	if(fuzz(d1, d2) > 0)
		return(one);
	return(zero);
}

data
ex_ne(d1, d2)
data d1, d2;
{

	if(fuzz(d1, d2) != 0)
		return(one);
	return(zero);
}

data
ex_plus(d)
data d;
{

	return(d);
}

data
ex_minus(d)
data d;
{

	return(-d);
}

data
ex_sgn(d)
data d;
{

	if(d == zero)
		return(zero);
	if(d < zero)
		return(-one);
	return(one);
}

data
ex_recip(d)
data d;
{

	if(d == zero)
		error("recip D");
	return(one/d);
}

data
ex_abs(d)
data d;
{

	if(d < zero)
		return(-d);
	return(d);
}

data
ex_floor(d)
data d;
{

	d = floor(d + thread.fuzz);
	return(d);
}

data
ex_ceil(d)
data d;
{

	d = ceil(d - thread.fuzz);
	return(d);
}

data
ex_exp(d)
data d;
{
	double f;

	f = d;
	if(f > maxexp)
		error ("exp D");
	d = exp(f);
	return(d);
}

data
ex_loge(d)
data d;
{
	double f;

	f = d;
	if(f <= 0.)
		error("log D");
	d = log(f);
	return(d);
}

data
ex_pi(d)
data d;
{

	d = pi * d;
	return(d);
}

data
ex_rand(d)
data d;
{
	double f;

	f = (rand()/(32768.*32768.*2.)) * d;
	d = floor(f) + thread.iorg;
	return(d);
}

data
ex_fac(d)
data d;
{
	double f;

	f = gamma(d+1.);
	if(f > maxexp)
		error("fac D");
	d = exp(f);
	if(signgam == -1)
		d = -d;
	return(d);
}

data
ex_not(d)
data d;
{

	if(d == zero)
		return(one);
	return(zero);
}
