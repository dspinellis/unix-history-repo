/*	@(#)exp.c	4.1/4.2 	10/31/84	CCI-CPG */


/*
 * The double-precision 'exp' returns the exponential
 * function of its floating-point argument.
 *
 * New version by Les Powers (3/23/85).
 */


#include <errno.h>



/*
 * The following number 'forces' the hex value of 0x7fffffff 0xffffffff
 * to be used for HUGE.
 */
#define	HUGE		1.701411834604692350e+40

#define EXPONENT28	0x0e000000
#define EXP_SIZE0	12
#define EXP_SIZE1	128
#define EXP_SIZE2	256
#define EXP_SIZE3	256
#define EXP_SIZE4	256

int	errno;

/*
 * This is the natural log of the smallest number that can
 * be represented with double precision format (2^-128).
 */
double minf = -88.7228391116729996054057115466466007136640;

/*
 * This is the natural log of the biggest number that can
 * be represented with double precision format (2^127 (1-2^-56)).
 */
double maxf = 88.0296919311130542821106916173739672939966;

double _ep0[EXP_SIZE0];
double _ep1[EXP_SIZE1];
double _ep2[EXP_SIZE2];
double _ep3[EXP_SIZE3];
double _ep4[EXP_SIZE4];
double _en0[EXP_SIZE0];
double _en1[EXP_SIZE1];
double _en2[EXP_SIZE2];
double _en3[EXP_SIZE3];
double _en4[EXP_SIZE4];


double
exp(arg)
double arg;
{
	int a0;
	union {
		int i;
		struct {
			unsigned char b0;
			unsigned char b1;
			unsigned char b2;
			unsigned char b3;
		} b;
	} u;
	register union {
		double d;
		int i;
	} abs_arg;

	if (arg == 0.0)
		return(1.0);

	if (arg >= 0.0) {
		abs_arg.d = arg;
		if (abs_arg.i >= 0x42000000 ) {	/* if (abs_arg.i >= 8.0) */
			if (abs_arg.d > maxf) {
				errno = ERANGE;
				return(HUGE);
			}
			a0 = abs_arg.d;
			return(exp(arg - (a0&0x78)) * _ep0[a0>>3]);
		}

		abs_arg.i += EXPONENT28;    /* multiply by 2 to 28th power */
		u.i = abs_arg.d;
		abs_arg.d = u.i;
		abs_arg.i -= EXPONENT28;    /* divide by 2 to 28th power */
		return(((arg-abs_arg.d)+1.)*
			_ep1[u.b.b0]*_ep2[u.b.b1]*_ep3[u.b.b2]*_ep4[u.b.b3]);
	} else {
		abs_arg.d = -arg;
		if (abs_arg.i >= 0x42000000) {
			if (arg < minf)
				return(0.);
			a0 = -arg;
			return(exp(arg + (a0&0x78)) * _en0[a0>>3]);
		}

		abs_arg.i += EXPONENT28;    /* multiply by 2 to 28th power */
		u.i = abs_arg.d;
		abs_arg.d = u.i;
		abs_arg.i -= EXPONENT28;    /* divide by 2 to 28th power */
		return(((arg+abs_arg.d)+1.)*
		      _en1[u.b.b0]*_en2[u.b.b1]*_en3[u.b.b2]*_en4[u.b.b3]);
	}
}
