/*
 *	@(#)FP.h	5.1 (Berkeley) 11/3/86
 *
 * General definitions of the floating point stuff on Power 6/32.
 * The floating point format definition is:
 *
 *		S    (exp-128)
 *	    (-1)  * 2	       * F
 *
 *	Where exp is the exponent field and F is the binary
 *	mantissa following it, including the hidden bit.
 *	The hidden bit actually is 1/2, so F is known to
 *	satisfy the range:
 *		1/2 <= F < 1
 */

typedef struct {
	unsigned	sign:1;
	unsigned	exponent:8;
	unsigned	mantissa:23;
} sp_format;

typedef struct {
	unsigned	sign:1;
	unsigned	exponent:8;
	unsigned	mantissa:23;
	unsigned	mantissa_lst;
} dp_format;

#define	EXP_BIAS	128		/* Exponent bias */
#define SIGN_BIT	0x80000000	/* S bit mask */
