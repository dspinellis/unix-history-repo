/*
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ieee.h	7.3 (Berkeley) %G%
 *
 * from: $Header: ieee.h,v 1.7 92/11/26 02:04:37 torek Exp $
 */

/*
 * ieee.h defines the machine-dependent layout of the machine's IEEE
 * floating point.  It does *not* define (yet?) any of the rounding
 * mode bits, exceptions, and so forth.
 */

/*
 * Define the number of bits in each fraction and exponent.
 *
 *		     k	         k+1
 * Note that  1.0 x 2  == 0.1 x 2      and that denorms are represented
 *
 *					  (-exp_bias+1)
 * as fractions that look like 0.fffff x 2             .  This means that
 *
 *			 -126
 * the number 0.10000 x 2    , for instance, is the same as the normalized
 *
 *		-127			   -128
 * float 1.0 x 2    .  Thus, to represent 2    , we need one leading zero
 *
 *				  -129
 * in the fraction; to represent 2    , we need two, and so on.  This
 *
 *						     (-exp_bias-fracbits+1)
 * implies that the smallest denormalized number is 2
 *
 * for whichever format we are talking about: for single precision, for
 *
 *						-126		-149
 * instance, we get .00000000000000000000001 x 2    , or 1.0 x 2    , and
 *
 * -149 == -127 - 23 + 1.
 */
#define	SNG_EXPBITS	8
#define	SNG_FRACBITS	23

#define	DBL_EXPBITS	11
#define	DBL_FRACBITS	52

#ifdef notyet
#define	E80_EXPBITS	15
#define	E80_FRACBITS	64
#endif

#define	EXT_EXPBITS	15
#define	EXT_FRACBITS	112

struct ieee_single {
	u_int	sng_sign:1;
	u_int	sng_exp:8;
	u_int	sng_frac:23;
};

struct ieee_double {
	u_int	dbl_sign:1;
	u_int	dbl_exp:11;
	u_int	dbl_frach:20;
	u_int	dbl_fracl;
};

struct ieee_ext {
	u_int	ext_sign:1;
	u_int	ext_exp:15;
	u_int	ext_frach:16;
	u_int	ext_frachm;
	u_int	ext_fraclm;
	u_int	ext_fracl;
};

/*
 * Floats whose exponent is in [1..INFNAN) (of whatever type) are
 * `normal'.  Floats whose exponent is INFNAN are either Inf or NaN.
 * Floats whose exponent is zero are either zero (iff all fraction
 * bits are zero) or subnormal values.
 *
 * A NaN is a `signalling NaN' if its QUIETNAN bit is clear in its
 * high fraction; if the bit is set, it is a `quiet NaN'.
 */
#define	SNG_EXP_INFNAN	255
#define	DBL_EXP_INFNAN	2047
#define	EXT_EXP_INFNAN	32767

#if 0
#define	SNG_QUIETNAN	(1 << 22)
#define	DBL_QUIETNAN	(1 << 19)
#define	EXT_QUIETNAN	(1 << 15)
#endif

/*
 * Exponent biases.
 */
#define	SNG_EXP_BIAS	127
#define	DBL_EXP_BIAS	1023
#define	EXT_EXP_BIAS	16383
