/*
 * Copyright (c) 1989 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)float.h	7.2 (Berkeley) %G%
 */

#define FLT_RADIX	2		/* b */
#define FLT_ROUNDS	1		/* FP addition rounds to nearest */

#define FLT_MANT_DIG	24		/* p */
#define FLT_EPSILON	1.19209290E-07F	/* b**(1-p) */
#define FLT_DIG		6		/* floor((p-1)*log10(b))+(b == 10) */
#define FLT_MIN_EXP	-127		/* emin */
#define FLT_MIN		2.93873588E-39F	/* b**(emin-1) */
#define FLT_MIN_10_EXP	-38		/* ceil(log10(b**(emin-1))) */
#define FLT_MAX_EXP	127		/* emax */
#define FLT_MAX		1.70141173E+38F	/* (1-b**(-p))*b**emax */
#define FLT_MAX_10_EXP	38		/* floor(log10((1-b**(-p))*b**emax)) */

#define DBL_MANT_DIG	56
#define DBL_EPSILON	2.775557561562891351E-17
#define DBL_DIG		16
#define DBL_MIN_EXP	-127
#define DBL_MIN		2.938735877055718770E-39
#define DBL_MIN_10_EXP	-38
#define DBL_MAX_EXP	127
#define DBL_MAX		1.701411834604692294E+38
#define DBL_MAX_10_EXP	38

#define LDBL_MANT_DIG	DBL_MANT_DIG
#define LDBL_EPSILON	DBL_EPSILON
#define LDBL_DIG	DBL_DIG
#define LDBL_MIN_EXP	DBL_MIN_EXP
#define LDBL_MIN	DBL_MIN
#define LDBL_MIN_10_EXP	DBL_MIN_10_EXP
#define LDBL_MAX_EXP	DBL_MAX_EXP
#define LDBL_MAX	DBL_MAX
#define LDBL_MAX_10_EXP	DBL_MAX_10_EXP
