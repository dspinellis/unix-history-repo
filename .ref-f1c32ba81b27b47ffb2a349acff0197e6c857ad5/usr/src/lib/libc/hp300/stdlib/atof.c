/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)atof.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/*
 * simple atof() for IEEE 754 architectures
 */

#include <machine/endian.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>

static double twoemax = 9007199254740992.;	/*2^53*/

/* attempt to be as exact as possible */
static struct {
	long low_word;
	long high_word;
} exp5[] = {
#if	BYTE_ORDER == BIG_ENDIAN
	{ 0x40140000, 0x00000000 },	/* 5 */
	{ 0x40390000, 0x00000000 },	/* 25 */
	{ 0x40838800, 0x00000000 },	/* 625 */
	{ 0x4117d784, 0x00000000 },	/* 390625 */
	{ 0x4241c379, 0x37e08000 },	/* 152587890625 */
	{ 0x4493b8b5, 0xb5056e17 },	/* 2.3283064365387e+022 */
	{ 0x49384f03, 0xe93ff9f6 },	/* 5.42101086242753e+044 */
	{ 0x52827748, 0xf9301d33 },	/* 2.93873587705572e+089 */
	{ 0x65154fdd, 0x7f73bf3f }	/* 8.63616855509445e+178 */
#else	/* BYTE_ORDER == LITTLE_ENDIAN */
	{ 0x00000000, 0x40140000 },	/* 5 */
	{ 0x00000000, 0x40390000 },	/* 25 */
	{ 0x00000000, 0x40838800 },	/* 625 */
	{ 0x00000000, 0x4117d784 },	/* 390625 */
	{ 0x37e08000, 0x4241c379 },	/* 152587890625 */
	{ 0xb5056e17, 0x4493b8b5 },	/* 2.3283064365387e+022 */
	{ 0xe93ff9f6, 0x49384f03 },	/* 5.42101086242753e+044 */
	{ 0xf9301d33, 0x52827748 },	/* 2.93873587705572e+089 */
	{ 0x7f73bf3f, 0x65154fdd }	/* 8.63616855509445e+178 */
#endif
};

double
atof(p)
	register const char *p;
{
	register int c;
	register int exp = 0;
	register int eexp = 0;
	double fl = 0;
	double flexp = 1.0;
	int bexp;
	int neg = 1;
	int negexp = 1;

	while (isspace(*p))
		++p;

	if ((c = *p++) == '-')
		neg = -1;
	else if (c == '+')
		/* skip it */;
	else
		--p;

	while ((c = *p++) && isdigit(c))
		if (fl < twoemax)
			fl = 10 * fl + (c-'0');
		else
			++exp;

	if (c == '.')
		while ((c = *p++) && isdigit(c))
			if (fl < twoemax) {
				fl = 10 * fl + (c-'0');
				--exp;
			}

	if (c == 'E' || c == 'e') {
		if ((c = *p++) == '-')
			negexp = -1;
		else if (c == '+')
			/* skip it */;
		else
			--p;
		while ((c = *p++) && isdigit(c))
			eexp = 10 * eexp + (c-'0');
		if (negexp < 0)
			eexp = -eexp;
		exp += eexp;
	}

	bexp = exp;
	if (exp < 0)
		exp = -exp;

	for (c = 0; exp && c < sizeof exp5 / sizeof exp5[0]; ++c) {
		if (exp & 1)
			flexp *= *(double *)&exp5[c];
		exp >>= 1;
	}

	if (bexp < 0)
		fl /= flexp;
	else
		fl *= flexp;

	fl = ldexp(fl, bexp);

	return neg < 0 ? -fl : fl;
}
