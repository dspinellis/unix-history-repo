#ifndef lint
static char sccsid[] = "@(#)ttwyse75.c	3.1 10/6/87";
#endif

#include "ww.h"
#include "tt.h"

/*
 * Wyse-75
 *
 * Contributed by David Elliott of MIPS (mips!dce).
 */

/*
 * Copyright 1987 by David C. Elliott, MIPS Computer Systems.
 *
 * Unlimited redistribution allowed as long as this notice
 * is kept intact.
 */

#define G (WWM_GRP << WWC_MSHIFT)
short wyse75_frame[16] = {
	' ',	'x'|G,	'q'|G,	'm'|G,
	'x'|G,	'x'|G,	'l'|G,	't'|G,
	'q'|G,	'j'|G,	'q'|G,	'v'|G,
	'k'|G,	'u'|G,	'w'|G,	'v'|G
};

extern struct tt_str *gen_AS;
extern struct tt_str *gen_AE;

tt_wyse75()
{
	static struct tt_str ae = { "\033(B", 3 };
	static struct tt_str as = { "\033(0", 3 };

	if (tt_generic() < 0)
		return -1;
	tt.tt_availmodes |= WWM_GRP;
	tt.tt_frame = wyse75_frame;
	if (gen_AS == 0)
		gen_AS = &as;
	if (gen_AE == 0)
		gen_AE = &ae;
	return 0;
}
