/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Wang at The University of California, Berkeley.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)ttf100.c	3.12 (Berkeley) 6/6/90";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

/*
 * Freedom 100
 */

#define G (WWM_GRP << WWC_MSHIFT)
short f100_frame[16] = {
	' ',	'J'|G,	'K'|G,	'A'|G,
	'J'|G,	'J'|G,	'B'|G,	'M'|G,
	'K'|G,	'D'|G,	'K'|G,	'O'|G,
	'C'|G,	'L'|G,	'N'|G,	'I'|G
};
extern struct tt_str *gen_AE, *gen_AS;

tt_f100()
{
	static struct tt_str ae = { "\033%", 2 };
	static struct tt_str as = { "\033$", 2 };

	if (tt_generic() < 0)
		return -1;
	tt.tt_frame = f100_frame;
	tt.tt_availmodes |= WWM_GRP;
	gen_AS = &as;
	gen_AE = &ae;
	return 0;
}
