/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)ttf100.c	3.10 (Berkeley) 6/29/88";
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
