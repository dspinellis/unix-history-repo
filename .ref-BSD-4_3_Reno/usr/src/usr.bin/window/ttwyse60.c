/*
 * Copyright 1987 by David C. Elliott, MIPS Computer Systems.
 *
 * Unlimited redistribution allowed as long as this notice
 * is kept intact.
 */

/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * David C. Elliott, of MIPS Computer Systems.
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
static char sccsid[] = "@(#)ttwyse60.c	3.4 (Berkeley) 6/6/90";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

#define G (WWM_GRP << WWC_MSHIFT)
short wyse60_frame[16] = {
	' ',	'6'|G,	':'|G,	'1'|G,
	'6'|G,	'6'|G,	'2'|G,	'4'|G,
	':'|G,	'5'|G,	':'|G,	'='|G,
	'3'|G,	'9'|G,	'0'|G,	'0'|G
};

extern struct tt_str *gen_AS;
extern struct tt_str *gen_AE;

tt_wyse60()
{
	static struct tt_str ae = { "\033H\003", 3 };
	static struct tt_str as = { "\033H\002", 3 };

	if (tt_generic() < 0)
		return -1;
	tt.tt_availmodes |= WWM_GRP;
	tt.tt_frame = wyse60_frame;
	if (gen_AS == 0)
		gen_AS = &as;
	if (gen_AE == 0)
		gen_AE = &ae;
	return 0;
}
