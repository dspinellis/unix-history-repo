/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * David Barto at Celerity Computer Corp.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)tttvi925.c	3.6 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

/*
 * Televideo 925 as emulated by Microterm.
 */

#define G (WWM_GRP << WWC_MSHIFT)
short tvi925_frame[16] = {
	' ',	'~'|G,	'|'|G,	'c'|G,
	'~'|G,	'~'|G,	'`'|G,	'e'|G,
	'|'|G,	'a'|G,	'|'|G,	'g'|G,
	'b'|G,	'f'|G,	'h'|G,	'd'|G
};

tt_tvi925()
{

	if (tt_generic() < 0)
		return -1;
	tt.tt_availmodes |= WWM_GRP;
	tt.tt_frame = tvi925_frame;
	return 0;
}
