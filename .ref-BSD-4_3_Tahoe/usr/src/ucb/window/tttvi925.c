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
static char sccsid[] = "@(#)tttvi925.c	3.4 (Berkeley) 6/29/88";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

/*
 * Televideo 925 as emulated by Microterm.
 *
 * From David Barto <sdcsvax!celerity!barto>.
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
