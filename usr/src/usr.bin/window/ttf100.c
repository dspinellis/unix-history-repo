#ifndef lint
static	char *sccsid = "@(#)ttf100.c	3.4 3.4";
#endif

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
extern char *gen_GE, *gen_GS;

tt_f100()
{
	int ret;

	ret = tt_generic();
	tt.tt_frame = f100_frame;
	tt.tt_availmodes |= WWM_GRP;
	gen_GS = "\033$";
	gen_GE = "\033%";
	return ret;
}
