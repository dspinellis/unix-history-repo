#ifndef lint
static	char *sccsid = "@(#)ttf100.c	3.3 3.3";
#endif

#include "ww.h"
#include "tt.h"

/*
 * Freedom 100
 */

char f100_frame[16] = {
	' ',      'J'|0x80, 'K'|0x80, 'A'|0x80,
	'J'|0x80, 'J'|0x80, 'B'|0x80, 'M'|0x80,
	'K'|0x80, 'D'|0x80, 'K'|0x80, 'O'|0x80,
	'C'|0x80, 'L'|0x80, 'N'|0x80, 'I'|0x80
};
extern char *gen_GE, *gen_GS;

tt_f100()
{
	int ret;

	ret = tt_generic();
	tt.tt_frame = f100_frame;
	gen_GS = "\033$";
	gen_GE = "\033%";
	return ret;
}
