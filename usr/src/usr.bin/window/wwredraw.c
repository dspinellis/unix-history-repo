#ifndef lint
static	char *sccsid = "@(#)wwredraw.c	3.5 83/12/02";
#endif

#include "ww.h"
#include "tt.h"

wwredraw()
{
	register i, j;
	register union ww_char *os;

	(*tt.tt_clear)();
	for (i = 0; i < wwnrow; i++) {
		wwtouched[i] = WWU_TOUCHED;
		os = wwos[i];
		for (j = wwncol; --j >= 0;)
			(os++)->c_w = ' ';
	}
}
