#ifndef lint
static	char *sccsid = "@(#)wwredraw.c	3.4 83/08/16";
#endif

#include "ww.h"
#include "tt.h"

wwredraw()
{
	register i, j;
	register union ww_char *os;

	(*tt.tt_clear)();
	for (i = 0; i < wwnrow; i++) {
		wwtouched[i] = 1;
		os = wwos[i];
		for (j = wwncol; --j >= 0;)
			(os++)->c_w = ' ';
	}
}
