#ifndef lint
static	char *sccsid = "@(#)wwredraw.c	3.2 83/08/11";
#endif

#include "ww.h"

wwredraw()
{
	register i, j;
	register union ww_char *os;

	(*tt.tt_clear)();
	for (i = 0; i < wwnrow; i++) {
		os = wwos[i];
		for (j = wwncol; --j >= 0;)
			(os++)->c_w = ' ';
	}
}
