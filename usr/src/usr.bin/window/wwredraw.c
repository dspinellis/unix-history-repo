#ifndef lint
static	char *sccsid = "@(#)wwredraw.c	3.3 83/08/15";
#endif

#include "ww.h"
#include "tt.h"

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
