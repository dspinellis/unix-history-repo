#ifndef lint
static	char *sccsid = "@(#)wwupdate.c	3.2 83/08/11";
#endif

#include "ww.h"

wwupdate()
{
	register i, j;
	register union ww_char *ns, *os;

	(*tt.tt_setinsert)(0);
	for (i = 0; i < wwnrow; i++) {
		ns = wwns[i];
		os = wwos[i];
		for (j = 0; j < wwncol; j++, ns++, os++) {
			if (ns->c_w != os->c_w) {
				(*tt.tt_move)(i, j);
				(*tt.tt_setmodes)(ns->c_m);
				(*tt.tt_putc)(ns->c_c);
				*os = *ns;
			}
		}
	}
}
