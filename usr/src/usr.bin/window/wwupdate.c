#ifndef lint
static	char *sccsid = "@(#)wwupdate.c	3.4 83/08/16";
#endif

#include "ww.h"
#include "tt.h"

wwupdate()
{
	register i, j;
	register union ww_char *ns, *os;
	register char *touched;
	register didit;

	wwnupdate++;
	(*tt.tt_setinsert)(0);
	for (i = 0, touched = wwtouched; i < wwnrow; i++, touched++) {
		if (!*touched)
			continue;
		wwntouched++;
		*touched = 0;
		ns = wwns[i];
		os = wwos[i];
		didit = 0;
		for (j = 0; j < wwncol; j++, ns++, os++) {
			if (ns->c_w != os->c_w) {
				(*tt.tt_move)(i, j);
				(*tt.tt_setmodes)(ns->c_m);
				(*tt.tt_putc)(ns->c_c);
				*os = *ns;
				didit++;
			}
		}
		if (!didit)
			wwnmiss++;
	}
}
