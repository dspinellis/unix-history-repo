#ifndef lint
static	char *sccsid = "@(#)wwupdate.c	3.8 83/09/15";
#endif

#include "ww.h"
#include "tt.h"

wwupdate()
{
	int i;
	register j;
	int c, x;
	register union ww_char *ns, *os;
	register char *p, *q;
	char m;
	char *touched;
	register didit;
	char buf[512];			/* > wwncol */
	union ww_char lastc;

	wwnupdate++;
	for (i = 0, touched = wwtouched; i < wwnrow; i++, touched++) {
		if (!*touched)
			continue;
		wwntouched++;
		*touched = 0;

		ns = wwns[i];
		os = wwos[i];
		didit = 0;
		for (j = 0; j < wwncol;) {
			for (; j++ < wwncol && ns++->c_w == os++->c_w;)
				;
			if (j > wwncol)
				break;
			p = buf;
			m = ns[-1].c_m & tt.tt_availmodes;
			c = j - 1;
			os[-1] = ns[-1];
			*p++ = ns[-1].c_c;
			x = 5;
			q = p;
			while (j < wwncol && (ns->c_m&tt.tt_availmodes) == m) {
				*p++ = ns->c_c;
				if (ns->c_w == os->c_w) {
					if (--x <= 0)
						break;
					os++;
					ns++;
				} else {
					x = 5;
					q = p;
					lastc = *os;
					*os++ = *ns++;
				}
				j++;
			}
			tt.tt_nmodes = m;
			(*tt.tt_move)(i, c);
			if (wwwrap
			    && i == wwnrow - 1 && q - buf + c == wwncol) {
				if (tt.tt_hasinsert) {
					(*tt.tt_write)(buf + 1, q - buf - 1);
					(*tt.tt_move)(i, c);
					tt.tt_ninsert = 1;
					(*tt.tt_write)(buf, 1);
					tt.tt_ninsert = 0;
				} else {
					os[-1] = lastc;
					(*tt.tt_write)(buf, q - buf - 1);
				}
			} else
				(*tt.tt_write)(buf, q - buf);
			didit++;
		}
		if (!didit)
			wwnmiss++;
	}
}
