#ifndef lint
static	char *sccsid = "@(#)wwupdate.c	3.10 83/12/21";
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
		if (*touched & WWU_MAJOR) {
			int ncleared = 0;
			int nsame = 0;

			wwnmajline++;
			j = wwncol;
			ns = wwns[i];
			os = wwos[i];
			while (--j >= 0) {
				if (ns->c_w == ' ') {
					if (ns->c_w != os->c_w)
						ncleared++;
				} else
					if (ns->c_w == os->c_w)
						nsame++;
				ns++;
				os++;
			}
			if (tt.tt_clreol != 0 && ncleared > nsame + 4) {
				(*tt.tt_move)(i, 0);
				(*tt.tt_clreol)();
				for (j = wwncol, os = wwos[i]; --j >= 0;)
					os++->c_w = ' ';
			} else
				wwnmajmiss++;
		}
		wwnupdline++;
		didit = 0;
		ns = wwns[i];
		os = wwos[i];
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
			if (wwwrap
			    && i == wwnrow - 1 && q - buf + c == wwncol) {
				if (tt.tt_hasinsert) {
					if (q - buf != 1) {
						(*tt.tt_move)(i, c);
						(*tt.tt_write)(buf + 1,
							q - buf - 1);
						(*tt.tt_move)(i, c);
						tt.tt_ninsert = 1;
						(*tt.tt_write)(buf, 1);
						tt.tt_ninsert = 0;
					} else {
						(*tt.tt_move)(i, c - 1);
						(*tt.tt_write)(buf, 1);
						tt.tt_nmodes = ns[-2].c_m;
						(*tt.tt_move)(i, c - 1);
						tt.tt_ninsert = 1;
						(*tt.tt_write)(&ns[-2].c_c, 1);
						tt.tt_ninsert = 0;
					}
				} else {
					(*tt.tt_move)(i, c);
					os[-1] = lastc;
					(*tt.tt_write)(buf, q - buf - 1);
				}
			} else {
				(*tt.tt_move)(i, c);
				(*tt.tt_write)(buf, q - buf);
			}
			didit++;
		}
		if (!didit)
			wwnupdmiss++;
		*touched = 0;
	}
}
