/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)wwupdate.c	3.19 (Berkeley) 6/29/88";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

wwupdate1(top, bot)
{
	int i;
	register j;
	register union ww_char *ns, *os;
	char *touched;
	char didit;

	wwnupdate++;
	for (i = top, touched = &wwtouched[i]; i < bot && !wwinterrupt();
	     i++, touched++) {
		if (!*touched)
			continue;
		if (*touched & WWU_MAJOR && tt.tt_clreol != 0) {
			register gain = 0;
			register best_gain = 0;
			register best;

			wwnmajline++;
			j = wwncol;
			ns = &wwns[i][j];
			os = &wwos[i][j];
			while (--j >= 0) {
				/*
				 * The cost of clearing is:
				 *	ncol - nblank + X
				 * The cost of straight update is:
				 *	ncol - nsame
				 * We clear if:  nblank - nsame > X
				 * X is the clreol overhead.
				 * So we make gain = nblank - nsame.
				 */
				if ((--ns)->c_w == (--os)->c_w)
					gain--;
				else
					best_gain--;
				if (ns->c_w == ' ')
					gain++;
				if (gain >= best_gain) {
					best = j;
					best_gain = gain;
				}
			}
			if (best_gain > 4) {
				(*tt.tt_move)(i, best);
				(*tt.tt_clreol)();
				for (j = wwncol - best, os = &wwos[i][best];
				     --j >= 0;)
					os++->c_w = ' ';
			} else
				wwnmajmiss++;
		}
		*touched = 0;
		wwnupdline++;
		didit = 0;
		ns = wwns[i];
		os = wwos[i];
		for (j = 0; j < wwncol;) {
			register char *p, *q;
			char m;
			int c;
			register n;
			char buf[512];			/* > wwncol */
			union ww_char lastc;

			for (; j++ < wwncol && ns++->c_w == os++->c_w;)
				;
			if (j > wwncol)
				break;
			p = buf;
			m = ns[-1].c_m;
			c = j - 1;
			os[-1] = ns[-1];
			*p++ = ns[-1].c_c;
			n = 5;
			q = p;
			while (j < wwncol && ns->c_m == m) {
				*p++ = ns->c_c;
				if (ns->c_w == os->c_w) {
					if (--n <= 0)
						break;
					os++;
					ns++;
				} else {
					n = 5;
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
					if (q - buf > 1) {
						(*tt.tt_move)(i, c);
						(*tt.tt_write)(buf, q-buf-1);
					}
					os[-1] = lastc;
					*touched = WWU_TOUCHED;
				}
			} else {
				(*tt.tt_move)(i, c);
				(*tt.tt_write)(buf, q - buf);
			}
			didit++;
		}
		if (!didit)
			wwnupdmiss++;
	}
}
