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
static char sccsid[] = "@(#)wwupdate.c	3.20 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"

wwupdate1(top, bot)
{
	int i;
	register j;
	char *touched;
	struct ww_update *upd;
	char didit;
	char check_clreos = 0;
	int scan_top, scan_bot;

	wwnupdate++;
	{
		register char *t1 = wwtouched + top, *t2 = wwtouched + bot;
		register n;

		while (!*t1++)
			if (t1 == t2)
				return;
		while (!*--t2)
			;
		scan_top = top = t1 - wwtouched - 1;
		scan_bot = bot = t2 - wwtouched + 1;
		if (tt.tt_clreos != 0 || tt.tt_clear != 0) {
			int st = tt.tt_clreos == 0 ? scan_top : 0;

			for (n = 1; t1 < t2;)
				if (*t1++)
					n++;
			if (check_clreos = n * 10 > (wwnrow - st) * 9) {
				scan_top = st;
				scan_bot = wwnrow;
			}
		}
	}
	if (tt.tt_clreol == 0 && !check_clreos)
		goto simple;
	for (i = scan_top, touched = &wwtouched[i], upd = &wwupd[i];
	     i < scan_bot;
	     i++, touched++, upd++) {
		register gain = 0;
		register best_gain = 0;
		register best_col;
		register union ww_char *ns, *os;

		if (wwinterrupt())
			return;
		if (!check_clreos && !*touched)
			continue;
		wwnupdscan++;
		j = wwncol;
		ns = &wwns[i][j];
		os = &wwos[i][j];
		while (--j >= 0) {
			/*
			 * The cost of clearing is:
			 *	ncol - nblank + X
			 * The cost of straight update is, more or less:
			 *	ncol - nsame
			 * We clear if  nblank - nsame > X
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
				best_col = j;
				best_gain = gain;
			}
		}
		upd->best_gain = best_gain;
		upd->best_col = best_col;
		upd->gain = gain;
	}
	if (check_clreos) {
		register struct ww_update *u;
		register gain = 0;
		register best_gain = 0;
		int best_row;
		register simple_gain = 0;
		char didit = 0;

		for (j = scan_bot - 1, u = wwupd + j; j >= top; j--, u--) {
			register g = gain + u->best_gain;

			if (g >= best_gain) {
				best_gain = g;
				best_row = j;
			}
			gain += u->gain;
			if (tt.tt_clreol != 0 && u->best_gain > 4)
				simple_gain += u->best_gain - 4;
		}
		if (tt.tt_clreos == 0) {
			if (gain > simple_gain && gain > 4) {
				(*tt.tt_clear)();
				i = top = scan_top;
				bot = scan_bot;
				j = 0;
				didit = 1;
			}
		} else
			if (best_gain > simple_gain && best_gain > 4) {
				i = best_row;
				(*tt.tt_move)(i, j = wwupd[i].best_col);
				(*tt.tt_clreos)();
				bot = scan_bot;
				didit = 1;
			}
		if (didit) {
			wwnupdclreos++;
			wwnupdclreosline += wwnrow - i;
			u = wwupd + i;
			while (i < scan_bot) {
				register union ww_char *os = &wwos[i][j];

				for (j = wwncol - j; --j >= 0;)
					os++->c_w = ' ';
				wwtouched[i++] = WWU_TOUCHED;
				u++->best_gain = 0;
				j = 0;
			}
		} else
			wwnupdclreosmiss++;
	}
simple:
	for (i = top, touched = &wwtouched[i], upd = &wwupd[i]; i < bot;
	     i++, touched++, upd++) {
		register union ww_char *os, *ns;
		char didit;

		if (!*touched)
			continue;
		*touched = 0;
		wwnupdline++;
		didit = 0;
		if (tt.tt_clreol != 0 && upd->best_gain > 4) {
			wwnupdclreol++;
			(*tt.tt_move)(i, j = upd->best_col);
			(*tt.tt_clreol)();
			for (os = &wwos[i][j], j = wwncol - j; --j >= 0;)
				os++->c_w = ' ';
			didit = 1;
		}
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
			didit = 1;
		}
		if (!didit)
			wwnupdmiss++;
	}
}
