/*
 * Copyright (c) 1989 Regents of the University of California.
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
static char sccsid[] = "@(#)compress.c	3.1 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "xx.h"
#include "tt.h"

	/* tunable constants */
int xc_token_size = 2;
int xc_weight = 1024;
int xc_thresh;
int xc_limit;

	/* fundamental constants */
#define TOKEN_MAX	32
#define NTOKEN		128
#define NCTOKEN		(NTOKEN * 32)	/* should be at least xxbufsize */
#define H		13
#define HSIZE		(1 << H)	/* at least twice NCTOKEN */

struct xc_token {
	long time;
	long weight;
	char string[TOKEN_MAX];
	short index;
	struct xc_token *forw;
	struct xc_token *back;
	struct xc_token *hforw;
	struct xc_token **hback;
};

struct xc_token xc_q1, xc_q2;
struct xc_token **xc_htab;
struct xc_token **xc_line;
long xc_time;
long xc_time0;

#define xc_hs1 3
#define xc_hs2 (H - 3)
int xc_uhs1, xc_uhs2;
#define xchash(h, c)	((((h) << xc_hs1 | (h) >> xc_hs2) ^ (c)) & HSIZE - 1)
#define xcunhash(h, c)	(((h) ^ (c) << xc_uhs1 ^ (c) >> xc_uhs2) & HSIZE - 1)
#define xccopy(f, t)	bcopy(f, t, xc_token_size)
#define xcequal(f, t)	(bcmp(f, t, xc_token_size) == 0)
#define xcinsert(t1, t2) \
			((t1)->back->forw = (t1)->forw, \
			(t1)->forw->back = (t1)->back, \
			(t2)->forw->back = (t1), \
			(t1)->forw = (t2)->forw, \
			(t2)->forw = (t1), \
			(t1)->back = (t2))

xcinit()
{
	register struct xc_token *tp;
	register i;

	if (xc_token_size < 2) {
		tt.tt_ntoken = 0;
		return;
	}
	xc_thresh = xc_weight *
		(1 + (tt.tt_set_token_cost + tt.tt_put_token_cost) /
			xc_token_size);
	xc_limit = xc_thresh + xc_weight;
	if (tt.tt_token_max > TOKEN_MAX)
		tt.tt_token_max = TOKEN_MAX;
	xc_uhs1 = (xc_token_size - 1) * xc_hs1 % H;
	xc_uhs2 = H - xc_uhs1;
	xc_time = 0;
	xc_htab = (struct xc_token **) calloc(HSIZE, sizeof *xc_htab);
	xc_line = (struct xc_token **)
		malloc((unsigned) xxbufsize * sizeof *xc_line);
	if (xc_line == 0)
		goto nomem;
	tp = (struct xc_token *) malloc(NCTOKEN * sizeof *tp);
	if (tp == 0)
		goto nomem;
	xc_q1.forw = &xc_q1;
	xc_q1.back = &xc_q1;
	for (i = 0; i < NTOKEN; i++, tp++) {
		tp->index = i + 1;
		tp->weight = 0;
		tp->time = -1;
		xc_q1.forw->back = tp;
		tp->forw = xc_q1.forw;
		xc_q1.forw = tp;
		tp->back = &xc_q1;
		tp->hback = 0;
	}
	xc_q2.forw = &xc_q2;
	xc_q2.back = &xc_q2;
	for (; i < NCTOKEN; i++, tp++) {
		tp->index = 0;
		tp->weight = 0;
		tp->time = -1;
		xc_q2.forw->back = tp;
		tp->forw = xc_q2.forw;
		xc_q2.forw = tp;
		tp->back = &xc_q2;
		tp->hback = 0;
	}
	return 0;
nomem:
	wwerrno = WWE_NOMEM;
	return -1;
}

xcstart()
{
	register struct xc_token *tp;

	for (tp = xc_q1.forw; tp != &xc_q1; tp = tp->forw)
		if (tp->index > 0)
			tp->index = - tp->index;
}

xcreset()
{
	xc_time0 = xc_time;
}

xcscan(s, n, x)
	char *s;
	register n;
{
	register char *p;
	register h;
	register i;
	register struct xc_token *tp;
	register struct xc_token **lp = xc_line + x;

	if (n < xc_token_size)
		return;
	p = s;
	for (i = xc_token_size - 2, h = xchash(0, *p++); --i >= 0;
	     h = xchash(h, *p++))
		;
	for (i = n - xc_token_size;;) {
		xc_time++;
		h = xchash(h, *p++);
		for (tp = xc_htab[h];
		     tp != 0 && !xcequal(tp->string, p - xc_token_size);
		     tp = tp->hforw)
			;
		if (tp == 0) {
			tp = xc_q2.back;
			if (tp->time >= xc_time0) {
				*lp = 0;
				goto cont;
			}
			if (tp->hback != 0 && (*tp->hback = tp->hforw) != 0)
				tp->hforw->hback = tp->hback;
			xccopy(p - xc_token_size, tp->string);
			if ((tp->hforw = xc_htab[h]) != 0)
				tp->hforw->hback = &tp->hforw;
			tp->hback = xc_htab + h;
			xc_htab[h] = tp;
			tp->weight = xc_weight;
		} else {
			/*
			 * Don't count overlapping tokens as two,
			 * and don't update the time so a token that overlaps
			 * this but not the previous will be counted,.
			 * but do fix up the weight to compensate for
			 * the incorrect time.
			 */
			if (tp->time > xc_time - xc_token_size)
				tp->weight += xc_time - tp->time;
			else if ((tp->weight += tp->time - xc_time) < 0)
				tp->weight = xc_weight;
			else if ((tp->weight += xc_weight) > xc_limit)
				tp->weight = xc_limit;
		}
		tp->time = xc_time;
		if (tp->index == 0)
			xcinsert(tp, &xc_q2);
		else
			xcinsert(tp, &xc_q1);
		*lp = tp;
	cont:
		lp++;
		if (--i < 0)
			break;
		h = xcunhash(h, p[- xc_token_size]);
	}
	for (i = xc_token_size - 1; --i >= 0;)
		*lp++ = 0;
}

xcwrite(s, n, x)
	register char *s;
	register n;
{
	register i;
	register struct xc_token **lp = xc_line + x;
	register struct xc_token *tp;
	register struct xc_token *tq;

	if (n < xc_token_size) {
		(*tt.tt_write)(s, n);
		return;
	}
	for (i = 0; i < n;) {
		if ((tp = lp[i]) == 0) {
			(*tt.tt_putc)(s[i]);
			i++;
		} else if (tp->index > 0) {
			xcinsert(tp, &xc_q1);
			(*tt.tt_put_token)(tp->index - 1, tp->string,
				xc_token_size);
			wwntokuse++;
			wwntoksave += xc_token_size - tt.tt_put_token_cost;
			i += xc_token_size;
		} else if (tp->index < 0) {
			tp->index = - tp->index;
			xcinsert(tp, &xc_q1);
			(*tt.tt_set_token)(tp->index - 1, tp->string,
				xc_token_size);
			wwntokdef++;
			wwntoksave -= tt.tt_set_token_cost;
			i += xc_token_size;
		} else if (tp->weight > xc_thresh &&
			   ((tq = xc_q1.back)->index < 0 ||
			    tq->weight + (tq->time - tp->time) + xc_thresh <
			    tp->weight)) {
			if ((tp->index = tq->index) < 0)
				tp->index = - tp->index;
			tq->index = 0;
			xcinsert(tq, &xc_q2);
			xcinsert(tp, &xc_q1);
			(*tt.tt_set_token)(tp->index - 1, tp->string,
				xc_token_size);
			wwntokdef++;
			wwntoksave -= tt.tt_set_token_cost;
			i += xc_token_size;
		} else {
			(*tt.tt_putc)(s[i]);
			i++;
		}
	}
	wwntokc += n;
}
