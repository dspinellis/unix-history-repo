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
static char sccsid[] = "@(#)compress.c	3.2 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "xx.h"
#include "tt.h"

	/* tunable parameters */

int xc_reverse = 1;
int xc_sort = 0;
int xc_chop = 0;

int xc_token_max = 8;		/* <= TOKEN_MAX */
int xc_token_min = 2;		/* > tt.tt_put_token_cost */
int xc_npass0 = 1;
int xc_npass1 = 1;

int xc_bufsize = 1024 * 3;	/* XXX, or 80 * 24 * 2 */

int xc_ntoken = 8192;

#define xc_weight XXX
#ifndef xc_weight
int xc_weight = 0;
#endif

#define TOKEN_MAX 16

struct xc {
	char string[TOKEN_MAX];
	char length;
	char flag;
#ifndef xc_weight
	short weight;
#endif
	long time;		/* time last seen */
	short bcount;		/* count in this buffer */
	short ccount;		/* count in compression */
	short places;		/* places in the buffer */
	short code;		/* token code */
	struct xc *qforw, *qback;
	struct xc *hforw, **hback;
};

short xc_thresholds[TOKEN_MAX + 1];
#define thresh(length) (xc_thresholds[length])
#define threshp(code, count, length) \
	((code) >= 0 || (short) (count) >= xc_thresholds[length])

#ifndef xc_weight
short xc_wthresholds[TOKEN_MAX + 1];
#define wthresh(length) (xc_wthresholds[length])
#define wthreshp(weight, length) ((short) (weight) >= xc_wthresholds[length])
#else
#define wthreshp(weight, length) (0)
#endif

#ifndef xc_weight
short xc_wlimits[TOKEN_MAX + 1];
#define wlimit(length) (xc_wlimits[length])
#endif

#define put_token_score(length) ((length) - tt.tt_put_token_cost)

int xc_score_adjustments[TOKEN_MAX + 1][8]; /* XXX, 8 > max of xc_thresholds */
#define score_adjust(score, p) \
	do { \
		int length = (p)->length; \
		int ccount = (p)->ccount; \
		if (threshp((p)->code, ccount, length) || \
		    wthreshp((p)->weight, length)) /* XXX */ \
			(score) -= length - tt.tt_put_token_cost; \
		else \
			(score) += xc_score_adjustments[length][ccount]; \
	} while (0)

int xc_initial_scores[TOKEN_MAX + 1][8]; /* XXX, 8 > max of xc_thresholds */

struct xc xc_q0a, xc_q0b, xc_q1a, xc_q1b;

#define qinsert(x1, x2) \
	do { \
		register struct xc *forw = (x1)->qforw; \
		register struct xc *back = (x1)->qback; \
		back->qforw = forw; \
		forw->qback = back; \
		forw = (x2)->qforw; \
		(x1)->qforw = forw; \
		forw->qback = (x1); \
		(x2)->qforw = (x1); \
		(x1)->qback = (x2); \
	} while (0)

#define qinsertq(q, x) \
	((q)->qforw == (q) ? 0 : \
	 ((q)->qback->qforw = (x)->qforw, \
	  (x)->qforw->qback = (q)->qback, \
	  (q)->qforw->qback = (x), \
	  (x)->qforw = (q)->qforw, \
	  (q)->qforw = (q), \
	  (q)->qback = (q)))

#define H		(14)
#define HSIZE		(1 << H)
#define hash(h, c)	((((h) >> H - 8 | (h) << 8) ^ (unsigned char)(c)) & \
				HSIZE - 1)

struct xc **xc_output;			/* the output array */
short *xc_places[TOKEN_MAX + 1];
short *xc_hashcodes;			/* for computing hashcodes */
struct xc **xc_htab;			/* the hash table */
struct xc **xc_tokens;			/* holds all the active tokens */
struct xc_undo {
	struct xc **pos;
	struct xc *val;
} *xc_undo;

long xc_time, xc_time0;

xcinit()
{
	register i, j;
	register struct xc *p;

	if (tt.tt_token_max > xc_token_max)
		tt.tt_token_max = xc_token_max;
	if (tt.tt_token_min < xc_token_min)
		tt.tt_token_min = xc_token_min;
	if (tt.tt_token_min > tt.tt_token_max) {
		tt.tt_ntoken = 0;
		return 0;
	}
	if (tt.tt_ntoken > xc_ntoken / 2)	/* not likely */
		tt.tt_ntoken = xc_ntoken / 2;
	if (xxbufsize > xc_bufsize)
		xxbufsize = xc_bufsize;		/* XXX */
#define C(x) (sizeof (x) / sizeof *(x))
	for (i = 0; i < C(xc_thresholds); i++) {
		int h = i - tt.tt_put_token_cost;
		if (h > 0)
			xc_thresholds[i] =
				(tt.tt_set_token_cost + 1 + h - 1) / h + 1;
		else
			xc_thresholds[i] = 0;
	}
	for (i = 0; i < C(xc_score_adjustments); i++) {
		int t = xc_thresholds[i];
		for (j = 0; j < C(*xc_score_adjustments); j++) {
			if (j >= t)
				xc_score_adjustments[i][j] =
					- (i - tt.tt_put_token_cost);
			else if (j < t - 1)
				xc_score_adjustments[i][j] = 0;
			else
				/*
				 * cost now is
				 *	length * (ccount + 1)		a
				 * cost before was
				 *	set-token-cost + length +
				 *		ccount * put-token-cost	b
				 * the score adjustment is (b - a)
				 */
				xc_score_adjustments[i][j] =
					tt.tt_set_token_cost + i +
						j * tt.tt_put_token_cost -
							i * (j + 1);
			if (j >= t)
				xc_initial_scores[i][j] = 0;
			else
				/*
				 * - (set-token-cost +
				 *	(length - put-token-cost) -
				 *	(length - put-token-cost) * ccount)
				 */
				xc_initial_scores[i][j] =
					- (tt.tt_set_token_cost +
					   (i - tt.tt_put_token_cost) -
					   (i - tt.tt_put_token_cost) * j);
		}
	}
#ifndef xc_weight
	for (i = 1; i < C(xc_wthresholds); i++) {
		xc_wthresholds[i] =
			((tt.tt_set_token_cost + tt.tt_put_token_cost) / i +
				i / 5 + 1) *
				xc_weight + 1;
		xc_wlimits[i] = xc_wthresholds[i] + xc_weight;
	}
#endif
#undef C
	if ((xc_output = (struct xc **)
	     malloc((unsigned) xxbufsize * sizeof *xc_output)) == 0)
		goto nomem;
	if ((xc_hashcodes = (short *)
	     malloc((unsigned) xxbufsize * sizeof *xc_hashcodes)) == 0)
		goto nomem;
	if ((xc_htab = (struct xc **) malloc(HSIZE * sizeof *xc_htab)) == 0)
		goto nomem;
	if ((xc_tokens = (struct xc **)
	     malloc((unsigned)
	            (xc_ntoken + tt.tt_token_max - tt.tt_token_min + 1) *
		    sizeof *xc_tokens)) == 0)
		goto nomem;
	if ((xc_undo = (struct xc_undo *)
	     malloc((unsigned) xxbufsize * sizeof *xc_undo)) == 0)
		goto nomem;
	for (i = tt.tt_token_min; i <= tt.tt_token_max; i++)
		if ((xc_places[i] = (short *)
		     malloc((unsigned) xxbufsize * sizeof **xc_places)) == 0)
			goto nomem;
	xc_q0a.qforw = xc_q0a.qback = &xc_q0a;
	xc_q0b.qforw = xc_q0b.qback = &xc_q0b;
	xc_q1a.qforw = xc_q1a.qback = &xc_q1a;
	xc_q1b.qforw = xc_q1b.qback = &xc_q1b;
	if ((p = (struct xc *) malloc((unsigned) xc_ntoken * sizeof *p)) == 0)
		goto nomem;
	for (i = 0; i < tt.tt_ntoken; i++) {
		p->code = i;
		p->time = -1;
		p->qback = xc_q0a.qback;
		p->qforw = &xc_q0a;
		p->qback->qforw = p;
		xc_q0a.qback = p;
		p++;
	}
	for (; i < xc_ntoken; i++) {
		p->code = -1;
		p->time = -1;
		p->qback = xc_q1a.qback;
		p->qforw = &xc_q1a;
		p->qback->qforw = p;
		xc_q1a.qback = p;
		p++;
	}
	return 0;
nomem:
	wwerrno = WWE_NOMEM;
	return -1;
}

xcstart()
{
	register struct xc *p;

	bzero((char *) xc_htab, HSIZE * sizeof *xc_htab);
	for (p = xc_q0a.qforw; p != &xc_q0a; p = p->qforw)
		p->hback = 0;
	for (p = xc_q1a.qforw; p != &xc_q1a; p = p->qforw)
		p->hback = 0;
}

xcscan(buffer, bufsize)
	char *buffer;
{
	int n;

	if (bufsize <= tt.tt_token_min)		/* one more for char_sep */
		return;
	xc_time0 = xc_time;
	xc_time += bufsize;
#ifdef STATS
	if (verbose >= 0)
		time_begin();
#endif
	n = xc_sweep_phase(buffer, bufsize, xc_tokens);
#ifdef STATS
	if (verbose >= 0) {
		time_end();
		time_begin();
	}
#endif
	xc_compress_phase(xc_output, bufsize, xc_tokens, n);
#ifdef STATS
	if (verbose >= 0)
		time_end();
#endif
}

xc_sweep_phase(buffer, bufsize, tokens)
	char *buffer;
	struct xc **tokens;
{
	register struct xc **pp = tokens;
	register i, n;
#ifdef STATS
	int nn, ii;
#endif

#ifdef STATS
	if (verbose > 0)
		printf("Sweep:");
#endif
	xc_sweep0(buffer, bufsize, tt.tt_token_min - 1);
#ifdef STATS
	xc_ntoken_stat = 0;
	nn = 0;
	ii = 0;
#endif
	for (i = tt.tt_token_min; i <= tt.tt_token_max; i++) {
#ifdef STATS
		if (verbose > 0) {
			if (ii > 7) {
				printf("\n      ");
				ii = 0;
			}
			ii++;
			printf(" (%d", i);
			(void) fflush(stdout);
		}
#endif
		n = xc_sweep(buffer, bufsize, pp, i);
		pp += n;
#ifdef STATS
		if (verbose > 0) {
			if (--n > 0) {
				printf(" %d", n);
				nn += n;
			}
			putchar(')');
		}
#endif
	}
	qinsertq(&xc_q1b, &xc_q1a);
#ifdef STATS
	if (verbose > 0)
		printf("\n       %d tokens, %d candidates\n",
			xc_ntoken_stat, nn);
#endif
	return pp - tokens;
}

xc_sweep0(buffer, n, length)
	char *buffer;
	register n;
	register length;
{
	register i;
	register char *p = buffer;
	register short *hc = xc_hashcodes;
	register h;

	if (--length == 0)
		do {
#ifdef char_sep
			if ((*hc++ = *p++) == char_sep)
				hc[-1] = -1;
#else
			*hc++ = *p++;
#endif
		} while (--n);
	else
		for (n -= length; --n >= 0;) {
#ifdef char_sep
			if (*p == char_sep) {
				*hc++ = -1;
				p++;
				continue;
			}
#endif
			h = *p++;
			for (i = length; --i >= 0;) {
#ifdef char_sep
				if (*p == char_sep) {
					h = -1;
					p += i + 1;
					break;
				}
#endif
				h = hash(h, *p++);
			}
			*hc++ = h;
			p -= length;
		}
}

xc_sweep(buffer, bufsize, tokens, length)
	char *buffer;
	struct xc **tokens;
	register length;
{
	register struct xc *p;
	register char *cp;
	register i;
	short *hc;
	short *places = xc_places[length];
	struct xc **pp = tokens;
	short threshold = thresh(length);
#ifndef xc_weight
	short wthreshold = wthresh(length);
	short limit = wlimit(length);
#endif
	int time;

	i = length - 1;
	bufsize -= i;
	cp = buffer + i;
	hc = xc_hashcodes;
	time = xc_time0;
	for (i = 0; i < bufsize; i++, time++) {
		struct xc **h;

		{
			register short *hc1 = hc;
#ifdef char_sep
			if (*hc1 < 0 || *cp == char_sep) {
				*hc1++ = -1;
				hc = hc1;
				cp++;
				continue;
			}
#endif
			h = xc_htab + (*hc1 = hash(*hc1, *cp++));
			hc = hc1 + 1;
		}
		for (p = *h; p != 0; p = p->hforw)
			if (p->length == (char) length) {
				register char *p1 = p->string;
				register char *p2 = cp - length;
				register n = length;
				do
					if (*p1++ != *p2++)
						goto fail;
				while (--n);
				break;
			fail:;
			}
		if (p == 0) {
			p = xc_q1a.qback;
			if (p == &xc_q1a ||
			    p->time >= xc_time0 && p->length == (char) length)
				continue;
			if (p->hback != 0)
				if ((*p->hback = p->hforw) != 0)
					p->hforw->hback = p->hback;
			{
				register char *p1 = p->string;
				register char *p2 = cp - length;
				register n = length;
				do
					*p1++ = *p2++;
				while (--n);
			}
			p->length = length;
#ifndef xc_weight
			p->weight = xc_weight;
#endif
			p->time = time;
			p->bcount = 1;
			p->ccount = 0;
			p->flag = 0;
			if ((p->hforw = *h) != 0)
				p->hforw->hback = &p->hforw;
			*h = p;
			p->hback = h;
			qinsert(p, &xc_q1a);
			places[i] = -1;
			p->places = i;
#ifdef STATS
			xc_ntoken_stat++;
#endif
		} else if (p->time < xc_time0) {
#ifndef xc_weight
			if ((p->weight += p->time - time) < 0)
				p->weight = xc_weight;
			else if ((p->weight += xc_weight) > limit)
				p->weight = limit;
#endif
			p->time = time;
			p->bcount = 1;
			p->ccount = 0;
			if (p->code >= 0) {
				p->flag = 1;
				*pp++ = p;
			} else
#ifndef xc_weight
			if (p->weight >= wthreshold) {
				p->flag = 1;
				*pp++ = p;
				qinsert(p, &xc_q1b);
			} else
#endif
			{
				p->flag = 0;
				qinsert(p, &xc_q1a);
			}
			places[i] = -1;
			p->places = i;
#ifdef STATS
			xc_ntoken_stat++;
#endif
		} else if (p->time + length > time) {
			/*
			 * overlapping token, don't count as two and
			 * don't update time, but do adjust weight to offset
			 * the difference
			 */
#ifndef xc_weight
			if (xc_weight != 0) {	/* XXX */
				p->weight += time - p->time;
				if (!p->flag && p->weight >= wthreshold) {
					p->flag = 1;
					*pp++ = p;
					qinsert(p, &xc_q1b);
				}
			}
#endif
			places[i] = p->places;
			p->places = i;
		} else {
#ifndef xc_weight
			if ((p->weight += p->time - time) < 0)
				p->weight = xc_weight;
			else if ((p->weight += xc_weight) > limit)
				p->weight = limit;
#endif
			p->time = time;
			p->bcount++;
			if (!p->flag &&
			    /* code must be < 0 if flag false here */
			    (p->bcount >= threshold
#ifndef xc_weight
			     || p->weight >= wthreshold
#endif
			     )) {
				p->flag = 1;
				*pp++ = p;
				qinsert(p, &xc_q1b);
			}
			places[i] = p->places;
			p->places = i;
		}
	}
	if ((i = pp - tokens) > 0) {
		*pp = 0;
		if (xc_reverse)
			xc_sweep_reverse(tokens, places);
		if (xc_sort && i > 1) {
			int xc_token_compare();
			qsort((char *) tokens, i, sizeof *tokens,
			      xc_token_compare);
		}
		if (xc_chop) {
			if ((i = i * xc_chop / 100) == 0)
				i = 1;
			tokens[i] = 0;
		}
		i++;
	}
	return i;
}

xc_sweep_reverse(pp, places)
	register struct xc **pp;
	register short *places;
{
	register struct xc *p;
	register short front, back, t;

	while ((p = *pp++) != 0) {
		back = -1;
		t = p->places;
		/* the list is never empty */
		do {
			front = places[t];
			places[t] = back;
			back = t;
		} while ((t = front) >= 0);
		p->places = back;
	}
}

xc_compress_phase(output, bufsize, tokens, ntoken)
	struct xc **output;
	struct xc **tokens;
{
	register i;

	bzero((char *) output, bufsize * sizeof *output);
	for (i = 0; i < xc_npass0; i++)
		xc_compress_phase1(output, tokens, ntoken, 0);
	for (i = 0; i < xc_npass1; i++)
		xc_compress_phase1(output, tokens, ntoken, 1);
	xc_compress_cleanup(output, bufsize);
}

xc_compress_phase1(output, tokens, ntoken, flag)
	register struct xc **output;
	struct xc **tokens;
{
	register int i = 0;
	register struct xc **pp;

#ifdef STATS
	if (verbose > 0)
		printf("Compress:");
#endif
	pp = tokens;
	while (pp < tokens + ntoken) {
#ifdef STATS
		if (verbose > 0) {
			xc_ntoken_stat = 0;
			xc_ccount_stat = 0;
			xc_ncover_stat = 0;
			if (i > 2) {
				printf("\n         ");
				i = 0;
			}
			i++;
			printf(" (%d", (*pp)->length);
			(void) fflush(stdout);
		}
#endif
		pp += xc_compress(output, pp, flag);
#ifdef STATS
		if (verbose > 0)
			printf(" %dt %du %dc)", xc_ntoken_stat, xc_ccount_stat,
			       xc_ncover_stat);
#endif
	}
#ifdef STATS
	if (verbose > 0)
		printf("\n");
#endif
}

xc_compress_cleanup(output, bufsize)
	register struct xc **output;
{
	register struct xc **end;

	/* the previous output phase may have been interrupted */
	qinsertq(&xc_q0b, &xc_q0a);
	for (end = output + bufsize; output < end;) {
		register struct xc *p;
		register length;
		if ((p = *output) == 0) {
			output++;
			continue;
		}
		length = p->length;
		if (!p->flag) {
		} else if (p->code >= 0) {
			qinsert(p, &xc_q0b);
			p->flag = 0;
		} else if (p->ccount == 0) {
			*output = 0;
		} else if (p->ccount >= thresh(length)
#ifndef xc_weight
			   || wthreshp(p->weight, length)
#endif
			   ) {
			p->flag = 0;
		} else {
			p->ccount = 0;
			*output = 0;
		}
		output += length;
	}
}

xc_compress(output, tokens, flag)
	struct xc **output;
	struct xc **tokens;
	char flag;
{
	struct xc **pp = tokens;
	register struct xc *p = *pp++;
	int length = p->length;
	int threshold = thresh(length);
#ifndef xc_weight
	short wthreshold = wthresh(length);
#endif
	short *places = xc_places[length];
	int *initial_scores = xc_initial_scores[length];
	int initial_score0 = put_token_score(length);

	do {
		int score;
		register struct xc_undo *undop;
		int ccount;
#ifdef STATS
		int ncover;
#endif
		int i;

		ccount = p->ccount;
		if ((short) ccount >= p->bcount)
			continue;
		if (p->code >= 0 || ccount >= threshold)
			score = 0;
#ifndef xc_weight
		else if (p->weight >= wthreshold)
			/* allow one fewer match than normal */
			/* XXX, should adjust for ccount */
			score = - tt.tt_set_token_cost;
#endif
		else
			score = initial_scores[ccount];
		undop = xc_undo;
#ifdef STATS
		ncover = 0;
#endif
		for (i = p->places; i >= 0; i = places[i]) {
			register struct xc **jp;
			register struct xc *x;
			register struct xc **ip = output + i;
			register score0 = initial_score0;
			struct xc **iip = ip + length;
			struct xc_undo *undop1 = undop;

			if ((x = *(jp = ip)) != 0)
				goto z;
			while (--jp >= output)
				if ((x = *jp) != 0) {
					if (jp + x->length > ip)
						goto z;
					break;
				}
			jp = ip + 1;
			while (jp < iip) {
				if ((x = *jp) == 0) {
					jp++;
					continue;
				}
			z:
				if (x == p)
					goto undo;
#ifdef STATS
				ncover++;
#endif
				undop->pos = jp;
				undop->val = x;
				undop++;
				*jp = 0;
				x->ccount--;
				score_adjust(score0, x);
				if (score0 < 0 && flag)
					goto undo;
				jp += x->length;
			}
			undop->pos = ip;
			undop->val = 0;
			undop++;
			*ip = p;
			ccount++;
			score += score0;
			continue;
		undo:
			while (--undop >= undop1)
				if (*undop->pos = x = undop->val)
					x->ccount++;
			undop++;
		}
		if (score > 0) {
#ifdef STATS
			xc_ccount_stat += ccount - p->ccount;
			xc_ntoken_stat++;
			xc_ncover_stat += ncover;
#endif
			p->ccount = ccount;
		} else {
			register struct xc_undo *u = xc_undo;
			while (--undop >= u) {
				register struct xc *x;
				if (*undop->pos = x = undop->val)
					x->ccount++;
			}
		}
	} while ((p = *pp++) != 0);
	return pp - tokens;
}

xcwrite(s, n, x)
	register char *s;
	register n;
{
	register i;
	register struct xc *p, *p1;
	register struct xc **output = xc_output + x;

	if (n < tt.tt_token_min) {
		(*tt.tt_write)(s, n);
		return;
	}
	for (i = 0; i < n;) {
		if ((p = output[i]) == 0) {
			(*tt.tt_putc)(s[i]);
			i++;
		} else if (p->code >= 0) {
			if (--p->ccount == 0)
				qinsert(p, &xc_q0a);
			(*tt.tt_put_token)(p->code, p->string, p->length);
			wwntokuse++;
			wwntoksave += put_token_score(p->length);
			i += p->length;
		} else if ((p1 = xc_q0a.qback) != &xc_q0a) {
			p->code = p1->code;
			p1->code = -1;
			qinsert(p1, &xc_q1a);
			if (--p->ccount == 0)
				qinsert(p, &xc_q0a);
			else
				qinsert(p, &xc_q0b);
			(*tt.tt_set_token)(p->code, p->string, p->length);
			wwntokdef++;
			wwntoksave -= tt.tt_set_token_cost;
			i += p->length;
		} else {
			p->ccount--;
			(*tt.tt_write)(p->string, p->length);
			wwntokbad++;
			i += p->length;
		}
	}
	wwntokc += n;
}

xc_token_compare(p1, p2)
	struct xc **p1, **p2;
{
	return (*p2)->bcount - (*p1)->bcount;
}
