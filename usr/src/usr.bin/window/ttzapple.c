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
static char sccsid[] = "@(#)ttzapple.c	3.2 (Berkeley) %G%";
#endif /* not lint */

#include "ww.h"
#include "tt.h"
#include "char.h"

/*
zz|zapple|unorthodox apple:\
	:am:pt:co#80:li#24:le=^H:nd=^F:up=^K:do=^J:\
	:ho=^[0:ll=^[1:cm=^]%+ %+ =:ch=^\%+ <:cv=^\%+ >:\
	:cl=^[4:ce=^[2:cd=^[3:rp=^]%.%+ @:\
	:so=^[+:se=^[-:\
	:dc=^[c:DC=^\%+ C:ic=^[i:IC=^\%+ I:\
	:al=^[a:AL=^\%+ A:dl=^[d:DL=^\%+ D:\
	:sf=^[f:SF=^\%+ F:sr=^[r:SR=^\%+ R:cs=^]%+ %+ ?:
*/

#define NCOL	80
#define NROW	24

#define pc(c)	ttputc(c)
#define esc()	pc(ctrl('['))
#define esc1()	pc(ctrl('\\'))
#define esc2()	pc(ctrl(']'))

extern short gen_frame[];

/*
 * stuff for token compression
 */

#define N	4
#define NTOKEN	128
#define NCTOKEN	(NTOKEN * 4)
#define H	11
#define HSIZE	(1 << H)
struct ctoken {
	short index;
	short hash;
	unsigned long time;
	unsigned long count;
	char string[N];
	struct ctoken *forw;
	struct ctoken *back;
};

static struct ctoken q1, q2;
static struct ctoken *htab[HSIZE];
static struct ctoken *line[NCOL];
static struct ctoken tokens[NTOKEN * 4];
static unsigned long tick;

#define zc_eval(t)	((int) ((t)->count * 400 + (t)->time - tick))
#define zc_hash(h, c)	((((h) << 1 | (h) >> H - 1) ^ (c)) & HSIZE - 1)
#define zc_unhash(h, c)	(((h) ^ (c) << N - 1 ^ (c) >> H - N + 1) & (HSIZE - 1))
#define zc_copy(f, t)	bcopy(f, t, N)
#define zc_equal(f, t)	(bcmp(f, t, N) == 0)
/*
#define zc_copy(f, t)	((t)[0] = (f)[0], (t)[1] = (f)[1], \
				(t)[2] = (f)[2], (t)[3] = (f)[3])
#define zc_equal(f, t)	((t)[0] == (f)[0] && (t)[1] == (f)[1] && \
				(t)[2] == (f)[2] && (t)[3] == (f)[3])
*/

zz_setmodes(new)
{
	if (new & WWM_REV) {
		if ((tt.tt_modes & WWM_REV) == 0) {
			esc();
			pc('+');
		}
	} else
		if (tt.tt_modes & WWM_REV) {
			esc();
			pc('-');
		}
	tt.tt_modes = new;
}

zz_insline(n)
{
	if (n == 0) {
		esc();
		pc('a');
	} else {
		esc1();
		pc(n + ' ');
		pc('A');
	}
}

zz_delline(n)
{
	if (n == 0) {
		esc();
		pc('d');
	} else {
		esc1();
		pc(n + ' ');
		pc('D');
	}
}

zz_putc(c)
	char c;
{
	if (tt.tt_nmodes != tt.tt_modes)
		zz_setmodes(tt.tt_nmodes);
	ttputc(c);
	if (++tt.tt_col == NCOL)
		tt.tt_col = 0, tt.tt_row++;
}

zz_write(p, n)
	register char *p;
	register n;
{
	if (tt.tt_nmodes != tt.tt_modes)
		zz_setmodes(tt.tt_nmodes);
	if (n < N)
		ttwrite(p, n);
	else
		zc_write(p, n);
	tt.tt_col += n;
	if (tt.tt_col == NCOL)
		tt.tt_col = 0, tt.tt_row++;
}

zz_move(row, col)
	register row, col;
{
	if (tt.tt_row == row) {
		if (tt.tt_col == col)
			return;
		if (col == 0) {
			pc('\r');
			goto out;
		}
		if (tt.tt_col == col - 1) {
			pc(ctrl('f'));
			goto out;
		}
		if (tt.tt_col == col + 1) {
			pc('\b');
			goto out;
		}
		esc1();
		pc(col + ' ');
		pc('<');
		goto out;
	}
	if (tt.tt_col == col) {
		if (tt.tt_row == row + 1) {
			pc(ctrl('k'));
			goto out;
		}
		if (tt.tt_row == row - 1) {
			pc('\n');
			goto out;
		}
		if (col == 0) {
			if (row == 0)
				goto home;
			if (row == NROW - 1)
				goto ll;
		}
		esc1();
		pc(row + ' ');
		pc('>');
		goto out;
	}
	if (col == 0) {
		if (row == 0) {
home:
			esc();
			pc('0');
			goto out;
		}
		if (row == NROW - 1) {
ll:
			esc();
			pc('1');
			goto out;
		}
	}
	esc2();
	pc(' ' + row);
	pc(' ' + col);
	pc('=');
out:
	tt.tt_col = col;
	tt.tt_row = row;
}

zz_init()
{
	zz_setmodes(0);
	zz_setscroll(0, NROW - 1);
	zz_clear();
	esc1();
	pc(N + ' ');
	pc('T');
	zc_init();
}

zz_end()
{
	esc1();
	pc(' ');
	pc('T');
	pc(0);
}

zz_clreol()
{
	esc();
	pc('2');
}

zz_clreos()
{
	esc();
	pc('3');
}

zz_clear()
{
	esc();
	pc('4');
	tt.tt_col = tt.tt_row = 0;
}

zz_inschar(n)
{
	if (n != 1) {
		esc1();
		pc(n + ' ');
		pc('I');
	} else {
		esc();
		pc('i');
	}
}

zz_delchar(n)
{
	if (n != 1) {
		esc1();
		pc(n + ' ');
		pc('C');
	} else {
		esc();
		pc('c');
	}
}

zz_scroll_down(n)
{
	if (n != 1) {
		esc1();
		pc(n + ' ');
		pc('F');
	} else if (tt.tt_row == tt.tt_scroll_bot)
		pc('\n');
	else {
		esc();
		pc('f');
	}
}

zz_scroll_up(n)
{
	if (n == 1) {
		esc();
		pc('r');
	} else {
		esc1();
		pc(n + ' ');
		pc('R');
	}
}

zz_setscroll(top, bot)
{
	esc2();
	pc(top + ' ');
	pc(bot + ' ');
	pc('?');
	tt.tt_scroll_top = top;
	tt.tt_scroll_bot = bot;
}

zc_write(s, n)
	char *s;
	register n;
{
	register char *p;
	register h;
	register i;
	register struct ctoken *tp;

	p = s;
	for (i = N - 2, h = zc_hash(0, *p++); --i >= 0; h = zc_hash(h, *p++))
		;
	for (i = 0;;) {
		tick++;
		h = zc_hash(h, *p++);
		if ((tp = htab[h]) == 0) {
			tp = q2.back;
			if (tp->hash >= 0)
				htab[tp->hash] = 0;
			zc_copy(p - N, tp->string);
			tp->hash = h;
			tp->count = 0;
			htab[h] = tp;
		} else if (!zc_equal(tp->string, p - N)) {
			if (tp->index == 0 && zc_eval(tp) < 0) {
				zc_copy(p - N, tp->string);
				tp->count = 0;
			} else {
				line[i] = 0;
				goto cont;
			}
		}
		tp->time = tick;
		tp->count++;
		if (tp->index == 0)
			zc_head(tp, &q2);
		else
			zc_head(tp, &q1);
		line[i] = tp;
	cont:
		if (++i > n - N)
			break;
		h = zc_unhash(h, p[- N]);
	}
	while (i < n)
		line[i++] = 0;
	for (i = 0; i < n;) {
		register struct ctoken *tp;

		if ((tp = line[i]) == 0) {
			pc(s[i]);
			i++;
		} else if (tp->index > 0) {
			zc_head(tp, &q1);
			pc(tp->index - 1 | 0x80);
			wwzc1++;
			wwzcsave += N - 1;
			i += N;
		} else if (tp->index < 0) {
			tp->index = - tp->index;
			zc_head(tp, &q1);
			pc(ctrl('^'));
			pc(tp->index - 1);
			ttwrite(tp->string, N);
			wwzc0++;
			wwzcsave -= 2;
			i += N;
		} else if (tp->count > 1 && zc_eval(tp) > zc_eval(q1.back)) {
			tp->index = abs(q1.back->index);
			q1.back->index = 0;
			zc_head(q1.back, &q2);
			zc_head(tp, &q1);
			pc(ctrl('^'));
			pc(tp->index - 1);
			ttwrite(tp->string, N);
			wwzc0++;
			wwzcsave -= 2;
			i += N;
		} else {
			pc(s[i]);
			i++;
		}
	}
	wwzctotal += n;
}

zc_head(tp, q)
	register struct ctoken *tp, *q;
{

	tp->back->forw = tp->forw;
	tp->forw->back = tp->back;
	q->forw->back = tp;
	tp->forw = q->forw;
	q->forw = tp;
	tp->back = q;
}

zc_init()
{
	register struct ctoken *tp;

	for (tp = tokens; tp < tokens + sizeof tokens / sizeof *tokens; tp++)
		if (tp->index > 0)
			tp->index = - tp->index;
}

zc_start()
{
	register struct ctoken *tp;
	register i;

	tick = 0;
	bzero((char *)htab, sizeof htab);
	q1.forw = &q1;
	q1.back = &q1;
	for (i = 0, tp = tokens; i < NTOKEN; i++, tp++) {
		tp->index = i + 1;
		tp->hash = -1;
		tp->count = 0;
		tp->time = 0;
		q1.forw->back = tp;
		tp->forw = q1.forw;
		q1.forw = tp;
		tp->back = &q1;
	}
	q2.forw = &q2;
	q2.back = &q2;
	for (; i < sizeof tokens / sizeof *tokens; i++, tp++) {
		tp->index = 0;
		tp->hash = -1;
		tp->count = 0;
		tp->time = 0;
		q2.forw->back = tp;
		tp->forw = q2.forw;
		q2.forw = tp;
		tp->back = &q2;
	}
}

tt_zapple()
{
	zc_start();
	tt.tt_inschar = zz_inschar;
	tt.tt_delchar = zz_delchar;
	tt.tt_insline = zz_insline;
	tt.tt_delline = zz_delline;
	tt.tt_clreol = zz_clreol;
	tt.tt_clreos = zz_clreos;
	tt.tt_scroll_down = zz_scroll_down;
	tt.tt_scroll_up = zz_scroll_up;
	tt.tt_setscroll = zz_setscroll;
	tt.tt_availmodes = WWM_REV;
	tt.tt_wrap = 1;
	tt.tt_retain = 0;
	tt.tt_ncol = 80;
	tt.tt_nrow = 24;
	tt.tt_init = zz_init;
	tt.tt_end = zz_end;
	tt.tt_write = zz_write;
	tt.tt_putc = zz_putc;
	tt.tt_move = zz_move;
	tt.tt_clear = zz_clear;
	tt.tt_setmodes = zz_setmodes;
	tt.tt_frame = gen_frame;
	return 0;
}
