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
static char sccsid[] = "@(#)ttzapple.c	3.1 (Berkeley) %G%";
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

#define ZZ_SETINSERT(new) (tt.tt_insert = new)

extern short gen_frame[];

zz_setinsert(new)
{
	ZZ_SETINSERT(new);
}

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

zz_insline()
{
	esc();
	pc('a');
}

zz_delline()
{
	esc();
	pc('d');
}

zz_putc(c)
	char c;
{
	if (tt.tt_ninsert != tt.tt_insert)
		ZZ_SETINSERT(tt.tt_ninsert);
	if (tt.tt_nmodes != tt.tt_modes)
		zz_setmodes(tt.tt_nmodes);
	if (tt.tt_insert) {
		esc();
		pc('i');
		ttputc(c);
	} else
		ttputc(c);
	if (++tt.tt_col == NCOL)
		tt.tt_col = 0, tt.tt_row++;
}

int zz_histo[127];

zz_write(p, n)
	register char *p;
	register n;
{
	if (n < 128)
		zz_histo[n]++;
	else
		zz_histo[127]++;
	if (tt.tt_ninsert != tt.tt_insert)
		ZZ_SETINSERT(tt.tt_ninsert);
	if (tt.tt_nmodes != tt.tt_modes)
		zz_setmodes(tt.tt_nmodes);
	if (tt.tt_insert) {
		esc1();
		pc(n + ' ');
		pc('I');
	}
	ttwrite(p, n);
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
	ZZ_SETINSERT(0);
	zz_setmodes(0);
	zz_setscroll(0, NROW - 1);
	zz_clear();
}

zz_end()
{
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

zz_delchar()
{
	esc();
	pc('c');
}

zz_scroll_down()
{
	if (tt.tt_row == tt.tt_scroll_bot)
		pc('\n');
	else {
		esc();
		pc('f');
	}
}

zz_scroll_up()
{
	esc();
	pc('r');
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

#ifdef notdef
struct ctoken {
	char ct_index;
	short ct_hash
	unsigned long ct_time;
	unsigned long ct_count;
	char ct_string[8];
	struct ctoken ct_forw;
	struct ctoken ct_back;
	struct ctoken ct_hforw;
	struct ctoken ct_hback;
};

zz_compress(p, n)
	register char *p;
	register n;
{
}

zc_insert(string, hash)
	char *string;
	short hash;
{
	register struct ctoken *tp;

	for (tp = zc_hashtable[hash];
	     *tp && strncmp(string, tp->ct_string, 8) != 0;
	     tp = tp->ct_hforw)
		;
	if (tp == 0) {
		if (eval(q2.ct_back) < THRESH) {
			tp = q2.ct_back;
			bcopy(string, tp->string, 8);
			tp->hash = hash;
			tp->count = 0;
		}
	}
	tp->time = zc_time;
	tp->count++;
	tp->back->forw = tp->forw;
	tp->forw->back = tp->back;
	if (tp->index == 0) {
		if (eval(q1.ct_back) < eval(tp)) {
		}
	}
}
#endif

tt_zapple()
{
	tt.tt_delchar = zz_delchar;
	tt.tt_insline = zz_insline;
	tt.tt_delline = zz_delline;
	tt.tt_clreol = zz_clreol;
	tt.tt_clreos = zz_clreos;
	tt.tt_scroll_down = zz_scroll_down;
	tt.tt_scroll_up = zz_scroll_up;
	tt.tt_setscroll = zz_setscroll;
	tt.tt_availmodes = WWM_REV;
	tt.tt_hasinsert = 1;
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
	tt.tt_setinsert = zz_setinsert;
	tt.tt_setmodes = zz_setmodes;
	tt.tt_frame = gen_frame;
	return 0;
}
