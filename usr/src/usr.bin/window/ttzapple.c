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
static char sccsid[] = "@(#)ttzapple.c	3.5 (Berkeley) %G%";
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

#define NCOL		80
#define NROW		24
#define TOKEN_MAX	32

#define pc(c)	ttputc(c)
#define esc()	pc(ctrl('['))
#define esc1()	pc(ctrl('\\'))
#define esc2()	pc(ctrl(']'))

extern short gen_frame[];

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
	ttwrite(p, n);
	tt.tt_col += n;
	if (tt.tt_col == NCOL)
		tt.tt_col = 0, tt.tt_row++;
}

zz_move(row, col)
	register row, col;
{
	register x;

	if (tt.tt_row == row) {
		if ((x = col - tt.tt_col) == 0)
			return;
		if (col == 0) {
			pc('\r');
			goto out;
		}
		switch (x) {
		case 2:
			pc(ctrl('f'));
		case 1:
			pc(ctrl('f'));
			goto out;
		case -2:
			pc('\b');
		case -1:
			pc('\b');
			goto out;
		}
		if (col & 7 == 0 && x > 0 && x <= 16) {
			pc('\t');
			if (x > 8)
				pc('\t');
			goto out;
		}
		esc1();
		pc(col + ' ');
		pc('<');
		goto out;
	}
	if (tt.tt_col == col) {
		switch (row - tt.tt_row) {
		case 2:
			pc('\n');
		case 1:
			pc('\n');
			goto out;
		case -2:
			pc(ctrl('k'));
		case -1:
			pc(ctrl('k'));
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
		if (row == tt.tt_row + 1) {
			pc('\r');
			pc('\n');
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

zz_start()
{
	zz_setmodes(0);
	zz_setscroll(0, NROW - 1);
	zz_clear();
	esc1();
	pc(TOKEN_MAX + ' ');
	pc('T');
}

zz_end()
{
	esc1();
	pc(' ');
	pc('T');
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

zz_insspace(n)
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
	} else if (tt.tt_row == NROW - 1)
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

int zz_debug = 0;

zz_set_token(t, s, n)
	char *s;
{
	if (tt.tt_nmodes != tt.tt_modes)
		zz_setmodes(tt.tt_nmodes);
	if (zz_debug) {
		char buf[100];
		zz_setmodes(WWM_REV);
		(void) sprintf(buf, "%02x=", t);
		ttputs(buf);
		tt.tt_col += 3;
	}
	pc(0x80);
	pc(t + 1);
	s[n - 1] |= 0x80;
	ttwrite(s, n);
	s[n - 1] &= ~0x80;
	tt.tt_col += n;
	if (tt.tt_col == NCOL)
		tt.tt_col = 0, tt.tt_row++;
}

/*ARGSUSED*/
zz_put_token(t, s, n)
	char *s;
{
	if (tt.tt_nmodes != tt.tt_modes)
		zz_setmodes(tt.tt_nmodes);
	if (zz_debug) {
		char buf[100];
		zz_setmodes(WWM_REV);
		(void) sprintf(buf, "%02x>", t);
		ttputs(buf);
		tt.tt_col += 3;
	}
	pc(t + 0x81);
	tt.tt_col += n;
	if (tt.tt_col == NCOL)
		tt.tt_col = 0, tt.tt_row++;
}

tt_zapple()
{
	tt.tt_insspace = zz_insspace;
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
	tt.tt_ncol = NCOL;
	tt.tt_nrow = NROW;
	tt.tt_start = zz_start;
	tt.tt_end = zz_end;
	tt.tt_write = zz_write;
	tt.tt_putc = zz_putc;
	tt.tt_move = zz_move;
	tt.tt_clear = zz_clear;
	tt.tt_setmodes = zz_setmodes;
	tt.tt_frame = gen_frame;
	tt.tt_ntoken = 127;
	tt.tt_set_token = zz_set_token;
	tt.tt_put_token = zz_put_token;
	tt.tt_token_min = 1;
	tt.tt_token_max = TOKEN_MAX;
	tt.tt_set_token_cost = 2;
	tt.tt_put_token_cost = 1;
	return 0;
}
