#ifndef lint
static	char *sccsid = "@(#)tth19.c	3.2 83/08/11";
#endif

#include "ww.h"

/*
kb|h19|heath|h19-b|h19b|heathkit|heath-19|z19|zenith:
	cr=^M:nl=^J:bl=^G:al=1*\EL:am:le=^H:bs:cd=\EJ:ce=\EK:
	cl=\EE:cm=\EY%+ %+ :co#80:dc=\EN:dl=1*\EM:do=\EB:
	ei=\EO:ho=\EH:im=\E@:li#24:mi:nd=\EC:as=\EF:ae=\EG:ms:
	ta=^I:pt:sr=\EI:se=\Eq:so=\Ep:up=\EA:vs=\Ex4:ve=\Ey4:
	kb=^h:ku=\EA:kd=\EB:kl=\ED:kr=\EC:kh=\EH:
	kn#8:k1=\ES:k2=\ET:k3=\EU:k4=\EV:k5=\EW:
	l6=blue:l7=red:l8=white:k6=\EP:k7=\EQ:k8=\ER:
	es:hs:ts=\Ej\Ex5\Ex1\EY8%+ \Eo:fs=\Ek\Ey5:ds=\Ey1:
*/

char h19_frame[16] = {
	' ',      '`'|0x80, 'a'|0x80, 'e'|0x80,
	'`'|0x80, '`'|0x80, 'f'|0x80, 'v'|0x80,
	'a'|0x80, 'd'|0x80, 'a'|0x80, 'u'|0x80,
	'c'|0x80, 't'|0x80, 's'|0x80, 'b'|0x80
};

char h19_row, h19_col;
char h19_modes, h19_nmodes;
char h19_insert, h19_ninsert;
char h19_graphics;
short h19_msp10c;

#define pc(c)	putchar('c')
#define esc()	pc(\033)
#define PAD(ms10) { \
	register i; \
	for (i = ((ms10) + 5) / h19_msp10c; --i >= 0;) \
		pc(\0); \
}
#define ICPAD() PAD((80 - h19_col) * 1)		/* 0.1 ms per char */
#define ILPAD() PAD((24 - h19_row) * 10);	/* 1 ms per char */

#define SETINSERT(m) \
	((m) != h19_insert \
		? (esc(), (h19_insert = (m)) ? pc(@) : pc(O)) : 0)
#define SETMODES(m) \
	((m) != h19_modes \
		? (esc(), (h19_modes = (m)) ? pc(p) : pc(q)) : 0)
#define SETGRAPHICS(m) \
	((m) != h19_graphics \
		? (esc(), (h19_graphics = (m)) ? pc(F) : pc(G)) : 0)

h19_setinsert(new)
char new;
{
	h19_ninsert = new;
}

h19_setmodes(new)
{
	h19_nmodes = new & WWM_REV;
}

h19_insline()
{
	esc();
	pc(L);
	ILPAD();
}

h19_delline()
{
	esc();
	pc(M);
	ILPAD();
}

h19_putc(c)
register char c;
{
	SETMODES(h19_nmodes);
	SETINSERT(h19_ninsert);
	if (c & 0x80) {
		SETGRAPHICS(1);
		putchar(c & 0x7f);
	} else {
		SETGRAPHICS(0);
		putchar(c);
	}
	if (h19_insert)
		ICPAD();
	h19_col++;
}

h19_write(start, end)
register char *start, *end;
{
	register char c;

	SETMODES(h19_nmodes);
	SETINSERT(h19_ninsert);
	if (h19_insert) {
		while (start <= end) {
			if ((c = *start++) & 0x80) {
				SETGRAPHICS(1);
				putchar(c & 0x7f);
			} else {
				SETGRAPHICS(0);
				putchar(c);
			}
			ICPAD();
			h19_col++;
		}
	} else {
		h19_col += end - start + 1;
		while (start <= end)
			if ((c = *start++) & 0x80) {
				SETGRAPHICS(1);
				putchar(c & 0x7f);
			} else {
				SETGRAPHICS(0);
				putchar(c);
			}
	}
}

h19_blank(n)
register n;
{
	if (n <= 0)
		return;
	SETMODES(h19_nmodes);
	SETINSERT(h19_ninsert);
	if (h19_insert) {
		while (--n >= 0) {
			putchar(' ');
			ICPAD();
			h19_col++;
		}
	} else {
		h19_col += n;
		while (--n >= 0)
			putchar(' ');
	}
}

h19_move(row, col)
register char row, col;
{
	if (h19_row == row) {
		if (h19_col == col)
			return;
		if (h19_col == col - 1) {
			esc();
			pc(C);
			goto out;
		} else if (h19_col == col + 1) {
			pc(\b);
			goto out;
		}
	}
	if (h19_col == col) {
		if (h19_row == row + 1) {
			esc();
			pc(A);
			goto out;
		} else if (h19_row == row + 1) {
			pc(\n);
			goto out;
		}
	}
	if (col == 1 && row == 1) {
		esc();
		pc(H);
		goto out;
	}
	esc();
	pc(Y);
	putchar(' ' + row);
	putchar(' ' + col);
out:
	h19_col = col;
	h19_row = row;
}

h19_init()
{
	float cpms = (float) wwbaud / 10000;	/* char per ms */

	h19_msp10c = 10 / cpms;			/* ms per 10 char */
#ifdef notdef
	tt.tt_ILmf = cpms;			/* 1 ms */
	tt.tt_ILov = 2;
	tt.tt_ICmf = cpms * 1.5 ;		/* 1.5 ms */
	tt.tt_ICov = 2;
	tt.tt_DCmf = 0;
	tt.tt_DCov = 2;
#endif
	return 0;
}

h19_reset()
{
	esc();
	pc(x);
	pc(4);
	esc();
	pc(E);
	esc();
	pc(w);
	h19_col = h19_row = 0;
	h19_insert = 0;
	h19_graphics = 0;
	h19_modes = 0;
}

h19_cleanup()
{
	SETMODES(0);
	SETINSERT(0);
	SETGRAPHICS(0);
	esc();
	pc(y);
	pc(4);
	esc();
	pc(v);
}

h19_clreol()
{
	esc();
	pc(K);
}

h19_clreos()
{
	esc();
	pc(J);
}

h19_clear()
{
	esc();
	pc(E);
}

h19_delchar()
{
	esc();
	pc(N);
}

tt_h19()
{
	tt.tt_setinsert = h19_setinsert;
	tt.tt_setmodes = h19_setmodes;
	tt.tt_insline = h19_insline;
	tt.tt_delline = h19_delline;
	tt.tt_delchar = h19_delchar;
	tt.tt_blank = h19_blank;
	tt.tt_init = h19_init;
	tt.tt_cleanup = h19_cleanup;
	tt.tt_clreol = h19_clreol;
	tt.tt_clreos = h19_clreos;
	tt.tt_clear = h19_clear;
	tt.tt_move = h19_move;
	tt.tt_reset = h19_reset;
	tt.tt_write = h19_write;
	tt.tt_putc = h19_putc;
	tt.tt_ncol = 80;
	tt.tt_nrow = 24;
	tt.tt_frame = h19_frame;
	return 0;
}
