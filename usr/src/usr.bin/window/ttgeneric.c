#ifndef lint
static	char *sccsid = "@(#)ttgeneric.c	3.1 83/08/09";
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

char gen_frame[16] = {
	' ', '|', '-', '+',
	'|', '|', '+', '+',
	'-', '|', '-', '+',
	'+', '+', '+', '+'
};

char gen_row, gen_col;
char gen_modes, gen_nmodes;
char gen_insert, gen_ninsert;
char gen_graphics;
short gen_msp10c;

#define pc(c)	putchar('c')
#define esc()	pc(\033)
#define PAD(ms10) { \
	register i; \
	for (i = ((ms10) + 5) / gen_msp10c; --i >= 0;) \
		pc(\0); \
}
#define ICPAD() PAD((80 - gen_col) * 1)		/* 0.1 ms per char */
#define ILPAD() PAD((24 - gen_row) * 10);	/* 1 ms per char */

#define SETINSERT(m) \
	((m) != gen_insert \
		? (esc(), (gen_insert = (m)) ? pc(@) : pc(O)) : 0)
#define SETMODES(m) \
	((m) != gen_modes \
		? (esc(), (gen_modes = (m)) ? pc(p) : pc(q)) : 0)
#define SETGRAPHICS(m) \
	((m) != gen_graphics \
		? (esc(), (gen_graphics = (m)) ? pc(F) : pc(G)) : 0)

gen_setinsert(new)
char new;
{
	gen_ninsert = new;
}

gen_setmodes(new)
{
	gen_nmodes = new & WWM_REV;
}

gen_insline()
{
	esc();
	pc(L);
	ILPAD();
}

gen_delline()
{
	esc();
	pc(M);
	ILPAD();
}

gen_putc(c)
register char c;
{
	SETMODES(gen_nmodes);
	SETINSERT(gen_ninsert);
	if (c & 0x80) {
		SETGRAPHICS(1);
		putchar(c & 0x7f);
	} else {
		SETGRAPHICS(0);
		putchar(c);
	}
	if (gen_insert)
		ICPAD();
	gen_col++;
}

gen_write(start, end)
register char *start, *end;
{
	register char c;

	SETMODES(gen_nmodes);
	SETINSERT(gen_ninsert);
	if (gen_insert) {
		while (start <= end) {
			if ((c = *start++) & 0x80) {
				SETGRAPHICS(1);
				putchar(c & 0x7f);
			} else {
				SETGRAPHICS(0);
				putchar(c);
			}
			ICPAD();
			gen_col++;
		}
	} else {
		gen_col += end - start + 1;
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

gen_blank(n)
register n;
{
	if (n <= 0)
		return;
	SETMODES(gen_nmodes);
	SETINSERT(gen_ninsert);
	if (gen_insert) {
		while (--n >= 0) {
			putchar(' ');
			ICPAD();
			gen_col++;
		}
	} else {
		gen_col += n;
		while (--n >= 0)
			putchar(' ');
	}
}

gen_move(row, col)
register char row, col;
{
	if (gen_row == row) {
		if (gen_col == col)
			return;
		if (gen_col == col - 1) {
			esc();
			pc(C);
			goto out;
		} else if (gen_col == col + 1) {
			pc(\b);
			goto out;
		}
	}
	if (gen_col == col) {
		if (gen_row == row + 1) {
			esc();
			pc(A);
			goto out;
		} else if (gen_row == row + 1) {
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
	gen_col = col;
	gen_row = row;
}

gen_init()
{
	float cpms = (float) wwbaud / 10000;	/* char per ms */

	gen_msp10c = 10 / cpms;			/* ms per 10 char */
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

gen_reset()
{
	esc();
	pc(x);
	pc(4);
	esc();
	pc(E);
	esc();
	pc(w);
	gen_col = gen_row = 0;
	gen_insert = 0;
	gen_graphics = 0;
	gen_modes = 0;
}

gen_cleanup()
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

gen_clreol()
{
	esc();
	pc(K);
}

gen_clreos()
{
	esc();
	pc(J);
}

gen_clear()
{
	esc();
	pc(E);
}

gen_delchar()
{
	esc();
	pc(N);
}

tt_generic()
{
	tt.tt_setinsert = gen_setinsert;
	tt.tt_setmodes = gen_setmodes;
	tt.tt_insline = gen_insline;
	tt.tt_delline = gen_delline;
	tt.tt_delchar = gen_delchar;
	tt.tt_blank = gen_blank;
	tt.tt_init = gen_init;
	tt.tt_cleanup = gen_cleanup;
	tt.tt_clreol = gen_clreol;
	tt.tt_clreos = gen_clreos;
	tt.tt_clear = gen_clear;
	tt.tt_move = gen_move;
	tt.tt_reset = gen_reset;
	tt.tt_write = gen_write;
	tt.tt_putc = gen_putc;
	tt.tt_ncol = 80;
	tt.tt_nrow = 24;
	tt.tt_frame = gen_frame;
	return 0;
}
