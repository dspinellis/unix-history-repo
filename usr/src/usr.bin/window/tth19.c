#ifndef lint
static	char *sccsid = "@(#)tth19.c	3.9 83/09/15";
#endif

#include "ww.h"
#include "tt.h"

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

#define NCOL	80
#define NROW	24

char h19_frame[16] = {
	' ',      '`'|0x80, 'a'|0x80, 'e'|0x80,
	'`'|0x80, '`'|0x80, 'f'|0x80, 'v'|0x80,
	'a'|0x80, 'd'|0x80, 'a'|0x80, 'u'|0x80,
	'c'|0x80, 't'|0x80, 's'|0x80, 'b'|0x80
};

extern char *gen_VS;
extern char *gen_VE;

char h19_graphics;			/* in graphics mode */
short h19_msp10c;

#define pc(c)	putchar('c')
#define esc()	pc(\033)
#define PAD(ms10) { \
	register i; \
	for (i = ((ms10) + 5) / h19_msp10c; --i >= 0;) \
		pc(\0); \
}
#define ICPAD() PAD((NCOL - tt.tt_col) * 1)	/* 0.1 ms per char */
#define ILPAD() PAD((NROW - tt.tt_row) * 10)	/* 1 ms per char */

#define SETINSERT(m) \
	((m) != tt.tt_insert \
		? (esc(), (tt.tt_insert = (m)) ? pc(@) : pc(O)) : 0)
#define SETMODES(m) \
	((m) != tt.tt_modes \
		? (esc(), (tt.tt_modes = (m)) ? pc(p) : pc(q)) : 0)
#define SETGRAPHICS(m) \
	((m) != h19_graphics \
		? (esc(), (h19_graphics = (m)) ? pc(F) : pc(G)) : 0)

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
	SETMODES(tt.tt_nmodes);
	SETINSERT(tt.tt_ninsert);
	if (c & 0x80) {
		SETGRAPHICS(1);
		putchar(c & 0x7f);
	} else {
		SETGRAPHICS(0);
		putchar(c);
	}
	if (tt.tt_insert)
		ICPAD();
	if (++tt.tt_col == NCOL)
		tt.tt_col = NCOL - 1;
}

h19_write(p, n)
register char *p;
register n;
{
	register char c;

	SETMODES(tt.tt_nmodes);
	SETINSERT(tt.tt_ninsert);
	if (tt.tt_insert) {
		while (--n >= 0) {
			if ((c = *p++) & 0x80) {
				SETGRAPHICS(1);
				putchar(c & 0x7f);
			} else {
				SETGRAPHICS(0);
				putchar(c);
			}
			ICPAD();
			tt.tt_col++;
		}
	} else {
		tt.tt_col += n;
		while (--n >= 0)
			if ((c = *p++) & 0x80) {
				SETGRAPHICS(1);
				putchar(c & 0x7f);
			} else {
				SETGRAPHICS(0);
				putchar(c);
			}
	}
	if (tt.tt_col == NCOL)
		tt.tt_col = NCOL - 1;
}

h19_move(row, col)
register char row, col;
{
	if (tt.tt_row == row) {
		if (tt.tt_col == col)
			return;
		if (tt.tt_col == col - 1) {
			esc();
			pc(C);
			goto out;
		} else if (tt.tt_col == col + 1) {
			pc(\b);
			goto out;
		}
	}
	if (tt.tt_col == col) {
		if (tt.tt_row == row + 1) {
			esc();
			pc(A);
			goto out;
		} else if (tt.tt_row == row + 1) {
			pc(\n);
			goto out;
		}
	}
	if (col == 0 && row == 0) {
		esc();
		pc(H);
		goto out;
	}
	esc();
	pc(Y);
	putchar(' ' + row);
	putchar(' ' + col);
out:
	tt.tt_col = col;
	tt.tt_row = row;
}

h19_init()
{
	if (gen_VS)
		fputs(gen_VS, stdout);
	esc();
	pc(w);
	esc();
	pc(E);
	tt.tt_col = tt.tt_row = 0;
	tt.tt_ninsert = tt.tt_insert = 0;
	tt.tt_nmodes = tt.tt_modes = 0;
	h19_graphics = 0;
}

h19_end()
{
	SETMODES(0);
	SETINSERT(0);
	SETGRAPHICS(0);
	if (gen_VE)
		fputs(gen_VE, stdout);
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
	float cpms = (float) wwbaud / 10000;	/* char per ms */

	h19_msp10c = 10 / cpms;			/* ms per 10 char */
	gen_VS = tt_xgetstr("vs");
	gen_VE = tt_xgetstr("ve");

	tt.tt_init = h19_init;
	tt.tt_end = h19_end;

	tt.tt_insline = h19_insline;
	tt.tt_delline = h19_delline;
	tt.tt_delchar = h19_delchar;
	tt.tt_clreol = h19_clreol;
	tt.tt_clreos = h19_clreos;
	tt.tt_clear = h19_clear;
	tt.tt_move = h19_move;
	tt.tt_write = h19_write;
	tt.tt_putc = h19_putc;

	tt.tt_ncol = NCOL;
	tt.tt_nrow = NROW;
	tt.tt_hasinsert = 1;
	tt.tt_availmodes = WWM_REV;
	tt.tt_frame = h19_frame;
	return 0;
}
