#ifndef lint
static char sccsid[] = "@(#)tth29.c	3.1 %G%";
#endif

#include "ww.h"
#include "tt.h"

/*
kC|h29|heath-29|z29|zenith-29:\
	:am:bc=\ED:bt=\E-:cr=^M:do=^J:nl=^J:bl=^G:\
	:al=\EL:le=^H:bs:cd=\EJ:ce=\EK:cl=\EE:cm=\EY%+ %+ :co#80:dc=\EN:\
	:dl=1*\EM:do=\EB:ei=\EO:ho=\EH:im=\E@:li#24:mi:nd=\EC:as=\EF:ae=\EG:\
	:ms:ta=^I:pt:sr=\EI:se=\Eq:so=\Ep:up=\EA:vs=\Ex4:ve=\Ey4:\
	:kb=^H:ku=\EA:kd=\EB:kl=\ED:kr=\EC:kh=\EH:kn#1:k0=\E~:l0=HOME:\
	:k1=\ES:k2=\ET:k3=\EU:k4=\EV:k5=\EW:k6=\EP:k7=\EQ:k8=\ER:k9=\E01:\
	:es:hs:ts=\Ej\Ex5\Ex1\EY8%+ \Eo:fs=\Ek\Ey5:ds=\Ey1:us=\Es8:ue=\Es0:
*/

#define NCOL	80
#define NROW	24

extern char *gen_VS;
extern char *gen_VE;

extern int h19_msp10c;

#define pc(c)	ttputc('c')
#define esc()	pc(\033)
#define PAD(ms10) { \
	register i; \
	for (i = ((ms10) + 5) / h19_msp10c; --i >= 0;) \
		pc(\0); \
}
#define ICPAD() PAD((NCOL - tt.tt_col) * 1)	/* 0.1 ms per char */
#define ILPAD() PAD((NROW - tt.tt_row) * 10)	/* 1 ms per char */

#define h29_setinsert(m) (esc(), (tt.tt_insert = (m)) ? pc(@) : pc(O))

h29_setmodes(new)
register new;
{
	register modes = '0';

	if (new & WWM_REV)
		modes |= 1;
	if (new & WWM_BLK)
		modes |= 2;
	if (new & WWM_UL)
		modes |= 8;
	esc();
	pc(s);
	ttputc(modes);
	if (new & WWM_GRP) {
		if ((tt.tt_modes & WWM_GRP) == 0)
			esc(), pc(F);
	} else
		if (tt.tt_modes & WWM_GRP)
			esc(), pc(G);
	tt.tt_modes = new;
}

h29_putc(c)
register char c;
{
	if (tt.tt_nmodes != tt.tt_modes)
		h29_setmodes(tt.tt_nmodes);
	if (tt.tt_ninsert != tt.tt_insert)
		h29_setinsert(tt.tt_ninsert);
	ttputc(c);
	if (tt.tt_insert)
		ICPAD();
	if (++tt.tt_col == NCOL)
		tt.tt_col = NCOL - 1;
}

h29_write(p, n)
register char *p;
register n;
{
	if (tt.tt_nmodes != tt.tt_modes)
		h29_setmodes(tt.tt_nmodes);
	if (tt.tt_ninsert != tt.tt_insert)
		h29_setinsert(tt.tt_ninsert);
	if (tt.tt_insert) {
		while (--n >= 0) {
			ttputc(*p++);
			ICPAD();
			tt.tt_col++;
		}
	} else {
		tt.tt_col += n;
		while (--n >= 0)
			ttputc(*p++);
	}
	if (tt.tt_col == NCOL)
		tt.tt_col = NCOL - 1;
}

h29_end()
{
	h29_setmodes(0);
	h29_setinsert(0);
	if (gen_VE)
		ttputs(gen_VE);
	esc();
	pc(v);
}

tt_h29()
{
	if (tt_h19() < 0)
		return -1;
	tt.tt_putc = h29_putc;
	tt.tt_write = h29_write;
	tt.tt_end = h29_end;
	tt.tt_availmodes = WWM_BLK|WWM_UL|WWM_REV|WWM_GRP;
	return 0;
}
