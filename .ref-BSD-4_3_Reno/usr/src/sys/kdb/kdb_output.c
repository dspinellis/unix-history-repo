/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_output.c	7.3 (Berkeley) 5/3/90
 */

#include "../kdb/defs.h"

long	kdbmaxpos;
int	kdbradix = 16;

char	kdbprintbuf[MAXLIN];
char	*kdbprintptr = kdbprintbuf;
char	*kdbdigitptr;

kdbprintc(c)
	char c;
{
	char d;
	register char *q;
	register posn, tabs, p;

	if (kdbmkfault)
		return;
	if ((*kdbprintptr=c)==EOR) {
		tabs=0; posn=0; q=kdbprintbuf;
		for (p=0; p<kdbprintptr-kdbprintbuf; p++) {
			d=kdbprintbuf[p];
			if ((p&7)==0 && posn) {
				tabs++;
				posn=0;
			}
			if (d!=SP) {
				while (tabs>0)
					*q++=TB, tabs--;
				while (posn>0)
					*q++=SP, posn--;
				*q++=d;
			} else
				posn++;
		 }
		 *q++=EOR;
		 kdbwrite(kdbprintbuf,q-kdbprintbuf);
		 kdbprintptr=kdbprintbuf;
	} else if (c==TB) {
		*kdbprintptr++=SP;
		while ((kdbprintptr-kdbprintbuf)&7)
			*kdbprintptr++=SP;
	} else if (c)
		kdbprintptr++;
	if (kdbprintptr >= &kdbprintbuf[MAXLIN-9]) {
		kdbwrite(kdbprintbuf, kdbprintptr - kdbprintbuf);
		kdbprintptr = kdbprintbuf;
	}
}

kdbcharpos()
{

	return (kdbprintptr-kdbprintbuf);
}

kdbflushbuf()
{

	if (kdbprintptr!=kdbprintbuf)
		kdbprintc(EOR);
}

/* VARARGS1 */
kdbprintf(fmat,a1)
	char *fmat, *a1;
{
	char *fptr;
	register char *s;
	register long *dptr;
	register width, prec;
	char c, adj;
	int x, n;
	register long lx;
	char digits[64];

	fptr = fmat; dptr = (long *)&a1;
	while (c = *fptr++) {
		if (c!='%') {
			kdbprintc(c);
			continue;
		}
		if (*fptr=='-') {
			adj='l'; fptr++;
		} else
			adj='r';
		width=kdbconvert(&fptr);
		if (*fptr=='.') {
			fptr++; prec=kdbconvert(&fptr);
		} else
			prec = -1;
		kdbdigitptr=digits;
		x = lx = *dptr++;
		s=0;
		switch (c = *fptr++) {
		case 'd':
			kdbprintnum((u_long)x, -10); break;
		case 'u':
			kdbprintnum((u_long)x, 10); break;
		case 'o':
			kdbprintnum((u_long)x, 8); break;
		case 'q':
			kdbprintnum((u_long)x, -8); break;
		case 'x':
			kdbprintnum((u_long)x, 16); break;
		case 'z':
			kdbprintnum((u_long)x, -16); break;
		case 'R':
			kdbprintnum((u_long)lx, kdbradix); break;
		case 'D':
			kdbprintnum((u_long)lx, -10); break;
		case 'U':
			kdbprintnum((u_long)lx, 10); break;
		case 'O':
			kdbprintnum((u_long)lx, 8); break;
		case 'Q':
			kdbprintnum((u_long)lx, -8); break;
		case 'X':
			kdbprintnum((u_long)lx, 16); break;
		case 'Z':
			kdbprintnum((u_long)lx, -16); break;
		case 'c':
			kdbprintc(x); break;
		case 's':
			s=(char *)lx; break;
		case 'm':
			break;
		case 'M':
			width=x; break;
		case 'T': case 't':
			if (c=='T')
				width=x;
			else
				dptr--;
			if (width)
				width -= kdbcharpos()%width;
			break;
		default:
			kdbprintc(c); dptr--;
			break;
		}
		if (s==0) {
			*kdbdigitptr=0; s=digits;
		}
		n=strlen(s);
		n=(prec<n && prec>=0 ? prec : n);
		width -= n;
		if (adj=='r')
			while (width-- > 0)
				kdbprintc(SP);
		while (n--)
			kdbprintc(*s++);
		while (width-- > 0)
			kdbprintc(SP);
		kdbdigitptr=digits;
	}
}

static
kdbconvert(cp)
	register char **cp;
{
	register char c;
	int n;

	n=0;
	while (((c = *(*cp)++)>='0') && c<='9')
		n=n*10+c-'0';
	(*cp)--;
	return (n);
}

static
kdbprintnum(n, base)
	register u_long n;
{
	register char *dptr;
	char digs[15];

	dptr=digs;
	if (base<0) {
		base = -base;
		if ((long)n<0) {
			n = -n;
			*kdbdigitptr++ = '-';
		}
	}
	while (n) {
		*dptr++ = n%base;
		n /= base;
	}
	if (dptr==digs)
		*dptr++=0;
	while (dptr!=digs) {
		n = *--dptr;
		*kdbdigitptr++ = (n+(n<=9 ? '0' : 'a'-10));
	}
}

kdbendline()
{

	if (kdbmaxpos <= kdbcharpos())
		kdbprintf("\n");
}
