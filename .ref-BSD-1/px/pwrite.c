#
#include "0x.h"
#include "opcode.h"

pwrite(opc, subopc, av)
{
	register char *ap, *cp;
	register i;
	int w, w1, sign, decpt;
	char *acp;
	long along;
	double adouble;
	int k2,k3;

	ap = &av;
	if (opc == O_WRITLN || opc == O_PAGE)
		opc = O_WRITC;
	switch(opc) {
		case O_WRIT2:
			along = ap->pint;
			ap =+ 2;
			w = 10;
			break;
		case O_WRIT4:
			along = ap->plong;
			ap =+ 4;
			w = 10;
			break;
		case O_WRITC:
			acp = ap;
			ap =+ 2;
			w = 1;
			break;
		case O_WRITB:
			i = ap->pint;
			ap =+ 2;
			w = 10;
			break;
		case O_WRITG:
			w1 = ap->pint;
			ap =+ 2;
			acp = ap;
			ap =+ (w1 + 1) & ~1;
			w = 0;
			break;
		case O_WRIT8:
		case O_WRIT82:
			adouble = ap->pdouble;
			ap =+ 8;
			w = 22;
			break;
		case O_WRHEX2:
		case O_WROCT2:
			(&along)->pint = 0;
			(&along)->p2int = ap->pint;
			ap =+ 2;
			w = opc == O_WROCT2 ? 11 : 8;
			break;
		case O_WRHEX2+1:	/* ugh, cc string table too small */
		case O_WROCT2+1:	/* ugh, cc string table too small */
			along = ap->plong;
			ap =+ 4;
			w = opc == O_WROCT2+1 ? 11 : 8;
			break;
	}
again:
	switch(subopc & 07) {
		case 0:
			break;
		case 2:
			w = ap->pint;
			ap =+ 2;
			break;
		case 4:
			w = ap->plong;
			ap =+ 4;
			break;
	}
	if (opc == O_WRIT82 && (i = (subopc >> 3) & 07) != 0) {
		subopc = i;
		w1 = w;
		goto again;
	}
	switch(opc) {
		case O_WROCT2+1:
		case O_WROCT2:
			while (w > 11) {
				pputch(' ');
				w--;
			}
			if (w > 0)
				wro(along, w);
			break;
		case O_WRHEX2:
		case O_WRHEX2+1:
			while (w > 8) {
				pputch(' ');
				w--;
			}
			if (w > 0)
				wrhex(along, w);
			break;
		case O_WRIT2:
		case O_WRIT4:
			pwril(w, along);
			break;
		case O_WRITC:
			while (w > 1) {
				pputch(' ');
				w--;
			}
			pputch(*acp);
			break;
		case O_WRITB:
			if (i) {
				acp = "true";
				w1 = 4;
			} else {
				acp = "false";
				w1 = 5;
			}
		case O_WRITG:
			cp = acp;
			while (w > w1) {
				pputch(' ');
				w--;
			}
			while(w1 > 0) {
				pputch(*cp++);
				w1--;
			}
			break;
		case O_WRIT8:
			if (adouble == 0.0) {
				do
					pputch(' ');
				while (--w > 0);
				pputch('0');
				break;
			}
			if (w <= 9)
				w = 3;
			else
				w =- 6;
			do
				pputch(' ');
			while (--w > 17);
			cp = ecvt(adouble, w+1, &decpt, &sign);
			pputch(sign ? '-' : ' ');
			pputch(*cp++);
			pputch('.');
			do
				pputch(*cp++);
			while (--w > 0);
			pputch('e');
			if (--decpt >= 0)
				pputch('+');
			else {
				pputch('-');
				decpt = -decpt;
			}
			pputch(decpt / 10 + '0');
			pputch(decpt % 10 + '0');
			break;
		case O_WRIT82:
			if (adouble == 0.0) {
				do
					pputch(' ');
				while(--w1 > 1);
				pputch('0');
				break;
			}
			if (w < 0)
				w = 0;
			cp = fcvt(adouble, w, &decpt, &sign);
			cp[17] = '\0';
			if (decpt > 0) {
				w1 = w1-w-decpt-2;
				k2 = decpt;
				k3 = 0;
			} else {
				w1 = w1-w-3;
				k2 = 0;
				if (w+decpt >= 0)
					k3 = -decpt;
				else
					k3 = w;
				w =- k3;
			}
			while (w1 > 0) {
				pputch(' ');
				w1--;
			}
			pputch(sign ? '-' : ' ');
			if (k2 <= 0)
				pputch('0');
			else
				do
					pputch(*cp ? *cp++ : '0');
				while (--k2 > 0);
			pputch('.');
			if (k3 <= 0 && w <= 0) {
				pputch('0');
				break;
			}
			while(k3 > 0) {
				pputch('0');
				k3--;
			}
			while(w > 0) {
				pputch(*cp ? *cp++ : '0');
				w--;
			}
			break;
	}
	return(ap);
}

wro(l, w)
long l;
{
	register c;

	c = (&l)->p2int & 07;
	l =>> 3;
	(&l)->pint =& 017777;
	if (w > 1 || l != 0)
		wro(l, w-1);
	pputch(c | '0');
}

wrhex(l, w)
long l;
{
	register c;

	c = (&l)->p2int & 017;
	l =>> 4;
	(&l)->pint =& 07777;
	if (w > 1 || l != 0)
		wrhex(l, w-1);
	pputch(c <= 9 ? c | '0' : 'a' + (c - 10));
}
