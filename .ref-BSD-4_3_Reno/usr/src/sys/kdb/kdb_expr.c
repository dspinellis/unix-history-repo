/*
 * Copyright (c) 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kdb_expr.c	7.6 (Berkeley) 5/3/90
 */

#include "../kdb/defs.h"

char	*kdbBADSYM;
char	*kdbBADVAR;
char	*kdbBADKET;
char	*kdbBADSYN;
char	*kdbNOCFN;
char	*kdbNOADR;
char	*kdbBADLOC;

ADDR	kdblastframe;
ADDR	kdbsavlastf;
ADDR	kdbsavframe;
ADDR	kdbsavpc;
ADDR	kdbcallpc;

char	*kdblp;
int	kdbradix;
char	kdbisymbol[1024];

char	kdblastc, kdbpeekc;

long	kdbditto;
long	kdbexpv;

static long
kdbround(a,b)
	register long a, b;
{
	register long w;

	w = (a/b)*b;
	if (a!=w)
		w += b;
	return (w);
}

/* term | term dyadic expr |  */
kdbexpr(a)
{
	register rc;
	register long lhs;

	(void) kdbrdc(); kdblp--; rc=kdbterm(a);

	while (rc) {
		lhs = kdbexpv;
		switch ((int)kdbreadchar()) {
		case '+':
			(void) kdbterm(a|1); kdbexpv += lhs; break;
		case '-':
			(void) kdbterm(a|1); kdbexpv = lhs - kdbexpv; break;
		case '#':
			(void) kdbterm(a|1); kdbexpv = kdbround(lhs,kdbexpv); break;
		case '*':
			(void) kdbterm(a|1); kdbexpv *= lhs; break;
		case '%':
			(void) kdbterm(a|1); kdbexpv = lhs/kdbexpv; break;
		case '&':
			(void) kdbterm(a|1); kdbexpv &= lhs; break;
		case '|':
			(void) kdbterm(a|1); kdbexpv |= lhs; break;
		case ')':
			if ((a&2)==0)
				kdberror(kdbBADKET);
		default:
			kdblp--;
			return (rc);
		}
	}
	return (rc);
}

/* item | monadic item | (expr) | */
static
kdbterm(a)
{

	switch ((int)kdbreadchar()) {
	case '*':
		(void) kdbterm(a|1); kdbexpv=kdbchkget(kdbexpv,DSP);
		return(1);
	case '@':
		(void) kdbterm(a|1); kdbexpv=kdbchkget(kdbexpv,ISP);
		return(1);
	case '-':
		(void) kdbterm(a|1); kdbexpv = -kdbexpv;
		return(1);
	case '~':
		(void) kdbterm(a|1); kdbexpv = ~kdbexpv;
		return(1);
	case '#':
		(void) kdbterm(a|1); kdbexpv = !kdbexpv;
		return(1);
	case '(':
		(void) kdbexpr(2);
		if (*kdblp!=')')
			kdberror(kdbBADSYN);
		kdblp++;
		return(1);
	}
	kdblp--;
	return (kdbitem(a));
}

/* name [ . local ] | number | . | ^ | <var | <register | 'x | | */
static
kdbitem(a)
{
	register base, d, regptr;
	char savc;
	register long frame;
	register struct nlist *symp;

	(void) kdbreadchar();
	if (kdbsymchar(0)) {
		kdbreadsym();
		if (kdblastc=='.') {
			frame = kdbpcb.pcb_fp; kdblastframe = 0;
			kdbcallpc = kdbpcb.pcb_pc;
			while (!kdberrflg) {
				kdbsavpc = kdbcallpc;
				(void) kdbfindsym((long)kdbcallpc,ISYM);
				if (kdbeqsym(kdbcursym->n_un.n_name,kdbisymbol,'~'))
					break;
				kdbcallpc = getprevpc(frame);
				kdblastframe = frame;
				frame = getprevframe(frame);
				if (frame == NOFRAME)
					kdberror(kdbNOCFN);
			}
			kdbsavlastf = kdblastframe; kdbsavframe = frame;
			(void) kdbreadchar();
			if (kdbsymchar(0))
				kdbchkloc(kdbexpv=frame);
		} else if ((symp=kdblookup(kdbisymbol))==0)
			kdberror(kdbBADSYM);
		else
			kdbexpv = symp->n_value;
		kdblp--;
		return (1);
	}
	if (kdbgetnum())
		return (1);
	switch (kdblastc) {
	case '.':
		(void) kdbreadchar();
		if (kdbsymchar(0)) {
			kdblastframe=kdbsavlastf; kdbcallpc=kdbsavpc;
			kdbchkloc((long)kdbsavframe);
		} else
			kdbexpv=kdbdot;
		kdblp--;
		break;
	case '"':
		kdbexpv=kdbditto;
		break;
	case '+':
		kdbexpv=kdbinkdot(kdbdotinc);
		break;
	case '^':
		kdbexpv=kdbinkdot(-kdbdotinc);
		break;
	case '<':
		savc=kdbrdc();
		if ((regptr=kdbgetreg(savc)) != -1)
			kdbexpv = *(int *)regptr;
		else if ((base=kdbvarchk(savc)) != -1)
			kdbexpv=kdbvar[base];
		else
			kdberror(kdbBADVAR);
		break;
	case '\'':
		d=4; kdbexpv=0;
		while (kdbquotchar()) {
		    if (d--) {
		         kdbexpv <<= 8;
			 kdbexpv |= kdblastc;
		    } else
			kdberror(kdbBADSYN);
		}
		break;
	default:
		if (a)
			kdberror(kdbNOADR);
		kdblp--;
		return(0);
	}
	return (1);
}

/* service routines for expression reading */
static
kdbgetnum()
{
	register base,d,frpt;

	if (!isdigit(kdblastc))
		return (0);
	if ((base = kdbradix) < 0)
		base = -base;
	kdbexpv = 0;
	while (base>10 ? isxdigit(kdblastc) : isdigit(kdblastc)) {
		register m = MAXINT/base;

		if (kdbexpv>m)		/* avoid overflow */
			kdbexpv = (kdbexpv-m)*base+m*base;
		else
			kdbexpv *= base;
		if ((d=kdbconvdig(kdblastc))>=base || d<0)
			kdberror(kdbBADSYN);
		kdbexpv += d; (void) kdbreadchar();
		if (kdbexpv==0) {
			if (kdblastc=='x' || kdblastc=='X') {
				 base=16; (void) kdbreadchar();
			} else if (kdblastc=='t' || kdblastc=='T') {
				 base=10; (void) kdbreadchar();
			} else if (kdblastc=='o' || kdblastc=='O') {
				 base=8; (void) kdbreadchar();
			}
		}
	}
	if (kdblastc=='.' && (base==10 || kdbexpv==0)) {
		frpt=0; base=10;
		while (isdigit(kdbreadchar())) {
			if (frpt)
				continue;
			frpt++;
			if (kdblastc - '0' >= 5)
				kdbexpv++;
		}
	}
	kdbpeekc=kdblastc;
	return (1);
}

static
kdbreadsym()
{
	register char *p;

	p = kdbisymbol;
	do {
		if (p < &kdbisymbol[sizeof(kdbisymbol)-1])
			*p++ = kdblastc;
		(void) kdbreadchar();
	} while (kdbsymchar(1));
	*p++ = 0;
}

static
kdbconvdig(c)
	char c;
{
	if (isdigit(c))
		return (c-'0');
	if (isxdigit(c))
		return (c-'a'+10);
	return (-1);
}

static
kdbsymchar(dig)
{

	if (kdblastc=='\\') {
		(void) kdbreadchar();
		return (1);
	}
	return (isalpha(kdblastc) || kdblastc=='_' || dig && isdigit(kdblastc));
}

kdbvarchk(name)
	register name;
{
	if (isdigit(name))
		return (name-'0');
	if (isalpha(name))
		return ((name&037)-1+10);
	return (-1);
}

static
kdbchkloc(frame)
	long frame;
{

	kdbreadsym();
	do {
		if (kdblocalsym(frame)==0)
			kdberror(kdbBADLOC);
		kdbexpv=kdblocalval;
	} while (!kdbeqsym(kdbcursym->n_un.n_name,kdbisymbol,'~'));
}

kdbeqsym(s1, s2, c)
	register char *s1, *s2;
{

	if (kdbstreq(s1,s2))
		return (1);
	if (*s1 == c && kdbstreq(s1+1, s2))
		return (1);
	return (0);
}

static
kdbstreq(s1, s2)
	char *s1, *s2;
{

	while (*s1 == *s2++)
		if (*s1++ == '\0')
			return (1);
	return (0);
}
