/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)output.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * adb - output
 */

#include "defs.h"
#include <ctype.h>
#include <stdio.h>
#include <varargs.h>

extern char TOODEEP[];

int	infile;
int	outfile = 1;

char	printbuf[LINELEN];
char	*printptr = printbuf;


/*
 * Print the string s.
 */
prints(s)
	register char *s;
{
	register int c;

	while ((c = *s++) != '\0')
		printc(c);
}

/*
 * Print the character c.
 */
printc(c)
	int c;
{

	if (mkfault)
		return;
	switch (c) {

	case 0:
		return;

	case '\n':
		sendout();
		return;

	default:
		if (isprint(c))
			*printptr++ = c;
		break;
	}
	if (printptr >= &printbuf[LINELEN - 1])	/* 1 == space for \n */
		sendout();
}

/*
 * Send (write) out the contents of the print buffer, compressing
 * spaces into tabs.
 */
static
sendout()
{
	register char *p, *q;
	register int c, off = 0, spaces = 0, s;
#define	tabsize(x) (8 - ((x) & 7))

	for (q = p = printbuf; p < printptr;) {
		c = *p++;
		switch (c) {

		case ' ':
			spaces++;
			break;

		case '\t':
			spaces += tabsize(off + spaces);
			break;

		default:
			s = tabsize(off);
			off += spaces + 1;
			while (spaces >= s) {
				*q++ = '\t';
				spaces -= s;
				s = 8;
			}
			while (--spaces >= 0)
				*q++ = ' ';
			spaces = 0;
			*q++ = c;
		}
	}
	*q++ = '\n';
	(void) write(outfile, printbuf, q - printbuf);
	printptr = printbuf;
#undef tabsize
}

charpos()
{

	return (printptr - printbuf);
}

endline()
{

	if (printptr - printbuf >= maxcol)
		printc('\n');
}

flushbuf()
{

	if (printptr != printbuf)
		sendout();
}

/* this should not be necessary! */
#ifdef lint
#undef va_arg
#define va_arg(ap, type) (ap = ap, (type)0)
#endif

/*
 * Context passed between adbprintf and decodefmt.
 */
struct prf {
	char	*fmt;		/* format pointer */
	va_list	ap;		/* argument pointer */
	char	*buf;		/* digit buffer, or %s string */
	int	adj;		/* 'l'eft (-) or 'r'ight adjustment */
	int	width;		/* width from format */
	int	prec;		/* precision from format */
};

/*
 * adb's very own version of printf() ... of course, all the format
 * escapes are different.  Noteworthy are the %<width>m and %<tabstop>t
 * formats, which move the given width, or to the given tabstop, and
 * the %?a format, which evaluates one argument, and if not zero, prints
 * according to format a.  (Note that any modifiers must appear in the 
 * `a' part, not in the %? part.)
 */
/* VARARGS1 */
adbprintf(fmt, va_alist)
	char *fmt;
	va_dcl
{
	register char *s;
	register int n, c;
	struct prf prf;
	char digits[130];	/* good to at least 128 bit expr_t */

	/* set up the fields adbprf needs */
	prf.fmt = fmt;
	va_start(prf.ap);
	for (;;) {
		/* look for % conversions */
		s = prf.fmt;
		while ((c = *s++) != '%') {
			if (c == 0)
				return;
			printc(c);
		}
		prf.fmt = s;
		prf.buf = digits;
		dofmt(&prf);		/* format one format */
		n = strlen(s = prf.buf);
		if (prf.prec >= 0 && n > prf.prec)
			n = prf.prec;
		c = prf.width - n;
		if (prf.adj == 'r')
			while (--c >= 0)
				printc(' ');
		while (--n >= 0)
			printc(*s++);
		while (--c >= 0)
			printc(' ');
	}
	va_end(prf.ap);
}

/*
 * Do a single format.
 */
static
dofmt(prf)
	register struct prf *prf;
{
	register char *s = prf->fmt;
	register va_list ap = prf->ap;
	register int c, n;
	expr_t v;
	int pluspref = 0;
	static char null[] = "";

	prf->adj = 'r';
	prf->width = 0;
	prf->prec = -1;
more:
	c = *s++;
sw:
	switch (c) {

	case '-':
		prf->adj = 'l';
		goto more;

	case '+':
		pluspref = 1;
		goto more;

	case '*':
		prf->width = va_arg(ap, int);
		goto more;

	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
		for (n = c - '0'; isdigit(c = *s++);)
			n = 10 * n + c - '0';
		prf->width = n;
		goto sw;

	case '.':
		c = *s++;
		if (c == '*') {
			prf->prec = va_arg(ap, int);
			goto more;
		}
		for (n = 0; isdigit(c); c = *s++)
			n = 10 * n + c - '0';
		prf->prec = n;
		goto sw;

	case 'v': case 'V':
		/* print in signed version of current radix */
		if ((n = radix) > 0)
			n = -n;
		goto rprint;

	case 'q': case 'Q': n =  -8; goto rprint; /* octal */
	case 'd': case 'D': n = -10; goto rprint; /* decimal */
	case 'z': case 'Z': n = -16; goto rprint; /* hex */
	case 'o': case 'O': n =   8; goto rprint; /* and */
	case 'u': case 'U': n =  10; goto rprint; /* unsigned */
	case 'x': case 'X': n =  16; goto rprint; /* versions */

	case 'r': case 'R':
		n = radix;
rprint:
		if (isupper(c))
			v = n < 0 ? SF_ARG : UF_ARG;
		else
			v = n < 0 ? SH_ARG : UH_ARG;
		printradix(prf->buf, v, n, pluspref);
		break;

	case 'Y':
		printdate(prf->buf, va_arg(ap, time_t));
		break;

	case 'c':
		*prf->buf = va_arg(ap, int);
		prf->buf[1] = 0;
		break;

	case 's':
		prf->buf = va_arg(ap, char *);
		break;

	case 'f':
		/* here comes stdio ... sigh */
		(void) sprintf(prf->buf, "%+*.*e", prf->width,
		    prf->prec >= 0 ? prf->prec : 16, va_arg(ap, double));
		prf->prec = -1;
		break;

	case 'm':
		prf->buf = null;
		break;

	case 't':
		if (prf->width)
			prf->width -= charpos() % prf->width;
		prf->buf = null;
		break;

	case '?':
		c = va_arg(ap, int);
		prf->fmt = s;
		prf->ap = ap;
		dofmt(prf);
		if (c == 0)
			prf->buf = null;
		return;

	default:
		panic("dofmt");
		/* NOTREACHED */
	}
	prf->fmt = s;
	prf->ap = ap;
}

/*
 * Print the date into the buffer at `p'.
 */
static
printdate(p, tm)
	register char *p;
	time_t tm;
{
	char *asc = ctime(&tm);
	char *strncpy();

	(void) strncpy(p, asc + 20, 4);		/* "1988" */
	(void) strncpy(p + 4, asc + 3, 16);	/* " Aug 18 03:04:49" */
	p[20] = 0;
}

/*
 * Print the value `val' in base `base' into the buffer at `p'.
 * If base is negative, assume the number is signed.
 */
static
printradix(p, val, base, pluspref)
	register char *p;
	register expr_t val;
	register int base;
	int pluspref;
{
	register char *d;
	register expr_t high;
	char digs[128];		/* good to 128 bits minimum */

	if (base < 0) {
		base = -base;
		if ((sexpr_t)val < 0) {
			val = -val;
			*p++ = '-';
		} else if (pluspref)
			*p++ = '+';
	} else if (pluspref)
		*p++ = '+';

	d = digs;
	switch (base) {

	case 8:
		while (val != 0) {
			*d++ = val & 7;
			val >>= 3;
		}
		*d++ = 0;
		break;

	case 16:
		do {
			*d++ = val & 15;
		} while ((val >>= 4) != 0);
		break;

	default:
		do {
			high = val / base;
			*d++ = val - (high * base);
		} while ((val = high) != 0);
		break;
	}
	while (d > digs)
		*p++ = "0123456789abcdef"[*--d];
	*p = 0;
}

/*
 * BEGIN XXX
 * THIS BELONGS ELSEWHERE
 */
#define	MAXIFD	5
struct {
	int fd;
	expr_t v9;
} istack[MAXIFD];
int ifiledepth;

iclose(stack, err)
	int stack, err;
{

	if (err) {
		if (infile) {
			(void) close(infile);
			infile = 0;
		}
		while (--ifiledepth >= 0)
			if (istack[ifiledepth].fd)
				(void) close(istack[ifiledepth].fd);
		ifiledepth = 0;
	} else if (stack == 0) {
		if (infile) {
			(void) close(infile);
			infile = 0;
		}
	} else if (stack > 0) {
		if (ifiledepth >= MAXIFD)
			error(TOODEEP);
		istack[ifiledepth].fd = infile;
		istack[ifiledepth].v9 = var[9];
		ifiledepth++;
		infile = 0;
	} else {
		if (infile) {
			(void) close(infile);
			infile = 0;
		}
		if (ifiledepth > 0) {
			infile = istack[--ifiledepth].fd;
			var[9] = istack[ifiledepth].v9;
		}
	}
}

oclose()
{

	if (outfile != 1) {
		flushbuf();
		(void) close(outfile);
		outfile = 1;
	}
}
