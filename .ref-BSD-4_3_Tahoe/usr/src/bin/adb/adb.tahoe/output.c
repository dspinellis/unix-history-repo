#ifndef lint
static	char sccsid[] = "@(#)output.c	1.2 (Berkeley) 10/22/87";
#endif
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"

INT		mkfault;
INT		infile;
INT		outfile = 1;
L_INT		maxpos;
ADDR		maxoff;
INT		radix = 16;

CHAR		printbuf[MAXLIN];
CHAR		*printptr = printbuf;
CHAR		*digitptr;
MSG		TOODEEP;

printc(c)
	CHAR		c;
{
	CHAR		d;
	REG	STRING	q;
	REG		posn, tabs, p;

	IF mkfault
	THEN	return;
	ELIF (*printptr=c)==EOR
	THEN tabs=0; posn=0; q=printbuf;
	     FOR p=0; p<printptr-printbuf; p++
	     DO d=printbuf[p];
		IF (p&7)==0 ANDF posn
		THEN tabs++; posn=0;
		FI
		IF d==SP
		THEN posn++;
		ELSE WHILE tabs>0 DO *q++=TB; tabs--; OD
		     WHILE posn>0 DO *q++=SP; posn--; OD
		     *q++=d;
		FI
	     OD
	     *q++=EOR;
#ifdef EDDT
		printptr=printbuf; do putchar(*printptr++); while (printptr<q);
#else
	     write(outfile,printbuf,q-printbuf);
#endif
	     printptr=printbuf;
	ELIF c==TB
	THEN *printptr++=SP;
	     WHILE (printptr-printbuf)&7 DO *printptr++=SP; OD
	ELIF c
	THEN printptr++;
	FI
	IF printptr >= &printbuf[MAXLIN-9] THEN
		write(outfile, printbuf, printptr - printbuf);
		printptr = printbuf;
	FI
}

charpos()
{	return(printptr-printbuf);
}

flushbuf()
{	IF printptr!=printbuf
	THEN printc(EOR);
	FI
}

/* VARARGS1 */
printf(fmat,a1)
	STRING		fmat;
	STRING		a1;
{
	STRING	fptr;
	REG	STRING	s;
	REG	L_INT	*dptr;
	L_REAL		*rptr;
	REG		width, prec;
	CHAR		c, adj;
	INT		x, n;
	REG	L_INT	lx;
	CHAR		digits[64];

	fptr = fmat; dptr = (L_INT *)&a1;

	WHILE c = *fptr++
	DO  IF c!='%'
	    THEN printc(c);
	    ELSE IF *fptr=='-' THEN adj='l'; fptr++; ELSE adj='r'; FI
		 width=convert(&fptr);
		 IF *fptr=='.' THEN fptr++; prec=convert(&fptr); ELSE prec = -1; FI
		 digitptr=digits;
		 rptr=(L_REAL *)dptr; x = lx = *dptr++;
		 s=0;
		 switch (c = *fptr++) {

		    case 'd':
			printnum(x, -10); break;
		    case 'u':
			printnum((unsigned short)x, 10); break;
		    case 'o':
			printnum((unsigned short)x, 8); break;
		    case 'q':
			printnum(x, -8); break;
		    case 'x':
			printnum((unsigned short)x, 16); break;
		    case 'z':
			printnum((unsigned short)x, -16); break;
		    case 'R':
			printnum(lx, radix); break;
		    case 'Y':
			printdate(lx); break;
		    case 'D':
			printnum(lx, -10); break;
		    case 'U':
			printnum(lx, 10); break;
		    case 'O':
			printnum(lx, 8); break;
		    case 'Q':
			printnum(lx, -8); break;
		    case 'X':
			printnum(lx, 16); break;
		    case 'Z':
			printnum(lx, -16); break;
		    case 'c':
			printc(x); break;
		    case 's':
			s=(STRING)lx; break;
#ifndef EDDT
		    case 'f':
		    case 'F':
			dptr++;
			(void)sprintf(s=digits, "%*.*f", width, prec, *rptr); prec= -1; break;
#endif
		    case 'm':
			break;
		    case 'M':
			width=x; break;
		    case 'T':
		    case 't':
			IF c=='T'
			THEN width=x;
			ELSE dptr--;
			FI
			IF width
			THEN width -= charpos()%width;
			FI
			break;
		    default:
			printc(c); dptr--;
		}

		IF s==0
		THEN *digitptr=0; s=digits;
		FI
		n=strlen(s);
		n=(prec<n ANDF prec>=0 ? prec : n);
		width -= n;
		IF adj=='r'
		THEN WHILE width-- > 0
		     DO printc(SP); OD
		FI
		WHILE n-- DO printc(*s++); OD
		WHILE width-- > 0 DO printc(SP); OD
		digitptr=digits;
	    FI
	OD
}

printdate(tvec)
	L_INT		tvec;
{
	REG		i;
	REG STRING	timeptr;
	STRING		ctime();

#ifndef EDDT
	timeptr = ctime(&tvec);
#else
	timeptr="????????????????????????";
#endif
	FOR i=20; i<24; i++ DO *digitptr++ = *(timeptr+i); OD
	FOR i=3; i<19; i++ DO *digitptr++ = *(timeptr+i); OD
} /*printdate*/

convert(cp)
REG STRING	*cp;
{
	REG CHAR	c;
	INT		n;
	n=0;
	WHILE ((c = *(*cp)++)>='0') ANDF (c<='9') DO n=n*10+c-'0'; OD
	(*cp)--;
	return(n);
}

printnum(n, base)
	REG POS		n;
{
	REG CHAR	*dptr;
	CHAR		digs[15];
	dptr=digs;
	IF base<0 THEN base = -base;
		IF (L_INT)n<0 THEN n = -n; *digitptr++ = '-'; FI
	FI
	WHILE n
	DO  *dptr++ = n%base;
	    n /= base;
	OD
	IF dptr==digs THEN *dptr++=0; FI
	WHILE dptr!=digs
	DO  n = *--dptr;
	    *digitptr++ = (n+(n<=9 ? '0' : 'a'-10));
	OD
}

#define	MAXIFD	5
struct {
	int	fd;
	int	r9;
} istack[MAXIFD];
int	ifiledepth;

iclose(stack, err)
{
	IF err
	THEN	IF infile
		THEN	close(infile); infile=0;
		FI
		WHILE --ifiledepth >= 0
		DO	IF istack[ifiledepth].fd
			THEN	close(istack[ifiledepth].fd);
			FI
		OD
		ifiledepth = 0;
	ELIF stack == 0
	THEN	IF infile
		THEN	close(infile); infile=0;
		FI
	ELIF stack > 0
	THEN	IF ifiledepth >= MAXIFD
		THEN	error(TOODEEP);
		FI
		istack[ifiledepth].fd = infile;
		istack[ifiledepth].r9 = var[9];
		ifiledepth++;
		infile = 0;
	ELSE	IF infile
		THEN	close(infile); infile=0;
		FI
		IF ifiledepth > 0
		THEN	infile = istack[--ifiledepth].fd;
			var[9] = istack[ifiledepth].r9;
		FI
	FI
}

oclose()
{
	IF outfile!=1
	THEN	flushbuf(); close(outfile); outfile=1;
	FI
}

endline()
{

	if (maxpos <= charpos())
		printf("\n");
}
