#
/*
 *
 *	UNIX debugger
 *
 */

#include "defs.h"


INT		mkfault;
INT		infile;
INT		outfile 1;
INT		maxpos;

CHAR		printbuf[MAXLIN];
CHAR		*printptr printbuf;
CHAR		*digitptr;


eqstr(s1, s2)
	REG STRING	s1, s2;
{
	REG STRING	 es1;
	es1 = s1+8;
	WHILE *s1++ == *s2
	DO IF *s2++ == 0 ORF s1>=es1
	   THEN return(1);
	   FI
	OD
	return(0);
}

length(s)
	STRING		s;
{
	INT		n 0;
	WHILE *s++ DO n++; OD
	return(n);
}

printc(c)
	CHAR		c;
{
	CHAR		d;
	STRING		q;
	INT		posn, tabs, p;

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
	     write(outfile,printbuf,q-printbuf);
	     printptr=printbuf;
	ELIF c==TB
	THEN *printptr++=SP;
	     WHILE (printptr-printbuf)&7 DO *printptr++=SP; OD
	ELIF c
	THEN printptr++;
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

printf(fmat,a1)
	STRING		fmat;
	STRING		*a1;
{
	STRING		fptr, s;
	INT		*vptr;
	L_INT		*dptr;
	L_REAL		*rptr;
	INT		width, prec;
	CHAR		c, adj;
	INT		x, decpt, n;
	L_INT		lx;
	CHAR		digits[64];

	fptr = fmat; vptr = &a1;

	WHILE c = *fptr++
	DO  IF c!='%'
	    THEN printc(c);
	    ELSE IF *fptr=='-' THEN adj='l'; fptr++; ELSE adj='r'; FI
		 width=convert(&fptr);
		 IF *fptr=='.' THEN fptr++; prec=convert(&fptr); ELSE prec = -1; FI
		 digitptr=digits;
		 dptr=rptr=vptr; lx = *dptr; x = *vptr++;
		 s=0;
		 switch (c = *fptr++) {

		    case 'd':
		    case 'u':
			printnum(x,c,10); break;
		    case 'o':
			printoct(0,x,0); break;
		    case 'q':
			lx=x; printoct(lx,-1); break;
		    case 'x':
			printdbl(0,x,c,16); break;
		    case 'Y':
			printdate(lx); vptr++; break;
		    case 'D':
		    case 'U':
			printdbl(lx,c,10); vptr++; break;
		    case 'O':
			printoct(lx,0); vptr++; break;
		    case 'Q':
			printoct(lx,-1); vptr++; break;
		    case 'X':
			printdbl(lx,'x',16); vptr++; break;
		    case 'c':
			printc(x); break;
		    case 's':
			s=x; break;
		    case 'f':
		    case 'F':
			vptr += 7;
			s=ecvt(*rptr, prec, &decpt, &n);
			*digitptr++=(n?'-':'+');
			*digitptr++ = (decpt<=0 ? '0' : *s++);
			IF decpt>0 THEN decpt--; FI
			*digitptr++ = '.';
			WHILE *s ANDF prec-- DO *digitptr++ = *s++; OD
			WHILE *--digitptr=='0' DONE
			digitptr += (digitptr-digits>=3 ? 1 : 2);
			IF decpt
			THEN *digitptr++ = 'e'; printnum(decpt,'d',10);
			FI
			s=0; prec = -1; break;
		    case 'm':
			vptr--; break;
		    case 'M':
			width=x; break;
		    case 'T':
		    case 't':
			IF c=='T'
			THEN width=x;
			ELSE vptr--;
			FI
			IF width
			THEN width -= charpos()%width;
			FI
			break;
		    default:
			printc(c); vptr--;
		}

		IF s==0
		THEN *digitptr=0; s=digits;
		FI
		n=length(s);
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
	REG INT		i;
	REG STRING	timeptr;
	timeptr = ctime(&tvec);
	FOR i=20; i<24; i++ DO *digitptr++ = *(timeptr+i); OD
	FOR i=3; i<19; i++ DO *digitptr++ = *(timeptr+i); OD
} /*printdate*/

prints(s)
char *s;
{	printf("%s",s);
}

newline()
{
	printc(EOR);
}

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

printnum(n,fmat,base)
	REG INT		n;
{
	REG CHAR	k;
	REG INT		*dptr;
	INT		digs[15];
	dptr=digs;
	IF n<0 ANDF fmat=='d' THEN n = -n; *digitptr++ = '-'; FI
	WHILE n
	DO  *dptr++ = ((POS)n)%base;
	    n=((POS)n)/base;
	OD
	IF dptr==digs THEN *dptr++=0; FI
	WHILE dptr!=digs
	DO  k = *--dptr;
	    *digitptr++ = (k+(k<=9 ? '0' : 'a'-10));
	OD
}

printoct(o,s)
	L_INT		o;
	INT		s;
{
	INT		i;
	L_INT		po = o;
	CHAR		digs[12];

	IF s
	THEN IF po<0
	     THEN po = -po; *digitptr++='-';
	     ELSE IF s>0 THEN *digitptr++='+'; FI
	     FI
	FI
	FOR i=0;i<=11;i++
	DO digs[i] = po&7; po =>> 3; OD
	digs[10] &= 03; digs[11]=0;
	FOR i=11;i>=0;i--
	DO IF digs[i] THEN break; FI OD
	FOR i++;i>=0;i--
	DO *digitptr++=digs[i]+'0'; OD
}

printdbl(lx,ly,fmat,base)
INT lx, ly; char fmat; int base;
{	int digs[20]; int *dptr; char k;
	L_REAL f ,g; long q;
	dptr=digs;
	IF fmat!='D'
	THEN	f=leng(lx); f *= itol(1,0); f += leng(ly);
		IF fmat=='x' THEN *digitptr++='#'; FI
	ELSE	f=itol(lx,ly);
		IF f<0 THEN *digitptr++='-'; f = -f; FI
	FI
	WHILE f
	DO  q=f/base; g=q;
	    *dptr++ = f-g*base;
	    f=q;
	OD
	IF dptr==digs THEN *dptr++=0; FI
	WHILE dptr!=digs
	DO  k = *--dptr;
	    *digitptr++ = (k+(k<=9 ? '0' : 'a'-10));
	OD
}

iclose()
{
	IF infile
	THEN	close(infile); infile=0;
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
	IF charpos()>=maxpos
	THEN printf("\n");
	FI
}
