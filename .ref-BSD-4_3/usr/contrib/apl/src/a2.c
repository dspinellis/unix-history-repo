static char Sccsid[] = "a2.c @(#)a2.c	1.1	10/1/82 Berkeley ";
#include "apl.h"
#include "aplmap.h"

int chartab[];
char *ecvt();

ex_print()
{

	if(epr0())
		putchar('\n');
}

ex_hprint()
{

	epr0();
	pop();
}

epr0()
{
	register struct item *p;
	register data *dp;
	register i;
	int j;
	int param[4];

	p = fetch1();
	if(p->type == DU)
		return(0);
	if(p->size == 0)
		return(1);
	if(p->type == DA) {

		/* Use "epr1()" to figure out the maximum field width
		 * required by any of the values to be printed.
		 */

		for(i=0; i<4; i++)
			param[i] = 0;
		dp = p->datap;
		for(i=0; i<p->size; i++)
			epr1(*dp++, param);
		i = param[1] + param[2]; /* size if fp */
		if(i > thread.digits)
			i += 100;               /* set "e" format flag */
		if(param[2])
			i++;
		if(i > param[0]+5) {
			i = param[0] + 5; /* size if ep */
			param[1] = param[0];
			param[2] = -1;
		}
		if(param[3])
			i++;	/* sign */
		i++;		/* leading space */
		param[0] = i;
		dp = p->datap;
	}
	bidx(p);
	for(i=1; i<p->size; i++) {
		if(intflg)
			break;
		if(p->type == CH) {
			j = getdat(p);
			putchar(j);
		} else
			epr2(*dp++, param);
		for(j=p->rank-2; j>=0; j--)
			if(i%idx.del[j] == 0)
				putchar('\n');  /* end of dimension reached */
	}
	if(p->type == CH) {
		j = getdat(p);
		putchar(j);
	} else
		epr2(*dp, param);
	return(1);
}

epr1(d, param)
data d;
int *param;
{
	double f;
	register a;
	register char *c;
	int dp, sg;


	/* This routine figures out the field with required by the value
	 * "d".  It adjusts the four elements of "param" so that they
	 * contain the maximum of their old values or the requirements for
	 * the current data item.
	 *
	 * param[0] = number of significant digits
	 * param[1] = number of digits to left of decimal point
	 * param[2] = number of digits to right of decimal point
	 * param[3] = 0 if positive, 1 if negative
	 */

	f = d;
	c = ecvt(f, thread.digits, &dp, &sg);
	if (f == zero)                  /* kludge due to change in ecvt */
		dp = 1;
	a = thread.digits;
	while(c[a-1]=='0' && a>1)
		a--;
	if(a > param[0])		/* sig digits */
		param[0] = a;
	a -= dp;
	if(a < 0)
		a = 0;
	if(a > param[2])		/* digits to right of dp */
		param[2] = a;
	if(dp > param[1])		/* digits to left of dp */
		param[1] = dp;
	param[3] |= sg;		/* and sign */
}

epr2(d, param)
int *param;
data d;
{
	register i;
	register char *c, *mc;
	double f;
	int dp, sg;

	if(param[0]+column > thread.width && !mencflg) {
		putchar('\n');
		putto(param[0]);
	}
	f = d;
	c = ecvt(f, thread.digits, &dp, &sg);
	if (f == zero)
		dp = 1;                 /* kludge due to change in ecvt */
	mc = c + thread.digits;
	putchar(' ');
	sg = sg? '-': ' ';	/* '-' used to be '"' */
	if(param[2] < 0) {
		if(param[3])
			putchar(sg);
		for(i=0; i<param[1]; i++) {
			putchar(*c++);
			if(i == 0)
				putchar('.');
		}
		putchar('e');
		dp--;
		if(dp < 0) {
			putchar('-');	/* '=' used to be '"' */
			dp = -dp;
		} else
			putchar('+'); /* apl style plus sign, used to be ':' */
		putchar(dp/10 + '0');
		putchar(dp%10 + '0');
		return;
	}
	i = dp;
	if(i < 0)
		i = 0;
	for(; i<param[1]; i++)
		putchar(' ');
	if(param[3])
		putchar(sg);
	for(i=0; i<dp; i++)
		if(c >= mc)
			putchar('0'); else
			putchar(*c++);
	for(i=0; i<param[2]; i++) {
		if(i == 0)
			putchar('.');
		if(dp < 0) {
			putchar('0');
			dp++;
		} else
		if(c >= mc)
			putchar('0'); else
			putchar(*c++);
	}
}

error(s)
char *s;
{
	register c;
	register char *cp, *cs;

	intflg = 0;
	if(ifile) {
		CLOSEF(ifile);
		ifile = 0;
	}
	cp = s;
	while(c = *cp++) {
		if(c >= 'A' && c <= 'Z') {
			switch(c) {

			case 'I':
				cs = "\ninterrupt";
				break;

			case 'L':
				cs = "L";
				break;

			case 'C':
				cs = "conformability";
				break;

			case 'S':
				cs = "syntax";
				break;

			case 'R':
				cs = "rank";
				break;

			case 'X':
				cs = "index";
				break;

			case 'Y':
				cs = "character";
				break;

			case 'M':
				cs = "memory";
				break;

			case 'D':
				cs = "domain";
				break;

			case 'T':
				cs = "type";
				break;

			case 'E':
				cs = "error";
				break;

			case 'P':
				cs = "programmer";
				break;

			case 'B':
				cs = "botch";
				break;

			default:
				putchar(c);
				continue;
			}
			printf(cs);
			continue;
		}
		putchar(c);
	}
	putchar('\n');
	if (prwsflg) exit(0);		/* if "prws", just exit */
	/*
	 * produce traceback and mark state indicator.
	 */
	tback(0);
	if(gsip)
		gsip->suspended = 1;
	else {
		while(sp > stack)
			pop();		/* zap garbage */
		reset();
	}
	mainloop();			/* reenter mainloop */
}

printf(f, a)
char *f;
{
	register char *s, *cp;
	register *p;

	s = f;
	p = &a;
	while(*s) {
		if(s[0] == '%')
			switch(s[1]){
			case 'd':
				putn(*p++);
				s += 2;
				continue;
			case 'o':
				puto(*p++);
				s += 2;
				continue;
			case 's':
				cp = (char *)*p++;
				s += 2;
				while(*cp)
					putchar(*cp++);
				continue;
			case 'f':
				putf(p);
				p += 4;	/* 4 words per floating arg */
				s += 2;
				continue;
			}
		putchar(*s++);
	}
}

putn(n)
{
	register a;

	if(n < 0) {
		n = -n;
		if(n < 0) {
			printf("32768");
			return;
		}
		putchar('-');	/* apl minus sign, was '"' */
	}
	if(a=n/10)
		putn(a);
	putchar(n%10 + '0');
}

putf(p)
data *p;
{
	int param[4];
	register int i;

	param[1] = param[2] = param[3] = param[0] = 0;
	epr1(*p, param);
	i = param[1] + param[2]; /* size if fp */
	if(i > thread.digits)
		i += 100;
	if(param[2])
		i++;
	if(i > param[0]+5) {
		i = param[0] + 5; /* size if ep */
		param[1] = param[0];
		param[2] = -1;
	}
	if(param[3])
		i++;	/* sign */
	i++;		/* leading space */
	param[0] = i;
	epr2(*p, param);
/*
 *	register i,j;
 *
 *	i = *p;
 *	j = (*p * 1000.0) - (i * 1000.0);
 *	putn(i);
 *	putchar('.');
 *	putchar('0' + j/100);
 *	putchar('0' + (j/10)%10);
 *	putchar('0' + j%10);
 */
}

puto(n)
{
	if(n&0177770)
		puto( (n>>3) & 017777);
	putchar( '0' + (n&07));
}

getchar()
{
	int c;

	c = 0;
	if(READF(ifile, &c, 1) == 1 && echoflg == 1 && !ifile)
			WRITEF(1, &c, 1);

	/* The following code converts the input character
	 * to the ASCII equivalent (internal format) if
	 * terminal character mapping is in force.
	 */

	if (apl_term && c >= 041 && !ifile) c = map_ascii[(c&0177)-041];
	if (c && protofile && ifile == 0) WRITEF(protofile, &c, 1);

	return(c);
}

putchar(c)
{
	register i;


	/* This is the basic character output routine.  If "mencflg"
	 * is zero, output is performed on file descriptor 1.  If
	 * "menclfg" is non-zero, output is placed into the buffer
	 * pointed to by "mencptr".
	 */

	if(mencflg) {                   /* Format operator */
		if(c != '\n') {
			mencflg = 1;
			*mencptr++ = c;
		}
		else
			if(mencflg > 1)
				mencptr += rowsz;
			else
				mencflg = 2;
		return;
	}


	switch(c){                      /* Normal output */

	case '\0':
		return;

	case '\b':
		if(column)
			column--;
		break;

	case '\t':
		column = (column+8) & ~7;
		break;

	case '\r':
	case '\n':
		column = 0;
		break;

	default:
		column++;
	}

	if (column > thread.width) printf("\n    ");    /* adjust for width */

	if(intflg == 0) {
		if(c & 0200) {
			i = chartab[c & 0177];
			putchar(i>>8);
			c = i & 0177;
			putchar('\b');
		}

		if(protofile)
			WRITEF(protofile, &c, 1);


		/* The following code converts the internal value
		 * to the APL character for modified terminals
		 * if the APL conversion was requested.
		 */

		if (apl_term && c >= 041)
			c = map_apl[c-041];
#ifdef PURDUE_EE
		if (apl_term && c == 010)
			c = '^';
#endif

		WRITEF(1, &c, 1);
#ifdef NBUF
		if (c == '\n' && !prwsflg)
			newbuf(files[1].fd_buf, 1);
#endif
	}
}

char *ty[] = {
0,"DA","CH","LV","QD","QQ","IN","EL","NF","MF","DF","QC","QV","DU","QX","LB"
};

dstack()
{
	register struct item **p;
	register i,n;

	p = sp;
	n = 0;
	while(--p > stack){
		printf("\t%o:  sp[%d]:   type = ", p, --n);
		if((i=(*p)->type) >= 0 && i <= LBL && ty[i])
			printf(ty[i]);
		else
			printf("%d", (*p)->type);
		switch(i){
		default:
			putchar('\n');
			break;
		case LV:
			printf(",  n = %s\n", ((struct nlist *)*p)->namep);
			break;

		case CH:
			if((*p)->size == 0)
				goto nullone;
			if((*p)->rank == 1){
				printf(",  \"");
				for(i=0; i<(*p)->size; i++)
					putchar(((struct chrstrct *)(*p)->datap)->c[i]);
				printf("\"\n");
			} else
				goto rnk;
			break;

		case DA:
		case LBL:
			if((*p)->size == 0)
				goto nullone;
			if((*p)->rank == 0){
				printf(",  v = %f\n", (*p)->datap[0]);
			}
			break;
		rnk:
			printf(",  rank = %d\n", (*p)->rank);
			break;

		nullone:
			printf(",  <null>\n");
			break;
		}
	}
	putchar('\n');
}

