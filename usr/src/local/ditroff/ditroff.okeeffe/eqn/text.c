#ifndef lint
static char sccsid[] = "@(#)text.c	2.2 (CWI) 87/04/01";
#endif lint
#include "e.h"
#include "y.tab.h"
#include <ctype.h>

#define	CSSIZE	400
char	cs[CSSIZE+20];	/* text string converted into this */
char	*csp;		/* next spot in cs[] */
char	*psp;		/* next character in input token */

int	lf, rf;		/* temporary spots for left and right fonts */
int	lastft;		/* last \f added */
int	nextft;		/* next \f to be added */

text(t, p1)	/* convert text string p1 of type t */
	int t;
	char *p1;
{
	int c;
	char *p;
	tbl *tp;

	yyval = salloc();
	ebase[yyval] = 0;
	eht[yyval] = EM(1.0, ps);	/* ht in ems of orig size */
	eps[yyval] = ps;
	lfont[yyval] = rfont[yyval] = ROM;
	if (t == QTEXT) {
		for (p = p1; *p; p++)	/* scan for embedded \f's */
			if (*p == '\\' && *(p+1) == 'f')
				break;
		if (*p)		/* if found \f, leave it alone and hope */
			p = p1;
		else {
			sprintf(cs, "\\f%s%s\\fP", ftp->name, p1);
			p = cs;
		}
	} else if (t == SPACE)
		p = "\\ ";
	else if (t == THIN)
		p = "\\|";
	else if (t == TAB)
		p = "\\t";
	else if ((tp = lookup(restbl, p1, NULL)) != NULL) {
		p = tp->defn;
	} else {
		lf = rf = 0;
		/* sprintf(cs, "\\f%s", ftp->name); */
		lastft = 0;
		csp = cs;
		for (psp = p1; (c = *psp++) != '\0'; ) {
			nextft = ft;
			rf = trans(c, p1);
			if (lf == 0)
				lf = rf;	/* save first */
			if (csp-cs > CSSIZE)
				error(FATAL,"converted token %.25s... too long",p1);
		}
		sadd("\\fP");
		*csp = '\0';
		p = cs;
		lfont[yyval] = lf;
		rfont[yyval] = rf;
	}
	dprintf(".\t%dtext: S%d <- %s; b=%g,h=%g,lf=%c,rf=%c,ps=%d\n",
		t, yyval, p, ebase[yyval], eht[yyval], lfont[yyval], rfont[yyval], ps);
	printf(".ds %d \"%s\n", yyval, p);
}

trans(c, p1)
	int c;
	char *p1;
{
	int f;

	f = ROM;
	switch (c) {
	case '0': case '1': case '2': case '3': case '4':
	case '5': case '6': case '7': case '8': case '9':
	case ':': case ';': case '!': case '%': case '?':
	case '(': case '[': case ']':
		if (rf == ITAL)
			shim();
		roman(c);
		break;
	case ')':
		if (rf == ITAL)
			halfshim();
		roman(c);
		break;
	case ',':
		roman(c);
		halfshim();
		f = rf;
		break;
	case '.':
		if (rf == ROM)
			roman(c);
		else
			cadd(c);
		f = rf;
		break;
	case '|':
		if (rf == ITAL && ttype != DEV202)
			shim();
		shim(); roman(c); shim();
		break;
	case '=':
		if (rf == ITAL)
			shim();
		name4('e','q');
		break;
	case '+':
		if (rf == ITAL)
			shim();
		name4('p','l');
		break;
	case '>': case '<':
		if (rf == ITAL)
			shim();
		if (*psp == '=') {	/* look ahead for == <= >= */
			name4(c,'=');
			psp++;
		} else {
			cadd(c);  
		}
		break;
	case '-':
		if (rf == ITAL)
			shim();
		if (*psp == '>') {
			name4('-','>');
			halfshim();
			psp++;
		} else {
			name4('m','i');
		}
		break;
	case '/':
		halfshim();
		cadd('/');
		halfshim();
		break;
	case '~': case ' ':
		shim(); shim();
		break;
	case '^':
		shim();
		break;
	case '\\':	/* troff - pass only \(xx without comment */
		if (rf == ITAL)
			shim();
		cadd('\\');
		cadd(c = *psp++);
		if (c == '(' && *psp && *(psp+1)) {
			cadd(*psp++);
			cadd(*psp++);
		} else
			fprintf(stderr, "eqn warning: unquoted troff command \\%c, line %d, file %s\n",
				c, curfile->lineno, curfile->fname);
		break;
	case '\'':
		name4('f','m');
		break;

	case 'f':
		if (ft == ITAL) {
			if (psp == p1+1 || !isalnum(*(psp-2)))
				halfshim();
			cadd('f');
			if (!isalpha(*psp) && *psp != '\0')	/* add \| except in text */
				shim();
			f = ITAL;
		}
		else
			cadd('f');
		break;
	case 'j':
		if (ft == ITAL) {
			sadd("\\^j");
			f = ITAL;
		}
		else
			cadd('j');
		break;
	default:
		cadd(c);
		f = ft==ITAL ? ITAL : ROM;
		break;
	}
	return(f);
}

shim()	/* add a \| space */
{
	sadd("\\|");
}

halfshim()	/* add a \^ space */
{
	sadd("\\^");
}

roman(c)	/* add char c in "roman" font */
	int c;
{
	nextft = ROM;
	cadd(c);
}

name4(c1,c2)
	int c1, c2;
{
	sadd("\\(");
	cadd(c1);
	cadd(c2);
}

sadd(s)		/* add string s to cs */
	char *s;
{
	while (*s)
		cadd(*s++);
}

cadd(c)		/* add char c to end of cs */
	int c;
{
	char *p;

	if (lastft != nextft) {
		if (lastft != 0) {
			*csp++ = '\\';
			*csp++ = 'f';
			*csp++ = 'P';
		}
		*csp++ = '\\';
		*csp++ = 'f';
		if (ftp == ftstack) {	/* bottom level */
			if (ftp->ft == ITAL)	/* usual case */
				*csp++ = nextft;
			else		/* gfont set, use it */
				for (p = ftp->name; *csp = *p++; )
					csp++;
		} else {	/* inside some kind of font ... */
			for (p = ftp->name; *csp = *p++; )
				csp++;
		}
		lastft = nextft;
	}
	*csp++ = c;
}
