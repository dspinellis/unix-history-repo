#ifndef lint
static char sccsid[] = "@(#)rio.c	1.2 (Berkeley) 8/11/83";
#endif

#include "r.h"
char	ibuf[BUFSIZ];
char	*ip = ibuf;

char	type[] = {
	0,	CRAP,	CRAP,	CRAP,	CRAP,	CRAP,	CRAP,	CRAP,
	CRAP,	'\t',	'\n',	CRAP,	CRAP,	CRAP,	CRAP,	CRAP,
	CRAP,	CRAP,	CRAP,	CRAP,	CRAP,	CRAP,	CRAP,	CRAP,
	CRAP,	CRAP,	CRAP,	CRAP,	CRAP,	CRAP,	CRAP,	CRAP,
	' ',	'!',	'"',	'#',	'$',	'%',	'&',	'\'',
	'(',	')',	'*',	'+',	',',	'-',	'.',	'/',
	DIG,	DIG,	DIG,	DIG,	DIG,	DIG,	DIG,	DIG,
	DIG,	DIG,	':',	';',	'<',	'=',	'>',	'?',
	'@',	LET,	LET,	LET,	LET,	LET,	LET,	LET,
	LET,	LET,	LET,	LET,	LET,	LET,	LET,	LET,
	LET,	LET,	LET,	LET,	LET,	LET,	LET,	LET,
	LET,	LET,	LET,	'[',	'\\',	']',	'^',	'_',
	'`',	LET,	LET,	LET,	LET,	LET,	LET,	LET,
	LET,	LET,	LET,	LET,	LET,	LET,	LET,	LET,
	LET,	LET,	LET,	LET,	LET,	LET,	LET,	LET,
	LET,	LET,	LET,	'{',	'|',	'}',	'~',	0,
};

gtok(s) char *s; {	/* get token into s */
	register c, t;
	register char *p;
	struct nlist *q;

	for(;;) {
		p = s;
		*p++ = c = getchr();
		switch(t = type[c]) {
		case 0:
			if (infptr > 0) {
				fclose(infile[infptr]);
				infptr--;
				continue;
			}
			if (svargc > 1) {
				svargc--;
				svargv++;
				if (infile[infptr] != stdin)
					fclose(infile[infptr]);
				if( (infile[infptr] = fopen(*svargv,"r")) == NULL )
					cant(*svargv);
				linect[infptr] = 0;
				curfile[infptr] = *svargv;
				continue;
			}
			return(EOF);	/* real eof */
		case ' ':
		case '\t':
			while ((c = getchr()) == ' ' || c == '\t')
				;	/* skip others */
			if (c == COMMENT || c == '_') {
				putbak(c);
				continue;
			}
			if (c != '\n') {
				putbak(c);
				*p = '\0';
				return(' ');
			} else {
				*s = '\n';
				*(s+1) = '\0';
				return(*s);
			}
		case '_':
			while ((c = getchr()) == ' ' || c == '\t')
				;
			if (c == COMMENT) {
				putbak(c);
				gtok(s);	/* recursive */
			}
			else if (c != '\n')
				putbak(c);
			continue;
		case LET:
		case DIG:
			while ((t=type[*p = getchr()]) == LET || t == DIG)
				p++;
			putbak(*p);
			*p = '\0';
			if ((q = lookup(s))->name != NULL && q->ydef == 0) {	/* found but not keyword */
				if (q->def != fcnloc) {	/* not "function" */
					pbstr(q->def);
					continue;
				}
				getfname();	/* recursive gtok */
			}
			for (p=s; *p; p++)
				if (*p>='A' && *p<='Z')
					*p += 'a' - 'A';
			for (p=s; *p; p++)
				if (*p < '0' || *p > '9')
					return(LET);
			return(DIG);
		case '[':
			*p = '\0';
			return('{');
		case ']':
			*p = '\0';
			return('}');
		case '$':
		case '\\':
			if ((*p = getchr()) == '(' || *p == ')') {
				putbak(*p=='(' ? '{' : '}');
				continue;
			}
			if (*p == '"' || *p == '\'')
				p++;
			else
				putbak(*p);
			*p = '\0';
			return('$');
		case COMMENT:
			comment[comptr++] = 'c';
			while ((comment[comptr++] = getchr()) != '\n')
				;
			flushcom();
			*s = '\n';
			*(s+1) = '\0';
			return(*s);
		case '"':
		case '\'':
			for (; (*p = getchr()) != c; p++) {
				if (*p == '\\')
					*++p = getchr();
				if (*p == '\n') {
					error("missing quote");
					putbak('\n');
					break;
				}
			}
			*p++ = c;
			*p = '\0';
			return(QUOTE);
		case '%':
			while ((*p = getchr()) != '\n')
				p++;
			putbak(*p);
			*p = '\0';
			return('%');
		case '>': case '<': case '=': case '!': case '^':
			return(peek(p, '='));
		case '&':
			return(peek(p, '&'));
		case '|':
			return(peek(p, '|'));
		case CRAP:
			continue;
		default:
			*p = '\0';
			return(*s);
		}
	}
}

gnbtok(s) char *s; {
	register c;
	while ((c = gtok(s)) == ' ' || c == '\t')
		;
	return(c);
}

getfname() {
	while (gtok(fcname) == ' ')
		;
	pbstr(fcname);
	putbak(' ');
}

peek(p, c1) char *p, c1; {
	register c;
	c = *(p-1);
	if ((*p = getchr()) == c1)
		p++;
	else
		putbak(*p);
	*p = '\0';
	return(c);
}

pbstr(str)
register char *str;
{
	register char *p;

	p = str;
	while (*p++);
	--p;
	if (ip >= &ibuf[BUFSIZ]) {
		error("pushback overflow");
		exit(1);
	}
	while (p > str)
		putbak(*--p);
}

getchr() {
	register c;

	if (ip > ibuf)
		return(*--ip);
	c = getc(infile[infptr]);
	if (c == '\n')
		linect[infptr]++;
	if (c == EOF)
		return(0);
	return(c);
}
