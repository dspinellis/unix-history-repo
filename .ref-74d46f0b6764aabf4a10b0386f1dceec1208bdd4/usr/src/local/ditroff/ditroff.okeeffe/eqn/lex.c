#ifndef lint
static char sccsid[] = "@(#)lex.c	2.2 (CWI) 87/04/01";
#endif lint
#include "e.h"
#include "y.tab.h"
#include "ctype.h"

#define	SSIZE	400
char	token[SSIZE];
int	sp;

yylex()
{
	register int c;
	tbl *tp;

  begin:
	while ((c=input()) == ' ' || c == '\n')
		;
	yylval = c;
	switch (c) {
	case EOF:
		error(!FATAL, "unexpected end of input inside equation");
		return(EOF);
	case '~':
		return(SPACE);
	case '^':
		return(THIN);
	case '\t':
		return(TAB);
	case '{':
		return('{');
	case '}':
		return('}');
	case '"':
		for (sp = 0; (c=input())!='"' && c != '\n'; ) {
			if (c == '\\')
				if ((c = input()) != '"')
					token[sp++] = '\\';
			token[sp++] = c;
			if (sp >= SSIZE)
				error(FATAL, "quoted string %.20s... too long", token);
		}
		token[sp] = '\0';
		yylval = (int) &token[0];
		if (c == '\n')
			error(!FATAL, "missing \" in %.20s", token);
		return(QTEXT);
	}
	if (!display && c == righteq)
		return(EOF);

	unput(c);
	getstr(token, SSIZE);
	dprintf(".\tlex token = |%s|\n", token);
	if ((tp = lookup(deftbl, token, NULL)) != NULL) {	/* defined term */
		c = input();
		unput(c);
		if (c == '(')	/* macro with args */
			dodef(tp);
		else {		/* no args */
			unput(' ');
			pbstr(tp->defn);
			dprintf(".\tfound %s|=%s|\n", token, tp->defn);
		}
		goto begin;
	}

	if ((tp = lookup(keytbl, token, NULL)) == NULL)	/* not a keyword */
		return CONTIG;

	switch ((int) tp->defn) {		/* some kind of keyword */
	case DEFINE: case TDEFINE: case NDEFINE:
		define(tp->defn);
		break;
	case IFDEF:
		ifdef();
		break;
	case DELIM:
		delim();
		break;
	case GSIZE:
		globsize();
		break;
	case GFONT:
		globfont();
		break;
	case INCLUDE:
		include();
		break;
	case SPACE:
		space();
		break;
	case DOTEQ:
			/* .EQ inside equation -- should warn if at bottom level */
		break;
	case DOTEN:
		if (curfile == infile)
			return EOF;
		/* else ignore nested .EN */
		break;
	default:
		return (int) tp->defn;
	}
	goto begin;
}

getstr(s, n)
	char *s;
	register int n;
{
	register int c;
	register char *p;

	p = s;
	while ((c = input()) == ' ' || c == '\n')
		;
	if (c == EOF) {
		*s = 0;
		return;
	}
	while (c != ' ' && c != '\t' && c != '\n' && c != '{' && c != '}'
	    && c != '"' && c != '~' && c != '^') {
		if (!display && c == righteq)
			break;
		if (c == '(' && p > s) {	/* might be defined(...) */
			*p = '\0';
			if (lookup(deftbl, s, NULL) != NULL)
				break;
		}
		if (c == '\\')
			if ((c = input()) != '"')
				*p++ = '\\';
		*p++ = c;
		if (--n <= 0)
			error(FATAL, "token %.20s... too long", s);
		c = input();
	}
	unput(c);
	*p = '\0';
	yylval = (int) s;
}

cstr(s, quote, maxs)
	char *s;
	int quote;
{
	int del, c, i;

	s[0] = 0;
	while ((del=input()) == ' ' || del == '\t')
		;
	if (quote)
		for (i=0; (c=input()) != del && c != EOF;) {
			s[i++] = c;
			if (i >= maxs)
				return(1);	/* disaster */
		}
	else {
		if (del == '\n')
			return(1);
		s[0] = del;
		for (i=1; (c=input())!=' ' && c!= '\t' && c!='\n' && c!=EOF;) {
			s[i++] = c;
			if (i >= maxs)
				return(1);	/* disaster */
		}
	}
	s[i] = '\0';
	if (c == EOF)
		error(FATAL, "Unexpected end of input at %.20s", s);
	return(0);
}

define(type)
	int type;
{
	char *p1, *p2;

	getstr(token, SSIZE);	/* get name */
	if (type != DEFINE) {
		cstr(token, 1, SSIZE);	/* skip the definition too */
		return;
	}
	p1 = strsave(token);
	if (cstr(token, 1, SSIZE))
		error(FATAL, "Unterminated definition at %.20s", token);
	p2 = strsave(token);
	lookup(deftbl, p1, p2);
	dprintf(".\tname %s defined as %s\n", p1, p2);
}

ifdef()		/* do body if name is defined */
{
	tbl *tp;
	char name[100], *p;

	getstr(name, sizeof(name));	/* get name */
	cstr(token, 1, SSIZE);		/* and body */
	if ((tp = lookup(deftbl, name, NULL)) != NULL) {	/* found it */
		p = strsave(token);
		pushsrc(Free, p);
		pushsrc(String, p);
	}
}

char	*spaceval	= NULL;

space()	/* collect line of form "space amt" to replace \x in output */
{
	getstr(token, SSIZE);
	spaceval = strsave(token);
	dprintf(".\tsetting spaceval to %s\n", token);
}

char *strsave(s)
	char *s;
{
	register char *q;

	q = malloc(strlen(s)+1);
	if (q == NULL)
		error(FATAL, "out of space in strsave on %s", s);
	strcpy(q, s);
	return(q);
}

include()
{
	char name[100];
	FILE *fin;
	int c;
	extern int errno;

	while ((c = input()) == ' ')
		;
	unput(c);
	cstr(name, c == '"', sizeof(name));	/* gets it quoted or not */
	if ((fin = fopen(name, "r")) == NULL)
		fatal("can't open file %s", name);
	errno = 0;
	curfile++;
	curfile->fin = fin;
	curfile->fname = strsave(name);
	curfile->lineno = 0;
	printf(".lf 1 %s\n", curfile->fname);
	pushsrc(File, curfile);
}

delim()
{
	yyval = eqnreg = 0;
	if (cstr(token, 0, SSIZE))
		error(FATAL, "Bizarre delimiters");
	lefteq = token[0];
	righteq = token[1];
        if (!isprint(lefteq) || !isprint(righteq))
		error(FATAL, "Bizarre delimiters");
	if (lefteq == 'o' && righteq == 'f')
		lefteq = righteq = '\0';
}
