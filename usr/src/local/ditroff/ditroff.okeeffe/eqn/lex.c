#ifndef lint
static char *sccsid = "lex.c	(CWI)	1.1	85/03/01";
#endif
#include "e.h"
#include "e.def"
#include "ctype.h"

#define	SSIZE	400
char	token[SSIZE];
int	sp;
#define	putbak(c)	*ip++ = c;
#define	PUSHBACK	300	/* maximum pushback characters */
char	ibuf[PUSHBACK+SSIZE];	/* pushback buffer for definitions, etc. */
char	*ip	= ibuf;

gtc() {
  loop:
	if (ip > ibuf)
		return(*--ip);	/* already present */
	lastchar = getc(curfile);
	if (lastchar=='\n')
		linect++;
	if (lastchar != EOF)
		return(lastchar);
	if (++ifile > svargc) {
		return(EOF);
	}
	if (curfile != stdin)
		fclose(curfile);
	linect = 1;
	if (strcmp(svargv[ifile], "-") == 0) {
		curfile = stdin;
		goto loop;
	}
	if ((curfile=fopen(svargv[ifile], "r")) != NULL)
		goto loop;
	error(FATAL, "can't open file %s", svargv[ifile]);
	return(EOF);
}

pbstr(str)
register char *str;
{
	register char *p;

	p = str;
	while (*p++);
	--p;
	if (ip >= &ibuf[PUSHBACK])
		error( FATAL, "pushback overflow");
	while (p > str)
		putbak(*--p);
}

yylex() {
	register int c;
	tbl *tp, *lookup();
	extern tbl **keytbl, **deftbl;

  beg:
	while ((c=gtc())==' ' || c=='\n')
		;
	yylval=c;
	switch(c) {

	case EOF:
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
		for (sp=0; (c=gtc())!='"' && c != '\n'; ) {
			if (c == '\\')
				if ((c = gtc()) != '"')
					token[sp++] = '\\';
			token[sp++] = c;
			if (sp>=SSIZE)
				error(FATAL, "quoted string %.20s... too long", token);
		}
		token[sp]='\0';
		yylval = (int) &token[0];
		if (c == '\n')
			error(!FATAL, "missing \" in %.20s", token);
		return(QTEXT);
	}
	if (c==righteq)
		return(EOF);

	putbak(c);
	getstr(token, SSIZE);
	if (dbg)printf(".\tlex token = |%s|\n", token);
	if ((tp = lookup(&deftbl, token, NULL)) != NULL) {
		putbak(' ');
		pbstr(tp->defn);
		putbak(' ');
		if (dbg)
			printf(".\tfound %s|=%s|\n", token, tp->defn);
	}
	else if ((tp = lookup(&keytbl, token, NULL)) == NULL) {
		if(dbg)printf(".\t%s is not a keyword\n", token);
		return(CONTIG);
	}
	else if (tp->defn == (char *) DEFINE || tp->defn == (char *) NDEFINE || tp->defn == (char *) TDEFINE)
		define(tp->defn);
	else if (tp->defn == (char *) DELIM)
		delim();
	else if (tp->defn == (char *) GSIZE)
		globsize();
	else if (tp->defn == (char *) GFONT)
		globfont();
	else if (tp->defn == (char *) INCLUDE)
		include();
	else if (tp->defn == (char *) SPACE)
		space();
	else {
		return((int) tp->defn);
	}
	goto beg;
}

getstr(s, n) char *s; register int n; {
	register int c;
	register char *p;

	p = s;
	while ((c = gtc()) == ' ' || c == '\n')
		;
	if (c == EOF) {
		*s = 0;
		return;
	}
	while (c != ' ' && c != '\t' && c != '\n' && c != '{' && c != '}'
	  && c != '"' && c != '~' && c != '^' && c != righteq) {
		if (c == '\\')
			if ((c = gtc()) != '"')
				*p++ = '\\';
		*p++ = c;
		if (--n <= 0)
			error(FATAL, "token %.20s... too long", s);
		c = gtc();
	}
	if (c=='{' || c=='}' || c=='"' || c=='~' || c=='^' || c=='\t' || c==righteq)
		putbak(c);
	*p = '\0';
	yylval = (int) s;
}

cstr(s, quote, maxs) char *s; int quote; {
	int del, c, i;

	s[0] = 0;
	while((del=gtc()) == ' ' || del == '\t')
		;
	if (quote)
		for (i=0; (c=gtc()) != del && c != EOF;) {
			s[i++] = c;
			if (i >= maxs)
				return(1);	/* disaster */
		}
	else {
		if (del == '\n')
			return(1);
		s[0] = del;
		for (i=1; (c=gtc())!=' ' && c!= '\t' && c!='\n' && c!=EOF;) {
			s[i++]=c;
			if (i >= maxs)
				return(1);	/* disaster */
		}
	}
	s[i] = '\0';
	if (c == EOF)
		error(FATAL, "Unexpected end of input at %.20s", s);
	return(0);
}

define(type) int type; {
	char *strsave(), *p1, *p2;
	tbl *lookup();
	extern tbl **deftbl;

	getstr(token, SSIZE);	/* get name */
	if (type != DEFINE) {
		cstr(token, 1, SSIZE);	/* skip the definition too */
		return;
	}
	p1 = strsave(token);
	if (cstr(token, 1, SSIZE))
		error(FATAL, "Unterminated definition at %.20s", token);
	p2 = strsave(token);
	lookup(&deftbl, p1, p2);
	if (dbg)printf(".\tname %s defined as %s\n", p1, p2);
}

char	*spaceval	= NULL;

space()	/* collect line of form "space amt" to replace \x in output */
{
	char *strsave();

	getstr(token, SSIZE);
	spaceval = strsave(token);
	if (dbg) printf(".\tsetting space to %s\n", token);
}

char *strsave(s)
char *s;
{
	char *malloc();
	register char *q;

	q = malloc(strlen(s)+1);
	if (q == NULL)
		error(FATAL, "out of space in strsave on %s", s);
	strcpy(q, s);
	return(q);
}

include() {
	error(!FATAL, "Include not yet implemented");
}

delim() {
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
