#ifndef lint
static char sccsid[] = "@(#)pic2.c	1.1 (CWI) 85/07/19";
#endif lint

#include	<stdio.h>
#include	<ctype.h>
#include	"pic.h"
#include	"y.tab.h"

extern	FILE	*yyin;
extern	int	lineno;
extern	char	*filename;
extern	int	synerr;

definition(s)	/* collect definition for s and install */
	char *s;	/* definitions picked up lexically */
{
	char buf[2000], *p, *tostring();
	int c, delim;
	struct symtab *stp;

	while ((delim = input()) == ' ' || delim == '\t')
		;
	for (p = buf; (c = input()) != delim; ) {
		if (p >= buf + sizeof(buf)) {
			yyerror("definition of %s is too long", s);
			exit(1);
		}
		if (c == EOF) {
			yyerror("end of file while defining %s", s);
			exit(1);
		}
		*p++ = c;
	}
	*p = '\0';
	p = tostring(buf);
	stp = lookup(s);
	if (stp != NULL) {	/* it's there before */
		if (stp->s_type != DEFNAME) {
			yyerror("%s used as variable and definition\n", s);
			return;
		}
		free(stp->s_val.p);
		stp->s_val.p = p;
	} else {
		YYSTYPE u;
		u.p = p;
		makevar(tostring(s), DEFNAME, u);
	}
	dprintf("installing %s as `%s'\n", s, p);
}

char *argstk[10];	/* pointers to actual arguments in argval */
char argval[1000];	/* arguments stored here end to end */
char *argp;		/* current position in argval */
int argcnt;		/* number of arguments seen so far */

dodef(stp)	/* collect args and push back defn for name in table slot n */
	struct symtab *stp;
{
	int i, c, len;
	char *p;

	argcnt = 0;
	if (input() != '(')
		yyerror("disaster in dodef\n");
	for (argp = argval; (len = getarg(argp)) != -1; argp += len) {
		argstk[argcnt++] = argp;
		if (input() == ')')
			break;
	}
	for (i = argcnt; i < 10; i++)
		argstk[i] = "";
	if (dbg) {
		for (i = 0; i < argcnt; i++)
			printf("arg %d = %s\n", i, argstk[i]);
	}

	/* push them back */
	for (p = stp->s_val.p; *p; p++)
		;		/* find the end */
	for (--p; p >= stp->s_val.p; p--) {
		if (*(p-1) == '$') {
			if (isdigit(*p)) {
				pbstr(argstk[*p - '0' - 1]);
				p--;
			}
			else
				unput(*p);
		} else {
			unput(*p);
		}
	}
}

getarg(p)	/* pick up single argument, store in p, return length */
	char *p;
{
	int n, c, npar;

	n = npar = 0;
	for ( ;; ) {
		c = input();
		if (c == EOF)
			yyerror("end of file in getarg!\n");
		if (npar == 0 && (c == ',' || c == ')'))
			break;
		if (c == '"')	/* copy quoted stuff intact */
			do {
				*p++ = c;
				n++;
			} while ((c = input()) != '"' && c != EOF);
		else if (c == '(')
			npar++;
		else if (c == ')')
			npar--;
		n++;
		*p++ = c;
	}
	*p = 0;
	unput(c);
	return(n + 1);
}

#define	PBSIZE	2000
char	pbuf[PBSIZE];		/* pushback buffer */
char	*pb	= pbuf-1;	/* next pushed back character */

char	ebuf[200];		/* collect input here for error reporting */
char	*ep	= ebuf;

input()
{
	register int c;

	if (pb >= pbuf) {
		c = *pb--;
	} else {
		c = getc(yyin);
		if (c == '\n')
			lineno++;
		else if (c == EOF) {
			yyerror("end of file inside .PS/.PE");
			exit(1);
		}
	}
	if (ep >= ebuf + sizeof ebuf)
		ep = ebuf;
	return (*ep++ = c);
}

unput(c)
{
	if (++pb >= pbuf + sizeof pbuf) {
		yyerror("pushback overflow\n");
		exit(1);
	}
	if (--ep < ebuf)
		ep = ebuf + sizeof(ebuf) - 1;
	return(*pb = c);
}

pbstr(s)
	char *s;
{
	int n;

	n = strlen(s);
	while (--n >= 0)
		unput(s[n]);
}

yyerror(s, s1, s2, s3, s4)
	char *s, *s1, *s2, *s3, *s4;
{
	if (synerr)
		return;
	fprintf(stderr, "pic: ");
	fprintf(stderr, s, s1, s2, s3, s4);
	fprintf(stderr, " near line %d, file %s\n", lineno, filename);
	eprint();
	synerr = 1;
}

eprint()	/* try to print context around error */
{
	char *p, *q;
	int c;

	p = ep - 1;
	if (p > ebuf && *p == '\n')
		p--;
	for ( ; p >= ebuf && *p != '\n'; p--)
		;
	while (*p == '\n')
		p++;
	fprintf(stderr, " context is\n\t");
	while (p < ep)
		putc(*p++, stderr);
	fprintf(stderr, " ^ ");
	while (pb >= pbuf)
		putc(*pb--, stderr);
	fgets(ebuf, sizeof ebuf, yyin);
	fprintf(stderr, "%s", ebuf);
	pbstr(".PE\n");	/* safety first */
	ep = ebuf;
}

yywrap() {;}
