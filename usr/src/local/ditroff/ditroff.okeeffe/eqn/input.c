#ifndef lint
static char sccsid[] = "@(#)input.c	2.2 (CWI) 87/04/01";
#endif lint
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include "e.h"
#include "y.tab.h"

Infile	infile[10];
Infile	*curfile = infile;

#define	MAXSRC	50
Src	src[MAXSRC];	/* input source stack */
Src	*srcp	= src;

pushsrc(type, ptr)	/* new input source */
	int type;
	char *ptr;
{
	if (++srcp >= src + MAXSRC)
		fatal("inputs nested too deep");
	srcp->type = type;
	srcp->sp = ptr;
	if (dbg > 1) {
		printf("\n%3d ", srcp - src);
		switch (srcp->type) {
		case File:
			printf("push file %s\n", ((Infile *)ptr)->fname);
			break;
		case Macro:
			printf("push macro <%s>\n", ptr);
			break;
		case Char:
			printf("push char <%c>\n", *ptr);
			break;
		case String:
			printf("push string <%s>\n", ptr);
			break;
		case Free:
			printf("push free <%s>\n", ptr);
			break;
		default:
			fatal("pushed bad type %d\n", srcp->type);
		}
	}
}

popsrc()	/* restore an old one */
{
	if (srcp <= src)
		fatal("too many inputs popped");
	if (dbg > 1) {
		printf("%3d ", srcp - src);
		switch (srcp->type) {
		case File:
			printf("pop file\n");
			break;
		case Macro:
			printf("pop macro\n");
			break;
		case Char:
			printf("pop char <%c>\n", *srcp->sp);
			break;
		case String:
			printf("pop string\n");
			break;
		case Free:
			printf("pop free\n");
			break;
		default:
			fatal("pop weird input %d\n", srcp->type);
		}
	}
	srcp--;
}

Arg	args[10];	/* argument frames */
Arg	*argfp = args;	/* frame pointer */
int	argcnt;		/* number of arguments seen so far */

dodef(stp)	/* collect args and switch input to defn */
	tbl *stp;
{
	int i, len;
	char *p;
	Arg *ap;

	ap = argfp+1;
	if (ap >= args+10)
		fatal("arguments too deep");
	argcnt = 0;
	if (input() != '(')
		fatal("disaster in dodef\n");
	if (ap->argval == 0)
		ap->argval = malloc(1000);
	for (p = ap->argval; (len = getarg(p)) != -1; p += len) {
		ap->argstk[argcnt++] = p;
		if (input() == ')')
			break;
	}
	for (i = argcnt; i < MAXARGS; i++)
		ap->argstk[i] = "";
	if (dbg)
		for (i = 0; i < argcnt; i++)
			printf("arg %d.%d = <%s>\n", ap-args, i+1, ap->argstk[i]);
	argfp = ap;
	pushsrc(Macro, stp->defn);
}

getarg(p)	/* pick up single argument, store in p, return length */
	char *p;
{
	int n, c, npar;

	n = npar = 0;
	for ( ;; ) {
		c = input();
		if (c == EOF)
			fatal("end of file in getarg!\n");
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

  loop:
	switch (srcp->type) {
	case File:
		c = getc(curfile->fin);
		if (c == EOF) {
			if (curfile == infile)
				break;
			if (curfile->fin != stdin) {
				fclose(curfile->fin);
				free(curfile->fname);	/* assumes allocated */
			}
			curfile--;
			printf(".lf %d %s\n", curfile->lineno, curfile->fname);
			popsrc();
			goto loop;
		}
		if (c == '\n')
			curfile->lineno++;
		break;
	case Char:
		if (pb >= pbuf) {
			c = *pb--;
			popsrc();
			break;
		} else {	/* can't happen? */
			popsrc();
			goto loop;
		}
	case String:
		c = *srcp->sp++;
		if (c == '\0') {
			popsrc();
			goto loop;
		} else {
			if (*srcp->sp == '\0')	/* empty, so pop */
				popsrc();
			break;
		}
	case Macro:
		c = *srcp->sp++;
		if (c == '\0') {
			if (--argfp < args)
				fatal("argfp underflow");
			popsrc();
			goto loop;
		} else if (c == '$' && isdigit(*srcp->sp)) {
			int n = 0;
			while (isdigit(*srcp->sp))
				n = 10 * n + *srcp->sp++ - '0';
			if (n > 0 && n <= MAXARGS)
				pushsrc(String, argfp->argstk[n-1]);
			goto loop;
		}
		break;
	case Free:	/* free string */
		free(srcp->sp);
		popsrc();
		goto loop;
	}
	if (ep >= ebuf + sizeof ebuf)
		ep = ebuf;
	*ep++ = c;
	return c;
}


unput(c)
{
	if (++pb >= pbuf + sizeof pbuf)
		fatal("pushback overflow\n");
	if (--ep < ebuf)
		ep = ebuf + sizeof(ebuf) - 1;
	*pb = c;
	pushsrc(Char, pb);
	return c;
}

pbstr(s)
	char *s;
{
	pushsrc(String, s);
}

fatal(s, s1, s2, s3, s4)	/* should be a flag on error */
	char *s, *s1, *s2, *s3, *s4;
{
	error(FATAL, s, s1, s2, s3, s4);
}

error(die, s, s1, s2, s3, s4)
	int die;
	char *s, *s1, *s2, *s3, *s4;
{
	extern char *cmdname, *sys_errlist[];
	extern int errno, sys_nerr;

	if (synerr)
		return;
	fprintf(stderr, "%s: ", cmdname);
	fprintf(stderr, s, s1, s2, s3, s4);
	if (errno > 0 && errno < sys_nerr)
		fprintf(stderr, " (%s)", sys_errlist[errno]);
	if (curfile->fin)
		fprintf(stderr, " near line %d, file %s",
			curfile->lineno, curfile->fname);
	fprintf(stderr, "\n");
	eprint();
	synerr = 1;
	errno = 0;
	if (die) {
		if (dbg)
			abort();
		else
			exit(1);
	}
}

yyerror() {;}

eprint()	/* try to print context around error */
{
	char *p, *q;
	int c;

	if (ep == ebuf)
		return;				/* no context */
	p = ep - 1;
	if (p > ebuf && *p == '\n')
		p--;
	for ( ; p >= ebuf && *p != '\n'; p--)
		;
	while (*p == '\n')
		p++;
	fprintf(stderr, " context is\n\t");
	for (q=ep-1; q>=p && *q!=' ' && *q!='\t' && *q!='\n'; q--)
		;
	while (p < q)
		putc(*p++, stderr);
	fprintf(stderr, " >>> ");
	while (p < ep)
		putc(*p++, stderr);
	fprintf(stderr, " <<< ");
	while (pb >= pbuf)
		putc(*pb--, stderr);
	if (curfile->fin)
		fgets(ebuf, sizeof ebuf, curfile->fin);
	fprintf(stderr, "%s", ebuf);
	pbstr("\n.EN\n");	/* safety first */
	ep = ebuf;
}
