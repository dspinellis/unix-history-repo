Virgin BTL M4 as sent out in 4.1
#ifndef lint
static char sccsid[] = "@(#)m4.c	1.5 (Berkeley) %G%";
#endif

#include <stdio.h>
#include <signal.h>
#include "pathnames.h"

#define ERROR NULL
#define	READ	"r"
#define	WRITE	"w"

#define	EOS	0
int	lpar	= '(';
#define	LPAR	lpar
#define	RPAR	')'
#define	COMMA	','
#define	GRAVE	'`'
#define	ACUTE	'\''
#define LBRAK	'['
#define RBRAK	']'
#ifdef  M4
char	lquote	LBRAK;
char	rquote	RBRAK;
#endif
#ifndef M4
char	lquote	= GRAVE;
char	rquote	= ACUTE;
#endif
#define	COMMENT	'#'
#define	ALPH	1
#define	DIG	2

#define	HSHSIZ	199	/* prime */
#define	STACKS	50
#define	SAVS	4096
#define	TOKS	128

#define	putbak(c)	*ip++ = c;
#define	getchr()	(ip>cur_ip?*--ip: getc(infile[infptr]))
#define	putchr(c)	if (cp==NULL) {if (curfile)putc(c,curfile);} else *op++ = c
char	type[] = {
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	0,	0,	0,	0,	0,	0,	0,	0,
	DIG,	DIG,	DIG,	DIG,	DIG,	DIG,	DIG,	DIG,
	DIG,	DIG,	0,	0,	0,	0,	0,	0,
	0,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	0,	0,	0,	0,	ALPH,
	0,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,	ALPH,
	ALPH,	ALPH,	ALPH,	0,	0,	0,	0,	0,
};

char	token[TOKS];
char	eoa[]	= "\0";

#define	RESERVED	01	/* This is a reserved word with side action */
struct	nlist {
	char	*name;
	char	*def;
	char	flag;
	struct	nlist *next;
};

struct	nlist	*hshtab[HSHSIZ];
char	ibuf[SAVS+TOKS];
char	obuf[SAVS+TOKS];
char	*op	= obuf;
char	*ip	= ibuf;
char *ip_stk[10] = {ibuf};
char *cur_ip = ibuf;
struct call {
	char	**argp;
	int	plev;
};
struct	call	*cp = NULL;

char	*makeloc;
char	*ifdefloc;
char	*lenloc;
char	*undefloc;
char	*shiftloc;
char	*cqloc;
char	*defloc;
char	*evaloc;
char	*incrloc;
char	*substrloc;
char	*indexloc;
char	*transloc;
char	*ifloc;
char	*divloc;
char	*divnumloc;
char	*undivloc;
char	*dnlloc;
char	*inclloc;
char	*sinclloc;
char	*syscmdloc;
char	*dumploc;
char	*errploc;

char	tempname[] = _PATH_TMP;
struct nlist	*lookup();
char	*install();
char	*malloc();
char	*mktemp();
char	*copy();
long	ctol();
int	hshval;
FILE	*olist[11] = { stdout };
int	okret;
int	curout	= 0;
FILE	*curfile = { stdout };
FILE	*infile[10] = { stdin };
int	infptr	= 0;

main(argc, argv)
char **argv;
{
	char *argstk[STACKS+10];
	struct call callst[STACKS];
	register char *tp, **ap;
	int delexit(), catchsig();
	register t;
	int i;

#ifdef gcos
#ifdef M4
	install("GCOS", eoa, 0);
#endif
#ifndef M4
	install("gcos", eoa, 0);
#endif
#endif
#ifdef unix
#ifdef M4
	install("UNIX", eoa, 0);
#endif
#ifndef M4
	install("unix", eoa, 0);
#endif
#endif

#ifdef M4
	makeloc = install("MAKETEMP", eoa, RESERVED);
	ifdefloc = install("IFDEF", eoa, RESERVED);
	lenloc = install("LEN", eoa, RESERVED);
	undefloc = install("UNDEFINE", eoa, RESERVED);
	shiftloc = install("SHIFT", eoa, RESERVED);
	cqloc = install("CHANGEQUOTE", eoa, RESERVED);
	defloc = install("DEFINE", eoa, RESERVED);
	evaloc = install("EVAL", eoa, RESERVED);
	inclloc = install("INCLUDE", eoa, RESERVED);
	sinclloc = install("SINCLUDE", eoa, RESERVED);
	syscmdloc = install("SYSCMD", eoa, RESERVED);
	dumploc = install("DUMPDEF", eoa, RESERVED);
	errploc = install("ERRPRINT", eoa, RESERVED);
	incrloc = install("INCR", eoa, RESERVED);
	substrloc = install("SUBSTR", eoa, RESERVED);
	indexloc = install("INDEX", eoa, RESERVED);
	transloc = install("TRANSLIT", eoa, RESERVED);
	ifloc = install("IFELSE", eoa, RESERVED);
	divloc = install("DIVERT", eoa, RESERVED);
	divnumloc = install("DIVNUM", eoa, RESERVED);
	undivloc = install("UNDIVERT", eoa, RESERVED);
	dnlloc = install("DNL", eoa, RESERVED);
#endif

#ifndef M4
	makeloc = install("maketemp", eoa, RESERVED);
	ifdefloc = install("ifdef", eoa, RESERVED);
	lenloc = install("len", eoa, RESERVED);
	undefloc = install("undefine", eoa, RESERVED);
	shiftloc = install("shift", eoa, RESERVED);
	cqloc = install("changequote", eoa, RESERVED);
	defloc = install("define", eoa, RESERVED);
	evaloc = install("eval", eoa, RESERVED);
	inclloc = install("include", eoa, RESERVED);
	sinclloc = install("sinclude", eoa, RESERVED);
	syscmdloc = install("syscmd", eoa, RESERVED);
	dumploc = install("dumpdef", eoa, RESERVED);
	errploc = install("errprint", eoa, RESERVED);
	incrloc = install("incr", eoa, RESERVED);
	substrloc = install("substr", eoa, RESERVED);
	indexloc = install("index", eoa, RESERVED);
	transloc = install("translit", eoa, RESERVED);
	ifloc = install("ifelse", eoa, RESERVED);
	divloc = install("divert", eoa, RESERVED);
	divnumloc = install("divnum", eoa, RESERVED);
	undivloc = install("undivert", eoa, RESERVED);
	dnlloc = install("dnl", eoa, RESERVED);
#endif
	ap = argstk;
#ifndef gcos
	if (signal(SIGHUP, SIG_IGN) != SIG_IGN)
		signal(SIGHUP, catchsig);
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, catchsig);
	mktemp(tempname);
	close(creat(tempname, 0));
#endif
#ifdef gcos
	tempname = "m4.tempa";
#endif
	if (argc>1)
		putbak(0);
	for (;;) {
		tp = token;
		*tp++ = t = getchr();
		*tp = EOS;
		if (t<=0) {
			if (infptr > 0) {
				fclose(infile[infptr]);
				infptr--;
				cur_ip = ip_stk[infptr];
				continue;
			}
			if (argc<=1)
				break;
			argc--;
			argv++;
			if (infile[infptr]!=stdin)
				fclose(infile[infptr]);
			if (**argv=='-')
				infile[infptr] = stdin;
			else if ((infile[infptr]=fopen(argv[0], READ))==ERROR) {
				fprintf(stderr, "m4: file not found: %s\n", argv[0]);
				delexit();
			}
			continue;
		}
		if (type[t]==ALPH) {
			while ((t=type[*tp++=getchr()])==ALPH||t==DIG);
			putbak(*--tp);
			*tp = EOS;
			if (*ap = lookup(token)->def) {
				if (++ap >= &argstk[STACKS]) {
					fprintf(stderr, "m4: arg stack overflow\n");
					delexit();
				}
				if (cp==NULL)
					cp = callst;
				else if (++cp > &callst[STACKS]) {
					fprintf(stderr, "m4: call stack overflow\n");
					delexit();
				}
				cp->argp = ap;
				*ap++ = op;
				puttok();
				*op++ = '\0';
				t = getchr();
				putbak(t);
				if (t!=LPAR) {
					/* if (t!=' ' && t!='\t') */
						putbak(')');
					putbak('(');
				}
				else	/* try to fix arg count */
					*ap++ = op;
				cp->plev = 0;
			} else
				puttok();
		} else if (t==lquote) {
			i = 1;
			for (;;) {
				t = getchr();
				if (t==rquote) {
					i--;
					if (i==0)
						break;
				} else if (t==lquote)
					i++;
				else if (t<0) {
					fprintf(stderr, "m4: EOF in string\n");
					delexit();
				}
				putchr(t);
			}
		} else if (t==COMMENT) {
			putbak(t);
			while ((t = getchr())!='\n'&& t>=0)
				if (cp==NULL)
					putchr(t);
			putbak(t);
		} else if (cp==NULL) {
			puttok();
		} else if (t==LPAR) {
			if (cp->plev)
				*op++ = t;
			cp->plev++;
			while ( (t=getchr())==' ' || t=='\t' || t=='\n')
				;	/* skip leading white space during arg collection */
			putbak(t);
/*
		} else if (t==' ' || t=='\t' || t=='\n') {
			continue;
*/
		} else if (t==RPAR) {
			cp->plev--;
			if (cp->plev==0) {
				*op++ = '\0';
				expand(cp->argp, ap-cp->argp-1);
				op = *cp->argp;
				ap = cp->argp-1;
				cp--;
				if (cp < callst)
					cp = NULL;
			} else
				*op++ = t;
		} else if (t==COMMA && cp->plev<=1) {
			*op++ = '\0';
			*ap++ = op;
			while ((t=getchr())==' ' || t=='\t' || t=='\n')
				;	/* skip leading white space during arg collection */
			putbak(t);
		} else
			*op++ = t;
	}
	if (cp!=NULL) {
		fprintf(stderr, "m4: unexpected EOF\n");
		delexit();
	}
	okret = 1;
	delexit();
}

catchsig()
{
	okret = 0;
	delexit();
}

delexit()
{
	register FILE *fp;
	register i, c;

	if (!okret) {
		signal(SIGHUP, SIG_IGN);
		signal(SIGINT, SIG_IGN);
	}
	for (i=1; i<10; i++) {
		if (olist[i]==NULL)
			continue;
		fclose(olist[i]);
		tempname[7] = 'a'+i;
		if (okret) {
			fp = fopen(tempname, READ);
			while ((c = getc(fp)) > 0)
				putchar(c);
			fclose(fp);
		}
		unlink(tempname);
	}
	tempname[7] = 'a';
	unlink(tempname);
	exit(1-okret);
}

puttok()
{
	register char *tp;

	tp = token;
	if (cp) {
		if (op >= &obuf[SAVS]) {
			fprintf(stderr, "m4: argument overflow\n");
			delexit();
		}
		while (*tp)
			*op++ = *tp++;
	} else if (curfile)
		while (*tp)
			putc(*tp++, curfile);
}

pbstr(str)
register char *str;
{
	register char *p;

	p = str;
	while (*p++);
	--p;
	if (ip >= &ibuf[SAVS]) {
		fprintf(stderr, "m4: pushback overflow\n");
		delexit();
	}
	while (p > str)
		putbak(*--p);
}

expand(a1, c)
register char **a1;
{
	register char *dp;
	register n;

	dp = a1[-1];
	if (dp==defloc)
		dodef(a1, c);
	else if (dp==evaloc)
		doeval(a1, c);
	else if (dp==inclloc)
		doincl(a1, c, 1);
	else if (dp==sinclloc)
		doincl(a1, c, 0);
	else if (dp==makeloc)
		domake(a1, c);
	else if (dp==syscmdloc)
		dosyscmd(a1, c);
	else if (dp==incrloc)
		doincr(a1, c);
	else if (dp==substrloc)
		dosubstr(a1, c);
	else if (dp==indexloc)
		doindex(a1, c);
	else if (dp==transloc)
		dotransl(a1, c);
	else if (dp==ifloc)
		doif(a1, c);
	else if (dp==divloc)
		dodiv(a1, c);
	else if (dp==divnumloc)
		dodivnum(a1, c);
	else if (dp==undivloc)
		doundiv(a1, c);
	else if (dp==dnlloc)
		dodnl(a1, c);
	else if (dp==dumploc)
		dodump(a1, c);
	else if (dp==errploc)
		doerrp(a1, c);
	else if (dp==lenloc)
		dolen(a1, c);
	else if (dp==ifdefloc)
		doifdef(a1, c);
	else if (dp==undefloc)
		doundef(a1, c);
	else if (dp==shiftloc)
		doshift(a1, c);
	else if (dp==cqloc)
		docq(a1, c);
	else {
		while (*dp++);
		for (dp--; dp>a1[-1]; ) {
			if (--dp>a1[-1] && dp[-1]=='$') {
				n = *dp-'0';
				if (n>=0 && n<=9) {
					if (n <= c)
						pbstr(a1[n]);
					dp--;
				} else
					putbak(*dp);
			} else
				putbak(*dp);
		}
	}
}

struct nlist *lookup(str)
char *str;
{
	register char *s1, *s2;
	register struct nlist *np;
	static struct nlist nodef;

	s1 = str;
	for (hshval = 0; *s1; )
		hshval += *s1++;
	hshval %= HSHSIZ;
	for (np = hshtab[hshval]; np!=NULL; np = np->next) {
		s1 = str;
		s2 = np->name;
		while (*s1++ == *s2)
			if (*s2++ == EOS)
				return(np);
	}
	return(&nodef);
}

char *install(nam, val, flag)
char *nam, *val;
char flag;
{
	register struct nlist *np;

	if ((np = lookup(nam))->name == NULL) {
		np = (struct nlist *)malloc(sizeof(*np));
		if (np == NULL) {
			fprintf(stderr, "m4: no space for alloc\n");
			exit(1);
		}
		np->name = copy(nam);
		np->def = copy(val);
		np->next = hshtab[hshval];
		np->flag = flag;
		hshtab[hshval] = np;
		return(np->def);
	}
	free(np->def);
	np->flag = flag;
	np->def = copy(val);
	return(np->def);
}

doundef(ap, c)
char **ap;
{
	register struct nlist *np, *tnp;

	if (c < 1 || (np = lookup(ap[1]))->name == NULL)
		return;
	tnp = hshtab[hshval];	/* lookup sets hshval */
	if (tnp == np)	/* it's in first place */
		hshtab[hshval] = np->next;
	else {
		for ( ; tnp->next != np; tnp = tnp->next)
			;
		tnp->next = np->next;
	}
	/*
	 * If this is a reserved word, it has been removed from the
	 * hastable.  We do not want to actually free the space because
	 * of the code in expand.  Expand wants to to pointer compairs
	 * to tell if this is a reserved word (e.g a special action
	 * needs to take place).  Thus if we do not free the space,
	 * expand will still work, but the name will never be found
	 * because it out of the symbol table!
	 */
	if (np->flag&RESERVED == 0) { /* If not reserved free it */
		free(np->name);
		free(np->def);
		free((char *)np);
	}
}

char *copy(s)
register char *s;
{
	register char *p, *s1;

	p = s1 = malloc((unsigned)strlen(s)+1);
	if (p == NULL) {
		fprintf(stderr, "m4: no space for alloc\n");
		exit(1);
	}
	while (*s1++ = *s++);
	return(p);
}

dodef(ap, c)
char **ap;
{
	if (c >= 2) {
		if (strcmp(ap[1], ap[2]) == 0) {
			fprintf(stderr, "m4: %s defined as itself\n", ap[1]);
			delexit();
		}
		install(ap[1], ap[2], 0);
	}
	else if (c == 1)
		install(ap[1], "", 0);
}

doifdef(ap, c)
char **ap;
{
	register struct nlist *np;

	if (c < 2)
		return;
	if (lookup(ap[1])->name != NULL)
		pbstr(ap[2]);
	else if (c >= 3)
		pbstr(ap[3]);
}

dolen(ap, c)
char **ap;
{
	putnum((long) strlen(ap[1]));
}

docq(ap, c)
char **ap;
{
	if (c > 1) {
		lquote = *ap[1];
		rquote = *ap[2];
	} else if (c == 1) {
		lquote = rquote = *ap[1];
	} else {
#ifndef M4
		lquote = GRAVE;
		rquote = ACUTE;
#endif
#ifdef M4
		lquote = LBRAK;
		rquote = RBRAK;
#endif
	}
}

doshift(ap, c)
char **ap;
{
	fprintf(stderr, "m4: shift not yet implemented\n");
}

dodump(ap, c)
char **ap;
{
	int i;
	register struct nlist *np;

	if (c > 0)
		while (c--) {
			if ((np = lookup(*++ap))->name != NULL)
				fprintf(stderr, "`%s'	`%s'\n", np->name, np->def);
		}
	else
		for (i=0; i<HSHSIZ; i++)
			for (np=hshtab[i]; np!=NULL; np=np->next)
				fprintf(stderr, "`%s'	`%s'\n", np->name, np->def);
}

doerrp(ap, c)
char **ap;
{
	if (c > 0) {
		fprintf(stderr, ap[1], ap[2], ap[3], ap[4], ap[5], ap[6]);
		fprintf(stderr, "\n");
	}
}


long	evalval;	/* return value from yacc stuff */
char	*pe;	/* used by grammar */

doeval(ap, c)
char **ap;
{

	if (c > 0) {
		pe = ap[1];
		if (yyparse() == 0)
			putnum(evalval);
		else
			fprintf(stderr, "m4: invalid expression in eval: %s\n", ap[1]);
	}
}

doincl(ap, c, noisy)
char **ap;
{
	if (c > 0 && strlen(ap[1]) > 0) {
		infptr++;
		ip_stk[infptr] = cur_ip = ip;
		if ((infile[infptr] = fopen(ap[1], READ))==ERROR) {
			if (noisy) {
				fprintf(stderr, "m4: file not found: %s\n", ap[1]);
				delexit();
			}
			else
				infptr--;
		}
	}
}

dosyscmd(ap, c)
char **ap;
{
	if (c > 0)
		system(ap[1]);
}

domake(ap, c)
char **ap;
{
	if (c > 0)
		pbstr(mktemp(ap[1]));
}

doincr(ap, c)
char **ap;
{
	if (c >= 1)
		putnum(ctol(ap[1])+1);
}

putnum(num)
long num;
{
	register sign;

	sign = (num < 0) ? '-' : '\0';
	if (num < 0)
		num = -num;
	do {
		putbak(num%10+'0');
		num = num/10;
	} while (num!=0);
	if (sign == '-')
		putbak('-');
}

dosubstr(ap, c)
char **ap;
{
	int nc;
	register char *sp, *fc;

	if (c<2)
		return;
	if (c<3)
		nc = TOKS;
	else
		nc = ctoi(ap[3]);
	fc = ap[1] + max(0, min(ctoi(ap[2]), strlen(ap[1])));
	sp = fc + min(nc, strlen(fc));
	while (sp > fc)
		putbak(*--sp);
}

doindex(ap, c)
char **ap;
{
	if (c >= 2)
		putnum((long) strindex(ap[1], ap[2]));
}

strindex(p1, p2)
char *p1, *p2;
{
	register m;
	register char *s, *t, *p;

	for (p=p1; *p; p++) {
		s = p;
		m = 1;
		for (t=p2; *t; )
			if (*t++ != *s++)
				m = 0;
		if (m == 1)
			return(p-p1);
	}
	return(-1);
}

dotransl(ap, c)
char **ap;
{
	register char *s, *fr, *to;

	if (c <= 1) return;

	if (c == 2) {
		register int i;
		to = ap[1];
		for (s = ap[1]; *s; s++) {
			i = 0;
			for (fr = ap[2]; *fr; fr++)
				if (*s == *fr) {
					i++;
					break;
				}
			if (i == 0)
				*to++ = *s;
		}
		*to = '\0';
	}

	if (c >= 3) {
		for (s = ap[1]; *s; s++)
			for (fr = ap[2], to = ap[3]; *fr && *to; fr++, to++)
				if (*s == *fr)
					*s = *to;
	}

	pbstr(ap[1]);
}

doif(ap, c)
register char **ap;
{
	if (c < 3)
		return;
	while (c >= 3) {
		if (strcmp(ap[1], ap[2]) == 0) {
			pbstr(ap[3]);
			return;
		}
		c -= 3;
		ap += 3;
	}
	if (c > 0)
		pbstr(ap[1]);
}

dodiv(ap, c)
register char **ap;
{
	register int f;

	if (c<1)
		f = 0;
	else
		f = ctoi(ap[1]);
	if (f>=10 || f<0) {
		curfile = NULL;
		return;
	}
	tempname[7] = 'a' + f;
	if (olist[f] || (olist[f]=fopen(tempname, WRITE))) {
		curout = f;
		curfile = olist[f];
	}
}

doundiv(ap, c)
char **ap;
{
	register FILE *fp;
	register int i, ch;
	int j;

	if (c == 0) {
		for (i=1; i<10; i++) {
			if (i==curout || olist[i]==NULL)
				continue;
			fclose(olist[i]);
			tempname[7] = 'a'+i;
			fp = fopen(tempname, READ);
			if (curfile != NULL)
				while ((ch = getc(fp)) > 0)
					putc(ch, curfile);
			fclose(fp);
			unlink(tempname);
			olist[i] = NULL;
		}

	}
	else {
		for (j = 1; j <= c; j++) {
			i = ctoi(*++ap);
			if (i<1 || i>9 || i==curout || olist[i]==NULL)
				continue;
			fclose(olist[i]);
			tempname[7] = 'a'+i;
			fp = fopen(tempname, READ);
			if (curfile != NULL)
				while ((ch = getc(fp)) > 0)
					putc(ch, curfile);
			fclose(fp);
			unlink(tempname);
			olist[i] = NULL;
		}
	}
}

dodivnum(ap, c)
char **ap;
{
	putnum((long) curout);
}

dodnl(ap, c)
char **ap;
{
	register t;

	while ((t=getchr())!='\n' && t>=0)
		;
}

long ctol(str)
register char *str;
{
	register sign;
	long num;

	while (*str==' ' || *str=='\t' || *str=='\n')
		str++;
	num = 0;
	if (*str == '-') {
		sign = -1;
		str++;
	}
	else
		sign = 1;
	while (*str>='0' && *str<='9')
		num = num*10 + *str++ - '0';
	return(sign * num);
}

ctoi(s)
char *s;
{
	return(ctol(s));
}

min(a, b)
{
	if (a>b)
		return(b);
	return(a);
}

max(a, b)
{
	if (a>b)
		return(a);
	return(b);
}
