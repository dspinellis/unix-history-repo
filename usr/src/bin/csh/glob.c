/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)glob.c	5.14 (Berkeley) %G%";
#endif /* not lint */

#include "sh.h"
#include "glob.h"

static int noglob, nonomatch;

static int pargsiz, gargsiz;
/*
 * Values for gflag
 */
#define G_NONE	0	/* No globbing needed			*/
#define G_GLOB	1	/* string contains *?[] characters	*/
#define G_CSH	2	/* string contains ~`{ characters	*/

#define LBRC '{'
#define RBRC '}'
#define LBRK '['
#define RBRK ']'

#define EOS '\0'
char **gargv = (char **) 0;
short gargc = 0;

/*
 * globbing is now done in two stages. In the first pass we expand
 * csh globbing idioms ~`{ and then we proceed doing the normal 
 * globbing if needed ?*[
 *
 * Csh type globbing is handled in globexpand() and the rest is
 * handled in glob() which is part of the 4.4BSD libc.
 * 
 */


static char *
globtilde(nv, s)
char **nv, *s;
{
	char gbuf[MAXPATHLEN], *gstart, *b, *u;

	gstart = gbuf;
	*gstart++ = *s++;
	for (u = s, b = gstart; *s != '\0' && *s != '/'; *b++ = *s++)
		 continue;
	*b = EOS;
	if (*s == EOS || *s == '/') {
		if (s == u) 
			gstart = strcpy(gbuf, value("home"));
		else if (gethdir(gstart)) {
			blkfree(nv);
			error("Unknown user: %s", gstart);
		}
		b = &gstart[strlen(gstart)];
	}
	while (*s) *b++ = *s++;
	*b = EOS;
	return(savestr(gstart));
}

static int
globbrace(s, p, bl)
char *s, *p, ***bl;
{
	int i, len;
	char *pm, *pe, *lm, *pl;
	char **nv, **vl;
	char gbuf[MAXPATHLEN];
	int size = GAVSIZ;

	nv = vl = (char **) xalloc(sizeof(char *) * size);

	len = 0;
	/* copy part up to the brace */
	for (lm = gbuf, p = s; *p != LBRC; *lm++ = *p++)
		continue;

	/* check for balanced braces */
	for (i = 0, pe = ++p; *pe; pe++) 
		if (*pe == LBRK) {
			/* Ignore everything between [] */
			for (++pe; *pe != RBRK && *pe != EOS; pe++)
				continue;
			if (*pe == EOS) {
				blkfree(nv);
				return(- LBRK);
			}
		}
		else if (*pe == LBRC)
			i++;
		else if (*pe == RBRC) {
			if (i == 0)
				break;
			i--;
		}

	if (i != 0) {
		blkfree(nv);
		return(- LBRC);
	}

	for (i = 0, pl = pm = p; pm <= pe; pm++)
	switch (*pm) {
	case LBRK:
		for (++pm; *pm != RBRK && *pm != EOS; pm++)
			continue;
		if (*pm == EOS) {
			blkfree(nv);
			return(- RBRK);
		}
		break;
	case LBRC:
		i++;
		break;
	case RBRC:
		if (i) {
			i--;
			break;
		}
		/*FALLTHROUGH*/
	case ',':
		if (i && *pm == ',')
			break;
		else {
			char savec = *pm;
			*pm = EOS;
			(void) strcpy(lm, pl);
			(void) strcat(gbuf, pe + 1);
			*pm = savec;
			*vl++ = savestr(gbuf);
			len++;
			pl = pm + 1;
			if (vl == &nv[size]) {
				size += GAVSIZ;
				nv = (char **) xrealloc(nv, 
							size * sizeof(char *));
				vl = &nv[size - GAVSIZ];
			}
		}
		break;
	}
	*vl = (char *) 0;
	*bl = nv;
	return(len);
}

static char **
globexpand(v)
	char **v;
{
	char *s;
	char **nv, **vl, **el;
	int size = GAVSIZ;


	nv = vl = (char **) xalloc(sizeof(char *) * size);
	*vl = (char *) 0;

	/*
	 * Step 1: expand backquotes.
	 */
	while (s = *v++) {
		if (index(s, '`')) {
			int i;

			dobackp(s, 0);
			for (i = 0; i < pargc; i++) {
				*vl++ = pargv[i];
				if (vl == &nv[size]) {
					size += GAVSIZ;
					nv = (char **) xrealloc(nv, 
							size * sizeof(char *));
					vl = &nv[size - GAVSIZ];
				}
			}
			xfree((char *) pargv);
			pargv = (char **) 0;
		}
		else {
			*vl++ = savestr(s);
			if (vl == &nv[size]) {
				size += GAVSIZ;
				nv = (char **) xrealloc(nv, 
							size * sizeof(char *));
				vl = &nv[size - GAVSIZ];
			}
		}
	}
	*vl = (char *) 0;

	if (noglob) 
		return(nv);

	/*
	 * Step 2: expand braces
	 */
	el = vl;
	vl = nv;
	for (s = *vl; s; s = *++vl) {
		char *b;
		char **vp, **bp;
		if (b = index(s, LBRC)) {
			char **bl;
			int i, len;
			if ((len = globbrace(s, b, &bl)) < 0) {
				blkfree(nv);
				error("Missing %c", -len);
			}
			xfree(s);
			if (len == 1) {
				*vl-- = *bl;
				xfree((char *) bl);
				continue;
			}
			len = blklen(bl);
			if (&el[len] >= &nv[size]) {
				int l, e;
				l = &el[len] - &nv[size];
				size += GAVSIZ > l ? GAVSIZ : l;
				l = vl - nv;
				e = el - nv;
				nv = (char **) xrealloc(nv, 
							size * sizeof(char *));
				vl = nv + l;
				el = nv + e;
			}
			vp = vl--;
			*vp = *bl;
			len--;
			for (bp = el; bp != vp; bp--) 
			     bp[len] = *bp;
			el += len;
			vp++;
			for (bp = bl + 1; *bp; *vp++ = *bp++)
				continue;
			xfree(bl);
		}
    
	}
	/*
	 * Step 3 expand tilde
	 */
	vl = nv;
	for (s = *vl; s; s = *++vl)
		if (*s == '~') {
			*vl = globtilde(nv, s);
			xfree(s);
		}
	return(nv);
}

char * 
globone(str)
	char *str;
{
	
	char *v[2];
	char *nstr;

	noglob = adrof("noglob") != 0;
	gflag = 0;
	v[0] = str;
	v[1] = 0;
	tglob(v);
	if (gflag == G_NONE)
		return (strip(savestr(str)));

	if (gflag & G_CSH) {
		char **vl;

		/*
		 * Expand back-quote, tilde and brace
		 */
		vl = globexpand(v);
		if (vl[1] != (char *) 0) {
			blkfree(vl);
			setname(str);
			bferr("Ambiguous");
			/*NOTREACHED*/
		}
		nstr = vl[0];
		xfree((char *) vl);
	}
	else	
		nstr = str;
	
	if (!noglob && (gflag & G_GLOB)) {
		glob_t globv;

		globv.gl_offs = 0;
		globv.gl_pathv = 0;
		nonomatch = adrof("nonomatch") != 0;
		glob(nstr, nonomatch ? GLOB_NOCHECK : 0, 0, &globv);
		if (gflag & G_CSH)
			xfree(nstr);
		switch (globv.gl_pathc) {
		case 0:
			setname(str);
			globfree(&globv);
			bferr("No match");
			/*NOTREACHED*/
		case 1:
			str = strip(savestr(globv.gl_pathv[0]));
			globfree(&globv);
			return(str);
		default:
			setname(str);
			globfree(&globv);
			bferr("Ambiguous");
			/*NOTREACHED*/
		}
	}
	return(nstr ? strip(nstr) : (char *) 0);
}

char **
globall(v)
	char **v;
{
	char *c, **vl, **vo;

	if (!v || !v[0]) {
		gargv = saveblk(v);
		gargc = blklen(gargv);
		return (gargv);
	}

	noglob = adrof("noglob") != 0;
	nonomatch = adrof("nonomatch") != 0;
	
	if (gflag & G_CSH) 
		/*
		 * Expand back-quote, tilde and brace
		 */
		vl = vo = globexpand(v);
	else	
		vl = vo = saveblk(v);
	    
	if (!noglob && (gflag & G_GLOB)) {
		/*
		 * Glob the strings in vl using the glob routine
		 * from libc
		 */
		int gappend = 0;
		glob_t globv;

		globv.gl_offs = 0;
		globv.gl_pathv = 0;
		gargc = 0;
		do {
			glob(*vl, gappend | GLOB_NOCHECK, 0, &globv);
			if (!nonomatch && (globv.gl_matchc == 0) && 
			    (globv.gl_flags & GLOB_MAGCHAR)) {
				if (gflag & G_CSH)
					blkfree(vo);
				globfree(&globv);
				gargc = 0;
				return(gargv = (char **) 0);
			}
			gappend = GLOB_APPEND;
		}
		while (*++vl);

		if (gflag & G_CSH)
			blkfree(vo);
		if (globv.gl_pathc)
			vl = saveblk(globv.gl_pathv);
		else
			vl = 0;
		globfree(&globv);
	}

	gargc = vl ? blklen(vl) : 0;
	return(gargv = vl);
}
		
ginit()
{
	gargsiz = GAVSIZ;
	gargv = (char **) xalloc(sizeof(char *) * gargsiz);
	gargv[0] = 0;
	gargc = 0;
}

rscan(t, f)
	register char **t;
	int (*f)();
{
	register char *p;

	while (p = *t++)
		while (*p)
			(*f)(*p++);
}

trim(t)
	register char **t;
{
	register char *p;

	while (p = *t++)
		while (*p)
			*p++ &= TRIM;
}

tglob(t)
	register char **t;
{
	register char *p, c;

	while (p = *t++) {
		if (*p == '~')
			gflag |= G_CSH;
		else if (*p == '{' &&
		    (p[1] == '\0' || p[1] == '}' && p[2] == '\0'))
			continue;
		while (c = *p++)
			if (isglob(c))
			    gflag |= (c == '{' || c == '`') ? G_CSH : G_GLOB;
	}
}

/*
 * Command substitute cp.  If literal, then this is a substitution from a
 * << redirection, and so we should not crunch blanks and tabs, separating
 * words only at newlines.
 */
char **
dobackp(cp, literal)
	char *cp;
	bool literal;
{
	register char *lp, *rp;
	char *ep, word[MAXPATHLEN];

	if (pargv) {
		abort();
		blkfree(pargv);
	}
	pargsiz = GAVSIZ;
	pargv = (char **) xalloc(sizeof(char *) * pargsiz);
	pargv[0] = NOSTR;
	pargcp = pargs = word;
	pargc = 0;
	pnleft = MAXPATHLEN - 4;
	for (;;) {
		for (lp = cp; *lp != '`'; lp++) {
			if (*lp == 0) {
				if (pargcp != pargs)
					pword();
				return (pargv);
			}
			psave(*lp);
		}
		lp++;
		for (rp = lp; *rp && *rp != '`'; rp++)
			if (*rp == '\\') {
				rp++;
				if (!*rp)
					goto oops;
			}
		if (!*rp)
oops:			error("Unmatched `");
		ep = savestr(lp);
		ep[rp - lp] = 0;
		backeval(ep, literal);
		cp = rp + 1;
	}
}

backeval(cp, literal)
	char *cp;
	bool literal;
{
	register int icnt, c;
	register char *ip;
	struct command faket;
	bool hadnl;
	int pvec[2], quoted;
	char *fakecom[2], ibuf[BUFSIZ];

	hadnl = 0;
	icnt = 0;
	quoted = (literal || (cp[0] & QUOTE)) ? QUOTE : 0;
	faket.t_dtyp = NODE_COMMAND;
	faket.t_dflg = 0;
	faket.t_dlef = 0;
	faket.t_drit = 0;
	faket.t_dspr = 0;
	faket.t_dcom = fakecom;
	fakecom[0] = "` ... `";
	fakecom[1] = 0;

	/*
	 * We do the psave job to temporarily change the current job so that
	 * the following fork is considered a separate job.  This is so that
	 * when backquotes are used in a builtin function that calls glob the
	 * "current job" is not corrupted.  We only need one level of pushed
	 * jobs as long as we are sure to fork here.
	 */
	psavejob();

	/*
	 * It would be nicer if we could integrate this redirection more with
	 * the routines in sh.sem.c by doing a fake execute on a builtin
	 * function that was piped out.
	 */
	mypipe(pvec);
	if (pfork(&faket, -1) == 0) {
		struct wordent paraml;
		struct command *t;

		(void) close(pvec[0]);
		(void) dmove(pvec[1], 1);
		(void) dmove(SHDIAG, 2);
		initdesc();
		arginp = cp;
		while (*cp)
			*cp++ &= TRIM;
		(void) lex(&paraml);
		if (err)
			error(err);
		alias(&paraml);
		t = syntax(paraml.next, &paraml, 0);
		if (err)
			error(err);
		if (t)
			t->t_dflg |= F_NOFORK;
		(void) signal(SIGTSTP, SIG_IGN);
		(void) signal(SIGTTIN, SIG_IGN);
		(void) signal(SIGTTOU, SIG_IGN);
		execute(t, -1);
		exitstat();
	}
	xfree(cp);
	(void) close(pvec[1]);
	do {
		int cnt = 0;
		for (;;) {
			if (icnt == 0) {
				ip = ibuf;
				icnt = read(pvec[0], ip, BUFSIZ);
				if (icnt <= 0) {
					c = -1;
					break;
				}
			}
			if (hadnl)
				break;
			--icnt;
			c = (*ip++ & TRIM);
			if (c == 0)
				break;
			if (c == '\n') {
				/*
				 * Continue around the loop one more time, so
				 * that we can eat the last newline without
				 * terminating this word.
				 */
				hadnl = 1;
				continue;
			}
			if (!quoted && (c == ' ' || c == '\t'))
				break;
			cnt++;
			psave(c | quoted);
		}
		/*
		 * Unless at end-of-file, we will form a new word here if there
		 * were characters in the word, or in any case when we take
		 * text literally.  If we didn't make empty words here when
		 * literal was set then we would lose blank lines.
		 */
		if (c != -1 && (cnt || literal))
			pword();
		hadnl = 0;
	} while (c >= 0);
	(void) close(pvec[0]);
	pwait();
	prestjob();
}

psave(c)
	char c;
{
	if (--pnleft <= 0)
		error("Word too long");
	*pargcp++ = c;
}

pword()
{
	psave(0);
	if (pargc == pargsiz - 1) {
	    pargsiz += GAVSIZ;
	    pargv = (char **) xrealloc(pargv, pargsiz * sizeof(char *));
	}
	pargv[pargc++] = savestr(pargs);
	pargv[pargc] = NOSTR;
	pargcp = pargs;
	pnleft = MAXPATHLEN - 4;
}

Gmatch(string, pattern)
	register char *string, *pattern;
{
	register char stringc, patternc;
	int match;
	char lastchar, rangec;

	for (;; ++string) {
		stringc = *string & TRIM;
		switch (patternc = *pattern++) {
		case 0:
			return (stringc == 0);
		case '?':
			if (stringc == 0)
				return (0);
			break;
		case '*':
			if (!*pattern)
				return (1);
			while (*string) 
				if (Gmatch(string++, pattern))
					return (1);
			return (0);
		case '[':
			lastchar = -1;
			match = 0;
			while (rangec = *pattern++) {
				if (rangec == ']')
					if (match)
						break;
					else
						return (0);
				if (match)
					continue;
				if (rangec == '-') 
					match = (stringc <= *pattern++ &&
					    *(pattern-2) <= stringc);
				else {
					lastchar = rangec;
					match = (stringc == rangec);
				}
			}
			if (rangec == 0)
				bferr("Missing ]");
			break;
		default:
			if ((patternc & TRIM) != stringc)
				return (0);
			break;

		}
	}
}

Gcat(s1, s2)
	char *s1, *s2;
{
	register char *p, *q;
	int n;

	for (p = s1; *p++;);
	for (q = s2; *q++;);
	n = (p - s1) + (q - s2) - 1;
	if (++gargc >= gargsiz) {
		gargsiz += GAVSIZ;
		gargv = (char **) xrealloc(gargv, gargsiz * sizeof(char *));
	}
	gargv[gargc] = 0;
	p = gargv[gargc - 1] = xalloc((unsigned)n);
	for (q = s1; *p++ = *q++;);
	for (p--, q = s2; *p++ = *q++;);
}

int
sortscmp(a1, a2)
	char **a1, **a2;
{
	return(strcmp(*a1, *a2));
}
