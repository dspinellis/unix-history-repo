/*-
 * Copyright (c) 1992 Henry Spencer.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Henry Spencer of the University of Toronto.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)engine.c	5.1 (Berkeley) %G%
 */

/*
 * The matching engine and friends.  This file is #included by regexec.c
 * after suitable #defines of a variety of macros used herein, so that
 * different state representations can be used without duplicating masses
 * of code.
 */

#ifdef SNAMES
#define	matcher	smatcher
#define	fast	sfast
#define	slow	sslow
#define	dissect	sdissect
#define	backref	sbackref
#define	expand	sexpand
#define	step	sstep
#define	print	sprint
#define	match	smat
#endif
#ifdef LNAMES
#define	matcher	lmatcher
#define	fast	lfast
#define	slow	lslow
#define	dissect	ldissect
#define	backref	lbackref
#define	expand	lexpand
#define	step	lstep
#define	print	lprint
#define	match	lmat
#endif

/* another structure passed up and down to avoid zillions of parameters */
struct match {
	struct re_guts *g;
	int eflags;
	regmatch_t *pmatch;	/* [nsub+1] (0 element unused) */
	uchar *offp;		/* offsets work from here */
	uchar *beginp;		/* start of string -- virtual NUL precedes */
	uchar *endp;		/* end of string -- virtual NUL here */
	uchar *coldp;		/* can be no match starting before here */
	uchar **lastpos;	/* [nplus+1] */
	STATEVARS;
	states st;		/* current states */
	states fresh;		/* states for a fresh start */
	states tmp;		/* temporary */
	states empty;		/* empty set of states */
};

#ifndef NDEBUG
STATIC void print();
extern char *regchar();
#define	SP(t, s, c)	{ if (m->eflags&REG_TRACE) print(m->g, t, s, c, stdout); }
#define	DO(t, p1, p2, s1, s2)	{ if (m->eflags&REG_TRACE) { \
					printf("%s %s-", t, regchar(*(p1))); \
					printf("%s ", regchar(*(p2))); \
					printf("%ld-%ld\n", s1, s2); } }
#else
#define	SP(t, s, c)	/* nothing */
#define	DO(t, p1, p2, s1, s2)	/* nothing */
#endif

STATIC uchar *fast();
STATIC uchar *slow();
STATIC uchar *dissect();
STATIC uchar *backref();
STATIC states expand();
STATIC states step();

/*
 - matcher - the actual matching engine
 */
static int			/* 0 success, REG_NOMATCH failure */
matcher(g, string, nmatch, pmatch, eflags)
register struct re_guts *g;
uchar *string;
size_t nmatch;
regmatch_t pmatch[];
int eflags;
{
	register uchar *endp;
	register int i;
	struct match mv;
	register struct match *m = &mv;
	register uchar *dp;
	const register sopno gf = g->firststate+1;	/* +1 for OEND */
	const register sopno gl = g->laststate;
	uchar *start;
	uchar *stop;

	if (g->cflags&REG_NOSUB)
		nmatch = 0;		/* simplify tests */
	if (eflags&REG_STARTEND) {
		start = string + pmatch[0].rm_so;
		stop = string + pmatch[0].rm_eo;
	} else {
		start = string;
		stop = start + strlen((char *)start);
	}

	/* match struct setup */
	m->g = g;
	m->eflags = eflags;
	m->pmatch = NULL;
	m->lastpos = NULL;
	m->offp = string;
	m->beginp = start;
	m->endp = stop;
	STATESETUP(m, 4);
	SETUP(m->st);
	SETUP(m->fresh);
	SETUP(m->tmp);
	SETUP(m->empty);
	CLEAR(m->empty);

	/* this loop does only one repetition except for backrefs */
	for (;;) {
		endp = fast(m, start, stop, gf, gl);
		if (endp == NULL) {		/* a miss */
			STATETEARDOWN(m);
			return(REG_NOMATCH);
		}
		if (nmatch == 0 && !g->backrefs)
			break;		/* no further info needed */

		/* where? */
		assert(m->coldp != NULL);
		for (;;) {
			endp = slow(m, m->coldp, stop, gf, gl);
			if (endp != NULL)
				break;
			assert(*m->coldp != '\0');
			m->coldp++;
		}
		if (nmatch == 1 && !g->backrefs)
			break;		/* no further info needed */

		/* oh my, he wants the subexpressions... */
		if (m->pmatch == NULL)
			m->pmatch = (regmatch_t *)malloc((m->g->nsub + 1) *
							sizeof(regmatch_t));
		if (m->pmatch == NULL) {
			STATETEARDOWN(m);
			return(REG_ESPACE);
		}
		for (i = 1; i <= m->g->nsub; i++) {
			m->pmatch[i].rm_so = -1;
			m->pmatch[i].rm_eo = -1;
		}
		if (!g->backrefs)
			dp = dissect(m, m->coldp, endp, gf, gl);
		else {
			if (g->nplus > 0 && m->lastpos == NULL)
				m->lastpos = (uchar **)malloc((g->nplus+1) *
							sizeof(uchar *));
			if (g->nplus > 0 && m->lastpos == NULL) {
				free(m->pmatch);
				STATETEARDOWN(m);
				return(REG_ESPACE);
			}
			dp = backref(m, m->coldp, endp, gf, gl, (sopno)0);
		}
		if (dp != NULL)
			break;

		/* uh-oh... we couldn't find a subexpression-level match */
		assert(g->backrefs);	/* must be back references doing it */
		assert(g->nplus == 0 || m->lastpos != NULL);
		while (dp == NULL && endp > m->coldp &&
			(endp = slow(m, m->coldp, endp-1, gf, gl)) != NULL) {
			/* try it on a shorter possibility */
			for (i = 1; i <= m->g->nsub; i++) {
				m->pmatch[i].rm_so = -1;
				m->pmatch[i].rm_eo = -1;
			}
			dp = backref(m, m->coldp, endp, gf, gl, (sopno)0);
		}
		assert(dp == NULL || dp == endp);
		if (dp != NULL)		/* found a shorter one */
			break;

		/* despite initial appearances, there is no match here */
		start = m->coldp + 1;	/* recycle starting later */
		assert(start <= stop);
	}

	/* fill in the details if requested */
	if (nmatch > 0) {
		pmatch[0].rm_so = m->coldp - m->offp;
		pmatch[0].rm_eo = endp - m->offp;
	}
	if (nmatch > 1) {
		assert(m->pmatch != NULL);
		for (i = 1; i < nmatch; i++)
			if (i <= m->g->nsub)
				pmatch[i] = m->pmatch[i];
			else {
				pmatch[i].rm_so = -1;
				pmatch[i].rm_eo = -1;
			}
	}

	if (m->pmatch != NULL)
		free((char *)m->pmatch);
	if (m->lastpos != NULL)
		free((char *)m->lastpos);
	STATETEARDOWN(m);
	return(0);
}

/*
 - dissect - figure out what matched what, no back references
 */
static uchar *			/* == stop (success) always */
dissect(m, start, stop, startst, stopst)
register struct match *m;
uchar *start;
uchar *stop;
sopno startst;
sopno stopst;
{
	register int i;
	register sopno ss;	/* start sop of current subRE */
	register sopno es;	/* end sop of current subRE */
	register uchar *sp;	/* start of string matched by it */
	register uchar *stp;	/* string matched by it cannot pass here */
	register uchar *rest;	/* start of rest of string */
	register uchar *tail;	/* string unmatched by rest of RE */
	register sopno ssub;	/* start sop of subsubRE */
	register sopno esub;	/* end sop of subsubRE */
	register uchar *ssp;	/* start of string matched by subsubRE */
	register uchar *sep;	/* end of string matched by subsubRE */
	register uchar *oldssp;	/* previous ssp */
	register uchar *dp;
	register size_t len;

	DO("diss", start, stop, startst, stopst);
	sp = start;
	for (ss = startst; ss < stopst; ss = es) {
		/* identify end of subRE */
		es = ss;
		switch (OP(m->g->strip[es])) {
		case OPLUS_:
		case OQUEST_:
			es += OPND(m->g->strip[es]);
			break;
		case OCH_:
			while (OP(m->g->strip[es]) != O_CH)
				es += OPND(m->g->strip[es]);
			break;
		}
		es++;

		/* figure out what it matched */
		switch (OP(m->g->strip[ss])) {
		case OEND:
			assert(0);
			break;
		case OCHAR:
			sp++;
			break;
		case OBOL:
		case OEOL:
			break;
		case OANY:
		case OANYOF:
			sp++;
			break;
		case OBACK_:
		case O_BACK:
			assert(0);
			break;
		/* cases where length of match is hard to find */
		case OQUEST_:
			stp = stop;
			for (;;) {
				/* how long could this one be? */
				rest = slow(m, sp, stp, ss, es);
				assert(rest != NULL);	/* it did match */
				/* could the rest match the rest? */
				tail = slow(m, rest, stop, es, stopst);
				if (tail == stop)
					break;		/* yes! */
				/* no -- try a shorter match for this one */
				stp = rest - 1;
				assert(stp >= sp);	/* it did work */
			}
			ssub = ss + 1;
			esub = es - 1;
			/* did innards match? */
			if (slow(m, sp, rest, ssub, esub) != NULL) {
				dp = dissect(m, sp, rest, ssub, esub);
				assert(dp == rest);
			} else		/* no */
				assert(sp == rest);
			sp = rest;
			break;
		case OPLUS_:
			stp = stop;
			for (;;) {
				/* how long could this one be? */
				rest = slow(m, sp, stp, ss, es);
				assert(rest != NULL);	/* it did match */
				/* could the rest match the rest? */
				tail = slow(m, rest, stop, es, stopst);
				if (tail == stop)
					break;		/* yes! */
				/* no -- try a shorter match for this one */
				stp = rest - 1;
				assert(stp >= sp);	/* it did work */
			}
			ssub = ss + 1;
			esub = es - 1;
			ssp = sp;
			oldssp = ssp;
			for (;;) {	/* find last match of innards */
				sep = slow(m, ssp, rest, ssub, esub);
				if (sep == NULL || sep == ssp)
					break;	/* failed or matched null */
				oldssp = ssp;	/* on to next try */
				ssp = sep;
			}
			if (sep == NULL) {
				/* last successful match */
				sep = ssp;
				ssp = oldssp;
			}
			assert(sep == rest);	/* must exhaust substring */
			assert(slow(m, ssp, sep, ssub, esub) == rest);
			dp = dissect(m, ssp, sep, ssub, esub);
			assert(dp == sep);
			sp = rest;
			break;
		case OCH_:
			stp = stop;
			for (;;) {
				/* how long could this one be? */
				rest = slow(m, sp, stp, ss, es);
				assert(rest != NULL);	/* it did match */
				/* could the rest match the rest? */
				tail = slow(m, rest, stop, es, stopst);
				if (tail == stop)
					break;		/* yes! */
				/* no -- try a shorter match for this one */
				stp = rest - 1;
				assert(stp >= sp);	/* it did work */
			}
			ssub = ss + 1;
			esub = ss + OPND(m->g->strip[ss]) - 1;
			assert(OP(m->g->strip[esub]) == OOR1);
			for (;;) {	/* find first matching branch */
				if (slow(m, sp, rest, ssub, esub) == rest)
					break;	/* it matched all of it */
				/* that one missed, try next one */
				assert(OP(m->g->strip[esub]) == OOR1);
				esub++;
				assert(OP(m->g->strip[esub]) == OOR2);
				ssub = esub + 1;
				esub += OPND(m->g->strip[esub]);
				if (OP(m->g->strip[esub]) == OOR2)
					esub--;
				else
					assert(OP(m->g->strip[esub]) == O_CH);
			}
			dp = dissect(m, sp, rest, ssub, esub);
			assert(dp == rest);
			sp = rest;
			break;
		case O_PLUS:
		case O_QUEST:
		case OOR1:
		case OOR2:
		case O_CH:
			assert(0);
			break;
		case OLPAREN:
			i = OPND(m->g->strip[ss]);
			m->pmatch[i].rm_so = sp - m->offp;
			break;
		case ORPAREN:
			i = OPND(m->g->strip[ss]);
			m->pmatch[i].rm_eo = sp - m->offp;
			break;
		default:		/* uh oh */
			assert(0);
			break;
		}
	}

	assert(sp == stop);
	return(sp);
}

/*
 - backref - figure out what matched what, figuring in back references
 */
static uchar *			/* == stop (success) or NULL (failure) */
backref(m, start, stop, startst, stopst, lev)
register struct match *m;
uchar *start;
uchar *stop;
sopno startst;
sopno stopst;
sopno lev;			/* PLUS nesting level */
{
	register int i;
	register sopno ss;	/* start sop of current subRE */
	register uchar *sp;	/* start of string matched by it */
	register sopno ssub;	/* start sop of subsubRE */
	register sopno esub;	/* end sop of subsubRE */
	register uchar *ssp;	/* start of string matched by subsubRE */
	register uchar *dp;
	register size_t len;
	register int hard;
	register sop s;
	register regoff_t offsave;
	register cset *cs;

	DO("back", start, stop, startst, stopst);
	sp = start;

	/* get as far as we can with easy stuff */
	hard = 0;
	for (ss = startst; !hard && ss < stopst; ss++)
		switch (OP(s = m->g->strip[ss])) {
		case OCHAR:
			if (sp == stop || *sp++ != OPND(s))
				return(NULL);
			break;
		case OANY:
			if (sp == stop)
				return(NULL);
			sp++;
			break;
		case OANYOF:
			cs = &m->g->sets[OPND(s)];
			if (sp == stop || !CHIN(cs, *sp++))
				return(NULL);
			break;
		case OBOL:
			if ( (sp == m->beginp && !(m->eflags&REG_NOTBOL)) ||
					(sp < m->endp && *(sp-1) == '\n' &&
						(m->g->cflags&REG_NEWLINE)) )
				{ /* yes */ }
			else
				return(NULL);
			break;
		case OEOL:
			if ( (sp == m->endp && !(m->eflags&REG_NOTEOL)) ||
					(sp < m->endp && *sp == '\n' &&
						(m->g->cflags&REG_NEWLINE)) )
				{ /* yes */ }
			else
				return(NULL);
			break;
		case O_QUEST:
			break;
		case OOR1:	/* matches null but needs to skip */
			ss++;
			s = m->g->strip[ss];
			do {
				assert(OP(s) == OOR2);
				ss += OPND(s);
			} while (OP(s = m->g->strip[ss]) != O_CH);
			/* note that the ss++ gets us past the O_CH */
			break;
		default:	/* have to make a choice */
			hard = 1;
			break;
		}
	if (!hard) {		/* that was it! */
		if (sp != stop)
			return(NULL);
		return(sp);
	}
	ss--;			/* adjust for the for's final increment */

	/* the hard stuff */
	DO("hard", sp, stop, ss, stopst);
	s = m->g->strip[ss];
	switch (OP(s)) {
	case OBACK_:		/* the vilest depths */
		i = OPND(s);
		if (m->pmatch[i].rm_eo == -1)
			return(NULL);
		assert(m->pmatch[i].rm_so != -1);
		len = m->pmatch[i].rm_eo - m->pmatch[i].rm_so;
		assert(stop - m->beginp >= len);
		if (sp > stop - len)
			return(NULL);	/* not enough left to match */
		ssp = m->offp + m->pmatch[i].rm_so;
		if (strncmp((char *)sp, (char *)ssp, len) != 0)
			return(NULL);
		while (m->g->strip[ss] != SOP(O_BACK, i))
			ss++;
		return(backref(m, sp+len, stop, ss+1, stopst, lev));
		break;
	case OQUEST_:		/* to null or not */
		dp = backref(m, sp, stop, ss+1, stopst, lev);
		if (dp != NULL)
			return(dp);	/* not */
		return(backref(m, sp, stop, ss+OPND(s)+1, stopst, lev));
		break;
	case OPLUS_:
		assert(m->lastpos != NULL);
		assert(lev+1 <= m->g->nplus);
		m->lastpos[lev+1] = sp;
		return(backref(m, sp, stop, ss+1, stopst, lev+1));
		break;
	case O_PLUS:
		if (sp == m->lastpos[lev])	/* last pass matched null */
			return(backref(m, sp, stop, ss+1, stopst, lev-1));
		/* try another pass */
		m->lastpos[lev] = sp;
		dp = backref(m, sp, stop, ss-OPND(s)+1, stopst, lev);
		if (dp == NULL)
			return(backref(m, sp, stop, ss+1, stopst, lev-1));
		else
			return(dp);
		break;
	case OCH_:		/* find the right one, if any */
		ssub = ss + 1;
		esub = ss + OPND(s) - 1;
		assert(OP(m->g->strip[esub]) == OOR1);
		for (;;) {	/* find first matching branch */
			dp = backref(m, sp, stop, ssub, esub, lev);
			if (dp != NULL)
				return(dp);
			/* that one missed, try next one */
			if (OP(m->g->strip[esub]) == O_CH)
				return(NULL);	/* there is none */
			esub++;
			assert(OP(m->g->strip[esub]) == OOR2);
			ssub = esub + 1;
			esub += OPND(m->g->strip[esub]);
			if (OP(m->g->strip[esub]) == OOR2)
				esub--;
			else
				assert(OP(m->g->strip[esub]) == O_CH);
		}
		break;
	case OLPAREN:		/* must undo assignment if rest fails */
		i = OPND(s);
		offsave = m->pmatch[i].rm_so;
		m->pmatch[i].rm_so = sp - m->offp;
		dp = backref(m, sp, stop, ss+1, stopst, lev);
		if (dp != NULL)
			return(dp);
		m->pmatch[i].rm_so = offsave;
		return(NULL);
		break;
	case ORPAREN:		/* must undo assignment if rest fails */
		i = OPND(s);
		offsave = m->pmatch[i].rm_eo;
		m->pmatch[i].rm_eo = sp - m->offp;
		dp = backref(m, sp, stop, ss+1, stopst, lev);
		if (dp != NULL)
			return(dp);
		m->pmatch[i].rm_eo = offsave;
		return(NULL);
		break;
	default:		/* uh oh */
		assert(0);
		break;
	}

	/* "can't happen" */
	assert(0);
	/* NOTREACHED */
}

/*
 - fast - step through the string at top speed
 */
static uchar *			/* where tentative match ended, or NULL */
fast(m, start, stop, startst, stopst)
register struct match *m;
uchar *start;
uchar *stop;
sopno startst;
sopno stopst;
{
	register states st = m->st;
	register states fresh = m->fresh;
	register states tmp = m->tmp;
	register uchar *p = start;
	register uchar c;
	register uchar lastc;	/* previous c */
	register int atbol;
	register int ateol;
	register uchar *coldp;	/* last p after which no match was underway */

	CLEAR(st);
	SET1(st, startst);
	st = expand(m->g, startst, stopst, st, 0, 0);
	ASSIGN(fresh, st);
	SP("start", st, *p);
	c = '\0';
	coldp = NULL;
	for (;;) {
		/* next character */
		lastc = c;
		c = (p == m->endp) ? '\0' : *p;
		if (EQ(st, fresh))
			coldp = p;

		/* is there an EOL and/or BOL between lastc and c? */
		atbol = ( (lastc == '\n' && m->g->cflags&REG_NEWLINE) ||
				(lastc == '\0' && !(m->eflags&REG_NOTBOL)) );
		ateol = ( (c == '\n' && m->g->cflags&REG_NEWLINE) ||
				(c == '\0' && !(m->eflags&REG_NOTEOL)) );
		if (atbol || ateol) {
			st = expand(m->g, startst, stopst, st, atbol, ateol);
			SP("bef", st, c);
		}

		/* are we done? */
		if (ISSET(st, stopst) || p == stop)
			break;		/* NOTE BREAK OUT */

		/* no, we must deal with this character */
		ASSIGN(tmp, st);
		ASSIGN(st, fresh);
		st = step(m->g, startst, stopst, tmp, c, st);
		SP("aft", st, c);
		assert(EQ(expand(m->g, startst, stopst, st, 0, 0), st));
		p++;
	}

	assert(coldp != NULL);
	m->coldp = coldp;
	if (ISSET(st, stopst))
		return(p+1);
	else
		return(NULL);
}

/*
 - slow - step through the string more deliberately
 */
static uchar *			/* where it ended */
slow(m, start, stop, startst, stopst)
register struct match *m;
uchar *start;
uchar *stop;
sopno startst;
sopno stopst;
{
	register states st = m->st;
	register states empty = m->empty;
	register states tmp = m->tmp;
	register uchar *p = start;
	register uchar c = (start == m->beginp) ? '\0' : *(start-1);
	register uchar lastc;	/* previous c */
	register int atbol;
	register int ateol;
	register uchar *matchp;	/* last p at which a match ended */

	DO("slow", start, stop, startst, stopst);
	CLEAR(st);
	SET1(st, startst);
	SP("sstart", st, *p);
	matchp = NULL;
	for (;;) {
		/* next character */
		lastc = c;
		c = (p == m->endp) ? '\0' : *p;

		/* is there an EOL and/or BOL between lastc and c? */
		atbol = ( (lastc == '\n' && m->g->cflags&REG_NEWLINE) ||
				(lastc == '\0' && !(m->eflags&REG_NOTBOL)) );
		ateol = ( (c == '\n' && m->g->cflags&REG_NEWLINE) ||
				(c == '\0' && !(m->eflags&REG_NOTEOL)) );

		/* do we need an expansion before looking at the char? */
		if (p == start || atbol || ateol) {
			st = expand(m->g, startst, stopst, st, atbol, ateol);
			SP("sbef", st, c);
		}

		/* are we done? */
		if (ISSET(st, stopst))
			matchp = p;
		if (EQ(st, empty) || p == stop)
			break;		/* NOTE BREAK OUT */

		/* no, we must deal with this character */
		ASSIGN(tmp, st);
		ASSIGN(st, empty);
		st = step(m->g, startst, stopst, tmp, c, st);
		SP("saft", st, c);
		assert(EQ(expand(m->g, startst, stopst, st, 0, 0), st));
		p++;
	}

	return(matchp);
}


/*
 - expand - return set of states reachable from an initial set
 */
static states
expand(g, start, stop, st, atbol, ateol)
register struct re_guts *g;
int start;			/* start state within strip */
int stop;			/* state after stop state within strip */
register states st;
int atbol;			/* at start or just after \n? (for BOL) */
int ateol;			/* just before \n or \0? (for EOL) */
{
	register sop s;
	register sopno pc;
	register onestate here;		/* note, macros know this name */
	register sopno look;
	register int i;

	for (pc = start, INIT(here, pc); pc != stop; pc++, INC(here)) {
		s = g->strip[pc];
		switch (OP(s)) {
		case OEND:
			assert(pc == stop-1);
			break;
		case OCHAR:		/* can't get past this */
			break;
		case OBOL:
			if (atbol)
				FWD(st, st, 1);
			break;
		case OEOL:
			if (ateol)
				FWD(st, st, 1);
			break;
		case OANY:		/* can't get past this either */
			break;
		case OANYOF:		/* or this */
			break;
		case OBACK_:		/* not significant here */
		case O_BACK:
			FWD(st, st, 1);
			break;
		case OPLUS_:		/* forward, this is just an empty */
			FWD(st, st, 1);
			break;
		case O_PLUS:		/* both forward and back */
			FWD(st, st, 1);
			i = ISSETBACK(st, OPND(s));
			BACK(st, st, OPND(s));
			if (!i && ISSETBACK(st, OPND(s))) {
				/* oho, must reconsider loop body */
				pc -= OPND(s) + 1;
				INIT(here, pc);
			}
			break;
		case OQUEST_:		/* two branches, both forward */
			FWD(st, st, 1);
			FWD(st, st, OPND(s));
			break;
		case O_QUEST:		/* just an empty */
			FWD(st, st, 1);
			break;
		case OLPAREN:		/* not significant here */
		case ORPAREN:
			FWD(st, st, 1);
			break;
		case OCH_:		/* mark the first two branches */
			FWD(st, st, 1);
			assert(OP(g->strip[pc+OPND(s)]) == OOR2);
			FWD(st, st, OPND(s));
			break;
		case OOR1:		/* done a branch, find the O_CH */
			if (ISSTATEIN(st, here)) {
				for (look = 1;
						OP(s = g->strip[pc+look]) != O_CH;
						look += OPND(s))
					assert(OP(s) == OOR2);
				FWD(st, st, look);
			}
			break;
		case OOR2:		/* propagate OCH_'s marking onward */
			FWD(st, st, 1);
			if (OP(g->strip[pc+OPND(s)]) != O_CH) {
				assert(OP(g->strip[pc+OPND(s)]) == OOR2);
				FWD(st, st, OPND(s));
			}
			break;
		case O_CH:		/* just an empty */
			FWD(st, st, 1);
			break;
		default:		/* ooooops... */
			assert(0);
			break;
		}
	}

	return(st);
}

/*
 - step - map set of states reachable before char to set reachable after
 */
static states
step(g, start, stop, bef, ch, aft)
register struct re_guts *g;
int start;			/* start state within strip */
int stop;			/* state after stop state within strip */
register states bef;		/* states reachable before */
uchar ch;
register states aft;		/* states already known reachable after */
{
	register cset *cs;
	register sop s;
	register sopno pc;
	register onestate here;		/* note, macros know this name */
	register sopno look;
	register int i;

	for (pc = start, INIT(here, pc); pc != stop; pc++, INC(here)) {
		s = g->strip[pc];
		switch (OP(s)) {
		case OEND:
			assert(pc == stop-1);
			break;
		case OCHAR:
			if (ch == OPND(s))
				FWD(aft, bef, 1);
			break;
		case OBOL:	/* these are looked after by expand() */
		case OEOL:
			break;
		case OANY:
			FWD(aft, bef, 1);
			break;
		case OANYOF:
			cs = &g->sets[OPND(s)];
			if (CHIN(cs, ch))
				FWD(aft, bef, 1);
			break;
		case OBACK_:		/* ignored here */
		case O_BACK:
			FWD(aft, aft, 1);
			break;
		case OPLUS_:		/* forward, this is just an empty */
			FWD(aft, aft, 1);
			break;
		case O_PLUS:		/* both forward and back */
			FWD(aft, aft, 1);
			i = ISSETBACK(aft, OPND(s));
			BACK(aft, aft, OPND(s));
			if (!i && ISSETBACK(aft, OPND(s))) {
				/* oho, must reconsider loop body */
				pc -= OPND(s) + 1;
				INIT(here, pc);
			}
			break;
		case OQUEST_:		/* two branches, both forward */
			FWD(aft, aft, 1);
			FWD(aft, aft, OPND(s));
			break;
		case O_QUEST:		/* just an empty */
			FWD(aft, aft, 1);
			break;
		case OLPAREN:		/* not significant here */
		case ORPAREN:
			FWD(aft, aft, 1);
			break;
		case OCH_:		/* mark the first two branches */
			FWD(aft, aft, 1);
			assert(OP(g->strip[pc+OPND(s)]) == OOR2);
			FWD(aft, aft, OPND(s));
			break;
		case OOR1:		/* done a branch, find the O_CH */
			if (ISSTATEIN(aft, here)) {
				for (look = 1;
						OP(s = g->strip[pc+look]) != O_CH;
						look += OPND(s))
					assert(OP(s) == OOR2);
				FWD(aft, aft, look);
			}
			break;
		case OOR2:		/* propagate OCH_'s marking */
			FWD(aft, aft, 1);
			if (OP(g->strip[pc+OPND(s)]) != O_CH) {
				assert(OP(g->strip[pc+OPND(s)]) == OOR2);
				FWD(aft, aft, OPND(s));
			}
			break;
		case O_CH:		/* just empty */
			FWD(aft, aft, 1);
			break;
		default:		/* ooooops... */
			assert(0);
			break;
		}
	}

	return(aft);
}

#ifndef NDEBUG
/*
 - print - print a set of states
 */
static void
print(g, caption, st, ch, d)
struct re_guts *g;
char *caption;
states st;
uchar ch;
FILE *d;
{
	register int i;
	register int first = 1;

	fprintf(d, "%s", caption);
	if (ch != '\0')
		fprintf(d, " %s", regchar(ch));
	for (i = 0; i < g->nstates; i++)
		if (ISSET(st, i)) {
			fprintf(d, "%s%d", (first) ? "\t" : ", ", i);
			first = 0;
		}
	fprintf(d, "\n");
}
#endif

#undef	matcher
#undef	fast
#undef	slow
#undef	dissect
#undef	backref
#undef	expand
#undef	step
#undef	print
#undef	match
