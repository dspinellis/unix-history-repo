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
 *	@(#)regexec.c	5.1 (Berkeley) %G%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)regexec.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <limits.h>
#include <regex.h>

#include "utils.h"
#include "regex2.h"

/*
 - regexec - interface for matching
 */
int				/* 0 success, REG_NOMATCH failure */
regexec(preg, string, nmatch, pmatch, eflags)
const regex_t *preg;
const char *string;
size_t nmatch;
regmatch_t pmatch[];
int eflags;
{
	register struct re_guts *g = preg->re_g;
	STATIC int smatcher();
	STATIC int lmatcher();
#ifndef NDEBUG
#	define	GOODFLAGS(f)	(f)
#else
#	define	GOODFLAGS(f)	((f)&(REG_NOTBOL|REG_NOTEOL|REG_STARTEND))
#endif

	if (preg->re_magic != MAGIC1 || g->magic != MAGIC2)
		return(REG_BADPAT);
	assert(!(g->iflags&BAD));
	if (g->iflags&BAD)		/* backstop for NDEBUG case */
		return(REG_BADPAT);
	eflags = GOODFLAGS(eflags);	/* xxx should we complain? */

	if (g->nstates <= 32 && !(eflags&REG_LARGE))
		return(smatcher(g, string, nmatch, pmatch, eflags));
	else
		return(lmatcher(g, string, nmatch, pmatch, eflags));
}

/* macros for manipulating states, small version */
#define	states	long
#define	CLEAR(v)	((v) = 0)
#define	SET0(v, n)	((v) &= ~(1 << (n)))
#define	SET1(v, n)	((v) |= 1 << (n))
#define	ISSET(v, n)	((v) & (1 << (n)))
#define	ASSIGN(d, s)	((d) = (s))
#define	EQ(a, b)	((a) == (b))
#define	STATEVARS	int dummy	/* dummy version */
#define	STATESETUP(m, n)	/* nothing */
#define	STATETEARDOWN(m)	/* nothing */
#define	SETUP(v)	/* nothing */
#define	onestate	int
#define	INIT(o, n)	((o) = 1 << (n))
#define	INC(o)	((o) <<= 1)
#define	ISSTATEIN(v, o)	((v) & (o))
/* some abbreviations; note that some of these know variable names! */
/* do "if I'm here, I can also be there" etc without branches */
#define	FWD(dst, src, n)	((dst) |= ((src)&(here)) << (n))
#define	BACK(dst, src, n)	((dst) |= ((src)&(here)) >> (n))
#define	ISSETBACK(v, n)	((v) & (here >> (n)))
/* function names */
#define SNAMES			/* engine.c looks after details */

#include "engine.c"

/* now undo things */
#undef	states
#undef	CLEAR
#undef	SET0
#undef	SET1
#undef	ISSET
#undef	ASSIGN
#undef	EQ
#undef	STATEVARS
#undef	STATESETUP
#undef	STATETEARDOWN
#undef	SETUP
#undef	onestate
#undef	INIT
#undef	INC
#undef	ISSTATEIN
#undef	FWD
#undef	BACK
#undef	ISSETBACK
#undef	SNAMES

/* macros for manipulating states, large version */
#define	states	char *
#define	CLEAR(v)	memset(v, 0, m->g->nstates)
#define	SET0(v, n)	((v)[n] = 0)
#define	SET1(v, n)	((v)[n] = 1)
#define	ISSET(v, n)	((v)[n])
#define	ASSIGN(d, s)	memcpy(d, s, m->g->nstates)
#define	EQ(a, b)	(memcmp(a, b, m->g->nstates) == 0)
#define	STATEVARS	int vn; char *space
#define	STATESETUP(m, nv)	{ (m)->space = malloc((nv)*(m)->g->nstates); \
				if ((m)->space == NULL) return(REG_ESPACE); \
				(m)->vn = 0; }
#define	STATETEARDOWN(m)	{ free((m)->space); }
#define	SETUP(v)	((v) = &m->space[m->vn++ * m->g->nstates])
#define	onestate	int
#define	INIT(o, n)	((o) = (n))
#define	INC(o)	((o)++)
#define	ISSTATEIN(v, o)	((v)[o])
/* some abbreviations; note that some of these know variable names! */
/* do "if I'm here, I can also be there" etc without branches */
#define	FWD(dst, src, n)	((dst)[here+(n)] |= (src)[here])
#define	BACK(dst, src, n)	((dst)[here-(n)] |= (src)[here])
#define	ISSETBACK(v, n)	((v)[here - (n)])
/* function names */
#define	LNAMES			/* flag */

#include "engine.c"
