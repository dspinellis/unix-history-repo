/*-
 * Copyright (c) 1980, 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)hist.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdlib.h>
#if __STDC__
# include <stdarg.h>
#else
# include <varargs.h>
#endif

#include "csh.h"
#include "extern.h"

static void	hfree __P((struct Hist *));
static void	dohist1 __P((struct Hist *, int *, int, int));
static void	phist __P((struct Hist *, int));

void
savehist(sp)
    struct wordent *sp;
{
    register struct Hist *hp, *np;
    register int histlen = 0;
    Char   *cp;

    /* throw away null lines */
    if (sp->next->word[0] == '\n')
	return;
    cp = value(STRhistory);
    if (*cp) {
	register Char *p = cp;

	while (*p) {
	    if (!Isdigit(*p)) {
		histlen = 0;
		break;
	    }
	    histlen = histlen * 10 + *p++ - '0';
	}
    }
    for (hp = &Histlist; (np = hp->Hnext) != NULL;)
	if (eventno - np->Href >= histlen || histlen == 0)
	    hp->Hnext = np->Hnext, hfree(np);
	else
	    hp = np;
    (void) enthist(++eventno, sp, 1);
}

struct Hist *
enthist(event, lp, docopy)
    int     event;
    register struct wordent *lp;
    bool    docopy;
{
    register struct Hist *np;

    np = (struct Hist *) xmalloc((size_t) sizeof(*np));
    np->Hnum = np->Href = event;
    if (docopy) {
	copylex(&np->Hlex, lp);
    }
    else {
	np->Hlex.next = lp->next;
	lp->next->prev = &np->Hlex;
	np->Hlex.prev = lp->prev;
	lp->prev->next = &np->Hlex;
    }
    np->Hnext = Histlist.Hnext;
    Histlist.Hnext = np;
    return (np);
}

static void
hfree(hp)
    register struct Hist *hp;
{

    freelex(&hp->Hlex);
    xfree((ptr_t) hp);
}

void
/*ARGSUSED*/
dohist(v, t)
    Char **v;
    struct command *t;
{
    int     n, rflg = 0, hflg = 0;

    if (getn(value(STRhistory)) == 0)
	return;
    if (setintr)
	(void) sigsetmask(sigblock((sigset_t) 0) & ~sigmask(SIGINT));
    while (*++v && **v == '-') {
	Char   *vp = *v;

	while (*++vp)
	    switch (*vp) {
	    case 'h':
		hflg++;
		break;
	    case 'r':
		rflg++;
		break;
	    case '-':		/* ignore multiple '-'s */
		break;
	    default:
		stderror(ERR_HISTUS);
		break;
	    }
    }
    if (*v)
	n = getn(*v);
    else {
	n = getn(value(STRhistory));
    }
    dohist1(Histlist.Hnext, &n, rflg, hflg);
}

static void
dohist1(hp, np, rflg, hflg)
    struct Hist *hp;
    int    *np, rflg, hflg;
{
    bool    print = (*np) > 0;

    for (; hp != 0; hp = hp->Hnext) {
	(*np)--;
	hp->Href++;
	if (rflg == 0) {
	    dohist1(hp->Hnext, np, rflg, hflg);
	    if (print)
		phist(hp, hflg);
	    return;
	}
	if (*np >= 0)
	    phist(hp, hflg);
    }
}

static void
phist(hp, hflg)
    register struct Hist *hp;
    int     hflg;
{
    if (hflg == 0)
	(void) fprintf(cshout, "%6d\t", hp->Hnum);
    prlex(cshout, &hp->Hlex);
}
