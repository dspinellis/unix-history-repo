/*-
 * Copyright (c) 1980, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)hist.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include "sh.h"

static void hfree();
static void dohist1();
static void phist();

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
    for (hp = &Histlist; np = hp->Hnext;)
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
    (void) time(&(np->Htime));
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
dohist(vp)
    Char  **vp;
{
    int     n, rflg = 0, hflg = 0, tflg = 0;

    if (getn(value(STRhistory)) == 0)
	return;
    if (setintr)
	(void) sigsetmask(sigblock((sigmask_t) 0) & ~sigmask(SIGINT));
    while (*++vp && **vp == '-') {
	Char   *vp2 = *vp;

	while (*++vp2)
	    switch (*vp2) {
	    case 'h':
		hflg++;
		break;
	    case 'r':
		rflg++;
		break;
	    case 't':
		tflg++;
		break;
	    case '-':		/* ignore multiple '-'s */
		break;
	    default:
		stderror(ERR_HISTUS);
		break;
	    }
    }
    if (*vp)
	n = getn(*vp);
    else {
	n = getn(value(STRhistory));
    }
    dohist1(Histlist.Hnext, &n, rflg, hflg, tflg);
}

static void
dohist1(hp, np, rflg, hflg, tflg)
    struct Hist *hp;
    int    *np, rflg, hflg, tflg;
{
    bool    print = (*np) > 0;

    for (; hp != 0; hp = hp->Hnext) {
	(*np)--;
	hp->Href++;
	if (rflg == 0) {
	    dohist1(hp->Hnext, np, rflg, hflg, tflg);
	    if (print)
		phist(hp, hflg, tflg);
	    return;
	}
	if (*np >= 0)
	    phist(hp, hflg, tflg);
    }
}

static void
phist(hp, hflg, tflg)
    register struct Hist *hp;
    int     hflg, tflg;
{
    struct tm *t;
    char    ampm = 'a';

    if (hflg == 0) {
	xprintf("%6d\t", hp->Hnum);
	if (tflg == 0) {
	    t = localtime(&hp->Htime);
	    if (adrof(STRampm)) {	/* addition by Hans J. Albertsson */
		if (t->tm_hour >= 12) {
		    if (t->tm_hour > 12)
			t->tm_hour -= 12;
		    ampm = 'p';
		}
		else if (t->tm_hour == 0)
		    t->tm_hour = 12;
		xprintf("%2d:%02d%cm\t", t->tm_hour, t->tm_min, ampm);
	    }
	    else {
		xprintf("%2d:%02d\t", t->tm_hour, t->tm_min);
	    }
	}
    }
    prlex(&hp->Hlex);
}
