/* Copyright (c) 1979 Regents of the University of California */
#include "sh.h"

/*
 * C shell
 */

savehist(sp)
	struct wordent *sp;
{
	register struct Hist *hp, *np;
	int histlen;
	register char *cp;

	cp = value("history");
	if (*cp == 0)
		histlen = 0;
	else {
		while (*cp && digit(*cp))
			cp++;
		/* avoid a looping snafu */
		if (*cp)
			set("history", "10");
		histlen = getn(value("history"));
	}
	/* throw away null lines */
	if (sp->next->word[0] == '\n')
		return;
	for (hp = &Histlist; np = hp->Hnext;)
		if (eventno - np->Href >= histlen || histlen == 0)
			hp->Hnext = np->Hnext, hfree(np);
		else
			hp = np;
	enthist(++eventno, sp, 1);
}

struct Hist *
enthist(event, lp, docopy)
	int event;
	register struct wordent *lp;
	bool docopy;
{
	register struct Hist *np;

	np = (struct Hist *) calloc(1, sizeof *np);
	np->Hnum = np->Href = event;
	if (docopy)
		copylex(&np->Hlex, lp);
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

hfree(hp)
	register struct Hist *hp;
{

	freelex(&hp->Hlex);
	xfree(hp);
}

dohist()
{

	if (getn(value("history")) == 0)
		return;
	dohist1(Histlist.Hnext);
}

dohist1(hp)
	register struct Hist *hp;
{

	if (hp == 0)
		return;
	hp->Href++;
	dohist1(hp->Hnext);
	phist(hp);
}

phist(hp)
	register struct Hist *hp;
{

	printf("%6d\t", hp->Hnum);
	prlex(&hp->Hlex);
}
