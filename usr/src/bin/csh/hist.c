static	char *sccsid = "@(#)hist.c 4.3 %G%";

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
	xfree((char *)hp);
}

dohist(vp)
	char **vp;
{
	int n, rflg = 0, hflg = 0;
	if (getn(value("history")) == 0)
		return;
	if (setintr)
		sigrelse(SIGINT);
	vp++;
	while (*vp[0] == '-') {
		if (*vp && eq(*vp, "-h")) {
			hflg++;
			vp++;
		}
		if (*vp && eq(*vp, "-r")) {
			rflg++;
			vp++;
		}
	}
	if (*vp)
		n = getn(*vp);
	else {
		n = getn(value("history"));
	}
	dohist1(Histlist.Hnext, &n, rflg, hflg);
}

dohist1(hp, np, rflg, hflg)
	struct Hist *hp;
	int *np, rflg, hflg;
{
	bool print = (*np) > 0;
top:
	if (hp == 0)
		return;
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
	hp = hp->Hnext;
	goto top;
}

phist(hp, hflg)
	register struct Hist *hp;
	int hflg;
{

	if (hflg == 0)
		printf("%6d\t", hp->Hnum);
	prlex(&hp->Hlex);
}
