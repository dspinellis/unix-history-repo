/* Copyright (c) 1979 Regents of the University of California */
#include "sh.h"

/*
 * C Shell
 */

static struct tbuffer {
#ifdef V6
	int 	put, pst;
#else
	time_t	put, pst;
#endif
	time_t	cut, cst;
} times0;
time_t	time0;

long
secs(bef, aft)
	struct tbuffer *bef, *aft;
{

	return ((aft->cut - bef->cut + aft->cst - bef->cst));
}

settimes()
{

	time(&time0);
	times(&times0);
}

dotime(v, kp)
	register char **v;
	struct command *kp;
{
	struct tbuffer timeszer, timesdol, *tzp;
	time_t timezer, timedol;

	if (v[1] != 0) {
		time(&timezer), times(&timeszer);
		lshift(v, 1);
		if (func(kp) == 0) {
			timflg = 1;
			return (0);
		}
		tzp = &timeszer;
	} else
		timezer = time0, tzp = &times0;
	time(&timedol);
	times(&timesdol);
	ptimes(timedol - timezer, tzp, &timesdol);
	return (1);
}

donice(v)
	register char **v;
{
	register char *cp;

	v++, cp = *v++;
	if (cp == 0) {
#ifndef V6
		nice(20);
		nice(-10);
#endif
		nice(4);
		return (1);
	}
	if (*v == 0 && any(cp[0], "+-")) {
#ifndef V6
		nice(20);
		nice(-10);
#endif
		nice(getn(cp));
		return (1);
	}
	return (0);
}

struct	tbuffer bef, aft;
time_t	btim, atim;

pwait(i)
	register int i;
{
	register int p, e;
	char *name;
	int s;

	if (i == 0)
		return;
	time(&btim);
	do {
		times(&bef);
		p = wait(&s);
		if (p == -1)
			return;
		times(&aft);
		if (p == getn(value("child")))
			unsetv("child");
		time(&atim);
		e = s & TRIM;
		if (e > 0 && (e > 15 || mesg[e])) {
			if (p != i)
				printf("%d: ", p);
			if (name = cname(p))
				printf("%s: ", name);
			if (e <= 15)
				printf(mesg[e]);
			else
				printf("Sig %d", e);
			if (s & 0200)
				printf(" -- Core dumped");
			printf("\n");
		}
		if (e != 0 && i == p) {
			cdone(p);
			if (e == SIGINT && setintr && (!gointr || !eq(gointr, "-")))
				pintr();
			error(0);
		}
		if (i == p) {
			set("status", putn(e ? e | QUOTE : (s >> 8) & 0377));
			if (exiterr && !eq(value("status"), "0")) {
				if (e == 0) {
					if (name = cname(p))
						printf("%s: ", name);
					printf("Exit status %s\n", value("status"));
				}
				exitstat();
			}
			cdone(p);
			break;
		}
		cdone(p);
	} while (i != p);
	if (timflg || (!child && adrof("time") && secs(&bef, &aft) / 60 >= getn(value("time")))) {
		timflg = 0;
		ptimes(atim - btim, &bef, &aft);
	}
}

ptimes(sec, bef, aft)
	time_t sec;
	register struct tbuffer *bef, *aft;
{

	p60ths(aft->cut - bef->cut);
	printf("u ");
	p60ths(aft->cst - bef->cst);
	printf("s ");
	psecs(sec);
	printf(" %d%%\n", (int) ((100 * secs(bef, aft)) / (60 * (sec ? sec : 1))));
}

endwait()
{

	signal(SIGINT, SIG_IGN);
	cleft();
	bferr("Interrupted");
}

await()
{
	if (setintr)
		signal(SIGINT, endwait);
	pwait(-1);
	if (setintr)
		signal(SIGINT, SIG_IGN);
}

struct	achild {
	int	pid;
	char	*cname;
	struct	achild *cnext;
} children;

char *
cname(pid)
	int pid;
{
	register struct achild *cp;

	for (cp = children.cnext; cp; cp = cp->cnext)
		if (cp->pid == pid)
			return (cp->cname);
	return (NOSTR);
}

cadd(pid, cname)
	int pid;
	char *cname;
{
	register struct achild *cp = (struct achild *) calloc(1, sizeof (struct achild));

	cp->pid = pid;
	cp->cname = savestr(cname);
	cp->cnext = children.cnext;
	children.cnext = cp;
}

cdone(pid)
	int pid;
{
	register struct achild *cpp, *cp;

	cpp = &children;
	for (cp = cpp->cnext; cp; cp = cp->cnext) {
		if (cp->pid == pid) {
			xfree(cp->cname);
			cpp->cnext = cp->cnext;
			xfree(cp);
			return;
		}
		cpp = cp;
	}
}

cleft()
{

	register struct achild *cp;

	for (cp = children.cnext; cp; cp = cp->cnext)
		printf("%6d  %s\n", cp->pid, cp->cname);
}
