#include "sh.h"

static	char *mesg[] {
	0,
	"Hangup",
	0,
	"Quit",
	"Illegal instruction",
	"Trace/BPT trap",
	"IOT trap",
	"EMT trap",
	"Floating exception",
	"Killed",
	"Bus error",
	"Segmentation violation",
	"Bad system call",
	0,
	"Alarm clock",
	"Terminate",
	"Sig 16",
};

static struct tbuffer {
	int 	put, pst;
	long	cut, cst;
} times0;
static long time0;
static char timflg;

double
secs(bef, aft)
	struct tbuffer *bef, *aft;
{

	return ((aft->cut - bef->cut + aft->cst - bef->cst) / 60.0);
}

settimes()
{

	time(&time0);
	times(&times0);
}

dotimes(v0)
	char *v0[];
{
	register char **v;
	struct tbuffer timesdol;
	long timedol;

	v = v0;
	if (v[1] != 0) {
		timflg = 1;
		do {
			v[0] = v[1];
			v++;
		} while (*v != 0);
		return (func(v0 - DCOM));
	}
	time(&timedol);
	times(&timesdol);
	ptimes(timedol - time0, &times0, &timesdol);
	return (1);
}

pwait(i)
	register int i;
{
	register p, e;
	int s;
	struct tbuffer bef, aft;
	long btim, atim;

	if (i == 0)
		return;
	time(&btim);
	do {
		times(&bef);
		p = wait(&s);
		if (p == getn(value(pcs)))
			unsetv(pcs);
		time(&atim);
		if (p == -1)
			return;
		e = s & 0177;
		if (mesg[e] != 0) {
			if (p != i) {
				prn(p);
				prs(": ");
			}
			prs(mesg[e]);
			if (s & 0200)
				prs(" -- Core dumped");
		}
		if (e != 0)
			err("");
	} while (i != p);
	times(&aft);
	if (timflg || (adrof(tim) && secs(&bef, &aft) >= getn(value(tim)))) {
		timflg = 0;
		ptimes(atim - btim, &bef, &aft);
	}
}

ptimes(sec, bef, aft)
	long sec;
	struct tbuffer *bef, *aft;
{
	register int i;

	p60ths(aft->cut - bef->cut);
	prs("u ");
	p60ths(aft->cst - bef->cst);
	prs("s ");
	psecs(sec);
	prs(" ");
	i = 100.0 * secs(bef, aft) / (sec ? sec : 1);
	prn(i);
	prs("%\n");
}

p60ths(l)
	long l;
{
	register int i;

	l =+ 3;
	i = l / 60;
	prn(i);
	prs(".");
	i = l % 60;
	prn(i / 6);
}

psecs(l)
	long l;
{
	register int i;

	i = l / 3600.0;
	if (i) {
		prn(i);
		prs(":");
		i = l % 3600;
		p2dig(i / 60);
		goto minsec;
	}
	i = l;
	prn(i / 60);
minsec:
	i =% 60;
	prs(":");
	p2dig(i);
}

p2dig(i)
	int i;
{
	prn(i / 10);
	prn(i % 10);
}

endwait()
{
	signal(INTR, 1);
	prs("wait: Interrupted\n");
	reset();		/* URK! */
}

await(v)
	register char *v[];
{
	register int i;

	v++;
	if (*v) {
		i = getn(*v);
		if (i == 0)
			return;
	} else
		i = -1;
	if (setintr && *value(prompt)) {
		signal(INTR, endwait);
		pwait(i);
		signal(INTR, 1);
	} else
		pwait(i);
}
