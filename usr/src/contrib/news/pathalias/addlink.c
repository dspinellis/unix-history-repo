/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)addlink.c	9.7 88/06/10";
#endif /* lint */

#include "def.h"

/* exports */
extern link *addlink();
extern void deadlink(), atrace(), freelink();
extern int tracelink(), maptrace();
char *Netchars = "!:@%";	/* sparse, but sufficient */
long Lcount;			/* how many edges? */

/* imports */
extern int Tflag, Dflag;
extern link *newlink();
extern node *addnode();
extern void yyerror(), die();
extern int strcmp(), strlen();

/* privates */
STATIC void netbits(), ltrace(), ltrprint();
static link	*Trace[NTRACE];
static int	Tracecount;

#define EQ(n1, n2)	(strcmp((n1)->n_name, (n2)->n_name) == 0)
#define LTRACE		if (Tflag) ltrace

link *
addlink(from, to, cost, netchar, netdir)
	node *from;
	register node *to;
	Cost cost;
	char netchar, netdir;
{	register link *l, *prev = 0;

	LTRACE(from, to, cost, netchar, netdir, "");
	/*
	 * maintain uniqueness for dead links (only).
	 */
	for (l = from->n_link; l; l = l->l_next) {
		if (!DEADLINK(l))
			break;
		if (to == l->l_to) {
			/* what the hell, use cheaper dead cost */
			if (cost < l->l_cost) {
				l->l_cost = cost;
				netbits(l, netchar, netdir);
			}
			return l;
		}
		prev = l;
	}
	

	/* allocate and link in the new link struct */
	l = newlink();
	if (cost != INF)	/* ignore back links */
		Lcount++;
	if (prev) {
		l->l_next = prev->l_next;
		prev->l_next = l;
	} else {
		l->l_next = from->n_link;
		from->n_link = l;
	}

	l->l_to = to;
	/* add penalty */
	if ((l->l_cost = cost + from->n_cost) < 0) {
		char buf[100];

		l->l_flag |= LDEAD;
		sprintf(buf, "link to %s ignored with negative cost", to->n_name);
		yyerror(buf);
	}
	if (netchar == 0) {
		netchar = DEFNET;
		netdir = DEFDIR;
	}
	netbits(l, netchar, netdir);
	if (Dflag && ISADOMAIN(from))
		l->l_flag |= LTERMINAL;

	return l;
}

void
deadlink(nleft, nright) 
	node *nleft, *nright;
{	link *l, *lhold = 0, *lprev, *lnext;

	/* DEAD host */
	if (nright == 0) {
		nleft->n_flag |= NDEAD;		/* DEAD host */
		return;
	}

	/* DEAD link */

	/* grab <nleft, nright> instances at head of nleft adjacency list */
	while ((l = nleft->n_link) != 0 && l->l_to == nright) {
		nleft->n_link = l->l_next;	/* disconnect */
		l->l_next = lhold;		/* terminate */
		lhold = l;			/* add to lhold */
	}

	/* move remaining <nleft, nright> instances */
	for (lprev = nleft->n_link; lprev && lprev->l_next; lprev = lprev->l_next) {
		if (lprev->l_next->l_to == nright) {
			l = lprev->l_next;
			lprev->l_next = l->l_next;	/* disconnect */
			l->l_next = lhold;		/* terminate */
			lhold = l;
		}
	}

	/* check for emptiness */
	if (lhold == 0) {
		addlink(nleft, nright, INF / 2, DEFNET, DEFDIR)->l_flag |= LDEAD;
		return;
	}

	/* reinsert deleted edges as DEAD links */
	for (l = lhold; l; l = lnext) {
		lnext = l->l_next;
		addlink(nleft, nright, l->l_cost, NETCHAR(l), NETDIR(l))->l_flag |= LDEAD;
		freelink(l);
	}
}

STATIC void
netbits(l, netchar, netdir)
	register link *l;
	char netchar, netdir;
{
	l->l_flag &= ~LDIR;
	l->l_flag |= netdir;
	l->l_netop = netchar;
}

int
tracelink(arg) 
	char *arg;
{	char *bang;
	link *l;

	if (Tracecount >= NTRACE)
		return -1;
	l = newlink();
	bang = index(arg, '!');
	if (bang) {
		*bang = 0;
		l->l_to = addnode(bang+1);
	} else 
		l->l_to = 0;

	l->l_from = addnode(arg);
	Trace[Tracecount++] = l;
	return 0;
}

/*
 * the obvious choice for testing equality is to compare struct
 * addresses, but that misses private nodes, so we use strcmp().
 */

STATIC void
ltrace(from, to, cost, netchar, netdir, message)
	node *from, *to;
	Cost cost;
	char netchar, netdir, *message;
{	link *l;
	int i;

	for (i = 0; i < Tracecount; i++) {
		l = Trace[i];
		/* overkill, but you asked for it! */
		if (l->l_to == 0) {
			if (EQ(from, l->l_from) || EQ(to, l->l_from))
				break;
		} else if (EQ(from, l->l_from) && EQ(to, l->l_to))
			break;
		else if (EQ(from, l->l_to) && EQ(to, l->l_from))
			break;	/* potential dead backlink */
	}
	if (i < Tracecount)
		ltrprint(from, to, cost, netchar, netdir, message);
}

/* print a trace item */
STATIC void
ltrprint(from, to, cost, netchar, netdir, message)
	node *from, *to;
	Cost cost;
	char netchar, netdir, *message;
{	char buf[256], *bptr = buf;

	strcpy(bptr, from->n_name);
	bptr += strlen(bptr);
	*bptr++ = ' ';
	if (netdir == LRIGHT)			/* @% */
		*bptr++ = netchar;
	strcpy(bptr, to->n_name);
	bptr += strlen(bptr);
	if (netdir == LLEFT)			/* !: */
		*bptr++ = netchar;
	sprintf(bptr, "(%ld) %s", cost, message);
	yyerror(buf);
}

void
atrace(n1, n2)
	node *n1, *n2;
{	link *l;
	int i;
	char buf[256];

	for (i = 0; i < Tracecount; i++) {
		l = Trace[i];
		if (l->l_to == 0 && ((node *) l->l_from == n1 || (node *) l->l_from == n2)) {
			sprintf(buf, "%s = %s", n1->n_name, n2->n_name);
			yyerror(buf);
			return;
		}
	}
}

int
maptrace(from, to)
	register node *from, *to;
{	register link *l;
	register int i;

	for (i = 0; i < Tracecount; i++) {
		l = Trace[i];
		if (l->l_to == 0) {
			if (EQ(from, l->l_from) || EQ(to, l->l_from))
				return 1;
		} else if (EQ(from, l->l_from) && EQ(to, l->l_to))
				return 1;
	}
	return 0;
}

void
deletelink(from, to)
	node *from;
	node *to;
{	register link *l, *lnext;

	l = from->n_link;

	/* delete all neighbors of from */
	if (to == 0) {
		while (l) {
			LTRACE(from, l->l_to, l->l_cost, NETCHAR(l), NETDIR(l), "DELETED");
			lnext = l->l_next;
			freelink(l);
			l = lnext;
		}
		from->n_link = 0;
		return;
	}

	/* delete from head of list */
	while (l && EQ(to, l->l_to)) {
		LTRACE(from, to, l->l_cost, NETCHAR(l), NETDIR(l), "DELETED");
		lnext = l->l_next;
		freelink(l);
		l = from->n_link = lnext;
	}

	/* delete from interior of list */
	if (l == 0)
		return;
	for (lnext = l->l_next; lnext; lnext = l->l_next) {
		if (EQ(to, lnext->l_to)) {
			LTRACE(from, to, l->l_cost, NETCHAR(l), NETDIR(l), "DELETED");
			l->l_next = lnext->l_next;
			freelink(lnext);
			/* continue processing this link */
		} else
			l = lnext;	/* next link */
	}
}
