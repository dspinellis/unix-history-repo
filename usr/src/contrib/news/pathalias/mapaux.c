/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)mapaux.c	9.8 91/06/23";
#endif /* lint */

#include "def.h"

/* imports */
extern long Nheap, Hashpart, Tabsize, NumNcopy, Nlink, NumLcopy;
extern node **Table, *Home;
extern char *Graphout, *Linkout, *Netchars, **Argv;
extern int Vflag;
extern void freelink(), die();
extern long pack();
extern link *newlink();
extern node *newnode();
extern char *strsave();
extern int strcmp(), strlen();

/* exports */
extern long pack();
extern void resetnodes(), dumpgraph(), showlinks(), terminalnet();
extern int tiebreaker();
extern node *ncopy();

/* privates */
static FILE *Gstream;	/* for dumping graph */
STATIC void dumpnode(), untangle(), dfs();
STATIC int height();
STATIC link *lcopy();

/*
 * slide everything from Table[low] to Table[high]
 * up toward the high end.  make room!  make room!
 */
long
pack(low, high)
	long low, high;
{	long hole, next;

	/* find first "hole " */
	for (hole = high; hole >= low && Table[hole] != 0; --hole)
		;

	/* repeatedly move next filled entry into last hole */
	for (next = hole - 1; next >= low; --next) {
		if (Table[next] != 0) {
			Table[hole] = Table[next];
			Table[hole]->n_tloc = hole;
			Table[next] = 0;
			while (Table[--hole] != 0)	/* find next hole */
				;
		}
	}
	return hole + 1;
}

void
resetnodes()
{	register long i;
	register node *n;

	for (i = Hashpart; i < Tabsize; i++)
		if ((n = Table[i]) != 0) {
			n->n_cost = (Cost) 0;
			n->n_flag &= ~(NALIAS|ATSIGN|MAPPED|HASLEFT|HASRIGHT|NTERMINAL);
			n->n_copy = n;
		}
		
	Home->n_cost = (Cost) 0;
	Home->n_flag &= ~(NALIAS|ATSIGN|MAPPED|HASLEFT|HASRIGHT|NTERMINAL);
	Home->n_copy = Home;
}

void	
dumpgraph()
{	register long i;
	register node *n;

	if ((Gstream = fopen(Graphout, "w")) == NULL) {
		fprintf(stderr, "%s: ", Argv[0]);
		perror(Graphout);
		return;
	}

	untangle();	/* untangle net cycles for proper output */

	for (i = Hashpart; i < Tabsize; i++) {
		n = Table[i];
		if (n == 0)
			continue;	/* impossible, but ... */
		/* a minor optimization ... */
		if (n->n_link == 0)
			continue;
		/* pathparse doesn't need these */
		if (n->n_flag & NNET)
			continue;
		dumpnode(n);
	}
}

STATIC void
dumpnode(from)
	register node *from;
{	register node *to;
	register link *l;
	link *lnet = 0, *ll, *lnext;

	for (l = from->n_link ; l; l = l->l_next) {
		to = l->l_to;
		if (from == to)
			continue;	/* oops -- it's me! */

		if ((to->n_flag & NNET) == 0) {
			/* host -> host -- print host>host */
			if (l->l_cost == INF)
				continue;	/* phoney link */
			fputs(from->n_name, Gstream);
			putc('>', Gstream);
			fputs(to->n_name, Gstream);
			putc('\n', Gstream);
		} else {
			/*
			 * host -> net -- just cache it for now.
			 * first check for dups.  (quadratic, but
			 * n is small here.)
			 */
			while (to->n_root && to != to->n_root)
				to = to->n_root;
			for (ll = lnet; ll; ll = ll->l_next)
				if (strcmp(ll->l_to->n_name, to->n_name) == 0)
					break;
			if (ll)
				continue;	/* dup */
			ll = newlink();
			ll->l_next = lnet;
			ll->l_to = to;
			lnet = ll;
		}
	}

	/* dump nets */
	if (lnet) {
		/* nets -- print host@\tnet,net, ... */
		fputs(from->n_name, Gstream);
		putc('@', Gstream);
		putc('\t', Gstream);
		for (ll = lnet; ll; ll = lnext) {
			lnext = ll->l_next;
			fputs(ll->l_to->n_name, Gstream);
			if (lnext)
				fputc(',', Gstream);
			freelink(ll);
		}
		putc('\n', Gstream);
	}
}

/*
 * remove cycles in net definitions. 
 *
 * depth-first search
 *
 * for each net, run dfs on its neighbors (nets only).  if we return to
 * a visited node, that's a net cycle.  mark the cycle with a pointer
 * in the n_root field (which gets us closer to the root of this
 * portion of the dfs tree).
 */
STATIC void
untangle()
{	register long i;
	register node *n;

	for (i = Hashpart; i < Tabsize; i++) {
		n = Table[i];
		if (n == 0 || (n->n_flag & NNET) == 0 || n->n_root)
			continue;
		dfs(n);
	}
}

STATIC void
dfs(n)
	register node *n;
{	register link *l;
	register node *next;

	n->n_flag |= INDFS;
	n->n_root = n;
	for (l = n->n_link; l; l = l->l_next) {
		next = l->l_to;
		if ((next->n_flag & NNET) == 0)
			continue;
		if ((next->n_flag & INDFS) == 0) {
			dfs(next);
			if (next->n_root != next)
				n->n_root = next->n_root;
		} else
			n->n_root = next->n_root;
	}
	n->n_flag &= ~INDFS;
}

void
showlinks() 
{	register link *l;
	register node *n;
	register long i;
	FILE	*estream;

	if ((estream = fopen(Linkout, "w")) == 0)
		return;

	for (i = Hashpart; i < Tabsize; i++) {
		n = Table[i];
		if (n == 0 || n->n_link == 0)
			continue;
		for (l = n->n_link; l; l = l->l_next) {
			fputs(n->n_name, estream);
			putc('\t', estream);
			if (NETDIR(l) == LRIGHT)
				putc(NETCHAR(l), estream);
			fputs(l->l_to->n_name, estream);
			if (NETDIR(l) == LLEFT)
				putc(NETCHAR(l), estream);
			fprintf(estream, "(%d)\n", l->l_cost);
		}
	}
	(void) fclose(estream);
}

/*
 * n is node in heap, newp is candidate for new parent.
 * choose between newp and n->n_parent for parent.
 * return 0 to use newp, non-zero o.w.
 */
#define NEWP 0
#define OLDP 1
int
tiebreaker(n, newp)
	node *n;
	register node *newp;
{	register char *opname, *npname, *name;
	register node *oldp;
	int metric;

	oldp = n->n_parent;

	/* given the choice, avoid gatewayed nets */
	if (GATEWAYED(newp) && !GATEWAYED(oldp))
		return OLDP;
	if (!GATEWAYED(newp) && GATEWAYED(oldp))
		return NEWP;

	/* look at real parents, not nets */
	while ((oldp->n_flag & NNET) && oldp->n_parent)
		oldp = oldp->n_parent;
	while ((newp->n_flag & NNET) && newp->n_parent)
		newp = newp->n_parent;

	/* use fewer hops, if possible */
	metric = height(oldp) - height(newp);
	if (metric < 0)
		return OLDP;
	if (metric > 0)
		return NEWP;

	/*
	 * compare names
	 */
	opname = oldp->n_name;
	npname = newp->n_name;
	name = n->n_name;

	/* use longest common prefix with parent */
	while (*opname == *name && *npname == *name && *name) {
		opname++;
		npname++;
		name++;
	}
	if (*opname == *name)
		return OLDP;
	if (*npname == *name)
		return NEWP;

	/* use shorter host name */
	metric = strlen(opname) - strlen(npname);
	if (metric < 0)
		return OLDP;
	if (metric > 0)
		return NEWP;

	/* use larger lexicographically */
	metric = strcmp(opname, npname);
	if (metric < 0)
		return NEWP;
	return OLDP;
}

STATIC int
height(n)
	register node *n;
{	register int i = 0;

	if (n == 0)
		return 0;
	while ((n = n->n_parent) != 0)
		if (ISADOMAIN(n) || !(n->n_flag & NNET))
			i++;
	return i;
}
	
/*
 * return a copy of n ( == l->l_to).  we rely on n and its copy
 * pointing to the same name string, which is kludgey, but works
 * because the name is non-volatile.
 */

#define REUSABLE(n, l)	(((n)->n_flag & NTERMINAL) == 0 \
		      && ((n)->n_copy->n_flag & NTERMINAL) \
		      && !((n)->n_copy->n_flag & NALIAS) \
		      && !((l)->l_flag & LALIAS))
node *
ncopy(parent, l)
	register node *parent;
	register link *l;
{	register node *n, *ncp;

#ifdef DEBUG
	if (Vflag > 1)
		vprintf(stderr, "<%s> <- %s\n", l->l_to->n_name, parent->n_name);
#endif
	n = l->l_to;
	if (REUSABLE(n, l)) {
		Nlink++;
		return n->n_copy;	/* re-use */
	}
	NumNcopy++;
	l->l_to = ncp = newnode();
	ncp->n_name = n->n_name;	/* nonvolatile */
	ncp->n_tloc = --Hashpart;	/* better not be > 20% of total ... */
	if (Hashpart == Nheap)
		die("too many terminal links");
	Table[Hashpart] = ncp;
	ncp->n_copy = n->n_copy;	/* circular list */
	n->n_copy = ncp;
	ncp->n_link = lcopy(parent, n);
	ncp->n_flag = (n->n_flag & ~(NALIAS|ATSIGN|MAPPED|HASLEFT|HASRIGHT)) | NTERMINAL;
	return ncp;
}

/*
 * copy n's links but don't copy any terminal links
 * since n is itself at the end of a terminal link.
 *
 * this is a little messier than it should be, because
 * it wants to be recursive, but the recursion might be
 * very deep (for a long link list), so it iterates.
 *
 * why copy any links other than aliases?  hmmm ...
 */
STATIC link *
lcopy(parent, n)
	register node *parent, *n;
{	register link *l, *lcp;
	link *first = 0, *last = 0;
 
	for (l = n->n_link; l != 0; l = l->l_next) {
		/* skip if dest is already mapped */
		if ((l->l_to->n_flag & MAPPED) != 0)
			continue;
		/* don't copy terminal links */
		if ((l->l_flag & LTERMINAL) != 0)
			continue;
		/* comment needed */
		if (ALTEREGO(l->l_to, parent))
			continue;
#ifdef DEBUG
		if (Vflag > 1)
			vprintf(stderr, "\t-> %s\n", l->l_to->n_name);
#endif
		NumLcopy++;
		lcp = newlink();
		*lcp = *l;	/* struct copy */
		lcp->l_flag &= ~LTREE;
		if (ISANET(n))
			lcp->l_flag |= LTERMINAL;
 
		if (first == 0) {
			first = last = lcp;
		} else {
			last->l_next = lcp;
			last = lcp;
		}
	}
	if (last)
		last->l_next = 0;
	return first;
}
