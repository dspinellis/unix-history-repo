/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)mapit.c	9.16 92/08/25";
#endif

#include "def.h"

#define chkheap(i)	/* void */
#define chkgap()	/* int */

#define trprint(stream, n) \
	fprintf((stream), (n)->n_flag & NTERMINAL ? "<%s>" : "%s", (n)->n_name)

/* exports */
/* invariant while mapping: Nheap < Hashpart */
long Hashpart;		/* start of unreached nodes */
long Nheap;		/* end of heap */
long NumNcopy, Nlink, NumLcopy;

void mapit();

/* imports */
extern long Nheap, Hashpart, Tabsize, Tcount;
extern int Tflag, Vflag;
extern node **Table, *Home;
extern char *Linkout, *Graphout;

extern void freelink(), resetnodes(), printit(), dumpgraph();
extern void showlinks(), die();
extern long pack(), allocation();
extern link *newlink(), *addlink();
extern int maptrace(), tiebreaker();
extern node *ncopy();


/* privates */
static long	Heaphighwater;
static link	**Heap;

STATIC void insert(), heapup(), heapdown(), heapswap(), backlinks();
STATIC void setheapbits(), mtracereport(), heapchildren(), otracereport();
STATIC link *min_node();
STATIC int dehash(), skiplink(), skipterminalalias();
STATIC Cost costof();
STATIC node *mappedcopy();

/* transform the graph to a shortest-path tree by marking tree edges */
void
mapit()
{	register node *n;
	register link *l;

	vprintf(stderr, "*** mapping\ttcount = %ld\n", Tcount);
	Tflag = Tflag && Vflag;		/* tracing here only if verbose */
	/* re-use the hash table space for the heap */
	Heap = (link **) Table;
	Hashpart = pack(0L, Tabsize - 1);

	/* expunge penalties from -a option and make circular copy lists */
	resetnodes();

	if (Linkout && *Linkout)	/* dump cheapest links */
		showlinks();
	if (Graphout && *Graphout)	/* dump the edge list */
		dumpgraph();

	/* insert Home to get things started */
	l = newlink();		/* link to get things started */
	l->l_to = Home;
	(void) dehash(Home);
	insert(l);

	/* main mapping loop */
	do {
		Heaphighwater = Nheap;
		while ((l = min_node()) != 0) {
			l->l_flag |= LTREE;
			n = l->l_to;
			if (n->n_flag & MAPPED)		/* sanity check */
				die("mapped node in heap");
			if (Tflag && maptrace(n, n))
				otracereport(n);	/* tracing */
			chkheap(1); chkgap();		/* debugging */
			n->n_flag |= MAPPED;
			heapchildren(n);	/* add children to heap */
		}
		vprintf(stderr, "heap hiwat %d\nalloc %ldk, ncopy = %ld, nlink = %ld, lcopy = %ld\n", Heaphighwater, allocation(), NumNcopy, Nlink, NumLcopy);

		if (Nheap != 0)		/* sanity check */
			die("null entry in heap");

		/*
		 * add back links from unreachable hosts to reachable
		 * neighbors, then remap.  asymptotically, this is
		 * quadratic; in practice, this is done once or twice,
		 * when n is small.
		 */
		backlinks();
	} while (Nheap > 0);

	if (Hashpart < Tabsize) {
		int foundone = 0;

		for ( ; Hashpart < Tabsize; Hashpart++) {
			if (Table[Hashpart]->n_flag & ISPRIVATE)
				continue;
			if (foundone++ == 0)
				fputs("You can't get there from here:\n", stderr);
			putc('\t', stderr);
			trprint(stderr, Table[Hashpart]);
			putc('\n', stderr);
		}
	}
}

STATIC void
heapchildren(n)
	register node *n;
{	register link *l;
	register node *next;
	register int mtrace;
	register Cost cost;

	for (l = n->n_link; l; l = l->l_next) {

		next = l->l_to;		/* neighboring node */
		mtrace = Tflag && maptrace(n, next);

		if (l->l_flag & LTREE)
			continue;

		if (l->l_flag & LTERMINAL)
			l->l_to = next = ncopy(n, l);

		if ((n->n_flag & NTERMINAL) && (l->l_flag & LALIAS))
			if (skipterminalalias(n, next))
				continue;
			else
				l->l_to = next = ncopy(n, l);

		if (next->n_flag & MAPPED) {
			if (mtrace)
				mtracereport(n, l, "-\talready mapped");
			continue;
		}
		cost = costof(n, l);

		if (skiplink(l, n, cost, mtrace))
			continue;

		/*
		 * put this link in the heap and restore the
		 * heap property.
		 */
		if (mtrace) {
			if (next->n_parent)
				mtracereport(next->n_parent, l, "*\tdrop");
			mtracereport(n, l, "+\tadd");
		}
		next->n_parent = n;
		if (dehash(next) == 0) {  /* first time */
			next->n_cost = cost;
			insert(l);	  /* insert at end */
			heapup(l);
		} else {
			/* replace inferior path */
			Heap[next->n_tloc] = l;
			if (cost > next->n_cost) {
				/* increase cost (gateway) */
				next->n_cost = cost;
				heapdown(l);
			} else if (cost < next->n_cost) {
				/* cheaper route */
				next->n_cost = cost;

				heapup(l);
			}
		}
		setheapbits(l);
		chkheap(1);

	}
}

/*
 * n is a terminal node just sucked out of the heap, next is an alias
 * for n.  if n was heaped because of a copy (ALTEREGO) of next, don't
 * heap next -- it will happen over and over and over and ...
 */
STATIC int
skipterminalalias(n, next)
	node *n, *next;
{

	while (n->n_flag & NALIAS) {
		n = n->n_parent;
		if (ALTEREGO(n, next))
			return 1;
	}
	return 0;
}

/*
 * return 1 if we definitely don't want want this link in the
 * shortest path tree, 0 if we might want it, i.e., best so far.
 *
 * if tracing is turned on, report only if this node is being skipped.
 */
STATIC int
skiplink(l, parent, cost, trace)
	link *l;		/* new link to this node */
	node *parent;		/* (potential) new parent of this node */
	register Cost cost;	/* new cost to this node */
	int trace;		/* trace this link? */
{	register node *n;	/* this node */
	register link *lheap;		/* old link to this node */

	n = l->l_to;

	/* first time we've reached this node? */
	if (n->n_tloc >= Hashpart)
		return 0;

	lheap = Heap[n->n_tloc];

	/* examine links to nets that require gateways */
	if (GATEWAYED(n)) {
		/* if exactly one is a gateway, use it */
		if ((lheap->l_flag & LGATEWAY) && !(l->l_flag & LGATEWAY)) {
			if (trace)
				mtracereport(parent, l, "-\told gateway");
			return 1;	/* old is gateway */
		}
		if (!(lheap->l_flag & LGATEWAY) && (l->l_flag & LGATEWAY))
			return 0;	/* new is gateway */

		/* no gateway or both gateways;  resolve in standard way ... */
	}

	/* examine dup link (sanity check) */
	if (n->n_parent == parent && (DEADLINK(lheap) || DEADLINK(l)))
		die("dup dead link");

	/*  examine cost */
	if (cost < n->n_cost)
		return 0;
	if (cost > n->n_cost) {
		if (trace)
			mtracereport(parent, l, "-\tcheaper");
		return 1;
	}

	/* all other things being equal, ask the oracle */
	if (tiebreaker(n, parent)) {
		if (trace)
			mtracereport(parent, l, "-\ttiebreaker");
		return 1;
	}
	return 0;
}

/* compute cost to next (l->l_to) via prev */
STATIC Cost
costof(prev, l)
	register node *prev;
	register link *l;
{	register node *next;
	register Cost cost;

	if (l->l_flag & LALIAS)
		return prev->n_cost;	/* by definition */

	next = l->l_to;
	cost = prev->n_cost + l->l_cost;	/* basic cost */
	if (cost >= INF)
		return cost + 1;

	/*
	 * heuristics:
	 *    charge for a dead link.
	 *    charge for getting past a terminal host
	 *    	or getting out of a dead host.
	 *    charge for getting into a gatewayed net (except at a gateway).
	 *    discourage mixing of syntax (when prev is a host).
	 *
	 * life was simpler when pathalias truly computed shortest paths.
	 */
	if (DEADLINK(l))
		cost += INF;				/* dead link */
	else if (DEADHOST(prev))
		cost += INF;				/* dead parent */
	else if (GATEWAYED(next) && !(l->l_flag & LGATEWAY))
		cost += INF;				/* not gateway */
	else if (!ISANET(prev)) {
		if ((NETDIR(l) == LLEFT && (prev->n_flag & HASRIGHT))
		 || (NETDIR(l) == LRIGHT && (prev->n_flag & HASLEFT)))
			cost += INF;			/* mixed syntax */
	}

	return cost;
}

/* binary heap implementation of priority queue */
STATIC void
insert(l)
	link *l;
{	register node *n;

	n = l->l_to;
	if (n->n_flag & MAPPED)
		die("insert mapped node");

	Heap[n->n_tloc] = 0;
	if (Heap[Nheap+1] != 0)
		die("heap error in insert");
	if (Nheap++ == 0) {
		Heap[1] = l;
		n->n_tloc = 1;
		return;
	}
	if (Vflag && Nheap > Heaphighwater)
		Heaphighwater = Nheap;	/* diagnostics */

	/* insert at the end.  caller must heapup(l). */
	Heap[Nheap] = l;
	n->n_tloc = Nheap;
}

/*
 * "percolate" up the heap by exchanging with the parent.  as in
 * min_node(), give tiebreaker() a chance to produce better, stable
 * routes by moving nets and domains close to the root, nets closer
 * than domains.
 *
 * i know this seems obscure, but it's harmless and cheap.  trust me.
 */
STATIC void
heapup(l)
	link *l;
{	register long cindx, pindx;	/* child, parent indices */
	register Cost cost;
	register node *child, *parent;

	child = l->l_to;

	cost = child->n_cost;
	for (cindx = child->n_tloc; cindx > 1; cindx = pindx) {
		pindx = cindx / 2;
		if (Heap[pindx] == 0)	/* sanity check */
			die("impossible error in heapup");
		parent = Heap[pindx]->l_to;
		if (cost > parent->n_cost)
			return;

		/* net/domain heuristic */
		if (cost == parent->n_cost) {
			if (!ISANET(child))
				return;
			if (!ISADOMAIN(parent))
				return;
			if (ISADOMAIN(child))
				return;
		}
		heapswap(cindx, pindx);
	}
}

/* extract min (== Heap[1]) from heap */
STATIC link	*
min_node()
{	link *rval, *lastlink;
	register link **rheap;

	if (Nheap == 0)
		return 0;

	rheap = Heap;		/* in register -- heavily used */
	rval = rheap[1];	/* return this one */

	/* move last entry into root and reheap */
	lastlink = rheap[Nheap];
	rheap[Nheap] = 0;

	if (--Nheap) {
		rheap[1] = lastlink;
		lastlink->l_to->n_tloc = 1;
		heapdown(lastlink);	/* restore heap property */
	}

	return rval;
}

/*
 * swap Heap[i] with smaller child, iteratively down the tree.
 *
 * given the opportunity, attempt to place nets and domains
 * near the root.  this helps tiebreaker() shun domain routes.
 */

STATIC void
heapdown(l)
	link *l;
{	register long pindx, cindx;
	register link **rheap = Heap;	/* in register -- heavily used */
	node *child, *rchild, *parent;

	pindx = l->l_to->n_tloc;
	parent = rheap[pindx]->l_to;	/* invariant */
	for ( ; (cindx = pindx * 2) <= Nheap; pindx = cindx) {
		/* pick lhs or rhs child */
		child = rheap[cindx]->l_to;
		if (cindx < Nheap) {
			/* compare with rhs child */
			rchild = rheap[cindx+1]->l_to;
			/*
			 * use rhs child if smaller than lhs child.
			 * if equal, use rhs if net or domain.
			 */
			if (child->n_cost > rchild->n_cost) {
				child = rchild;
				cindx++;
			} else if (child->n_cost == rchild->n_cost)
				if (ISANET(rchild)) {
					child = rchild;
					cindx++;
				}
		}

		/* child is the candidate for swapping */
		if (parent->n_cost < child->n_cost)
			break;

		/*
		 * heuristics:
		 *	move nets/domains up
		 *	move nets above domains
		 */
		if (parent->n_cost == child->n_cost) {
			if (!ISANET(child))
				break;
			if (ISANET(parent) && ISADOMAIN(child))
				break;
		}

		heapswap(pindx, cindx);
	}
}

/* exchange Heap[i] and Heap[j] pointers */
STATIC void
heapswap(i, j)
	long i, j;
{	register link *temp, **rheap;

	rheap = Heap;	/* heavily used -- put in register */
	temp = rheap[i];
	rheap[i] = rheap[j];
	rheap[j] = temp;
	rheap[j]->l_to->n_tloc = j;
	rheap[i]->l_to->n_tloc = i;
}

/* return 1 if n is already de-hashed (n_tloc < Hashpart), 0 o.w. */
STATIC int
dehash(n)
	register node *n;
{
	if (n->n_tloc < Hashpart)
		return 1;

	/* swap with entry in Table[Hashpart] */
	Table[Hashpart]->n_tloc = n->n_tloc;
	Table[n->n_tloc] = Table[Hashpart];
	Table[Hashpart] = n;

	n->n_tloc = Hashpart++;
	return 0;
}

/*
 * everything reachable has been mapped.  what to do about any
 * unreachable hosts?  the sensible thing to do is to dump them on
 * stderr and be done with it.  unfortunately, there are hundreds of
 * such hosts in the usenet maps.  so we take the low road: for each
 * unreachable host, we add a back link from its cheapest mapped child,
 * in the faint that a reverse path works.
 *
 * beats me why people want their error output in their map databases.
 */
STATIC void
backlinks()
{	register link *l;
	register node *n, *child;
	node *nomap;
	long i;

	/* hosts from Hashpart to Tabsize are unreachable */
	for (i = Hashpart; i < Tabsize; i++) {
		nomap = Table[i];
		/* if a copy has been mapped, we're ok */
		if (nomap->n_copy != nomap) {
			dehash(nomap);
			Table[nomap->n_tloc] = 0;
			nomap->n_tloc = 0;
			continue;
		}

		/* TODO: simplify this */		
		/* add back link from minimal cost child */
		child = 0;
		for (l = nomap->n_link; l; l = l->l_next) {
			n = l->l_to;
			/* never ever ever crawl out of a domain */
			if (ISADOMAIN(n))
				continue;
			if ((n = mappedcopy(n)) == 0)
				continue;
			if (child == 0) {
				child = n;
				continue;
			}
			if (n->n_cost > child->n_cost)
				continue;
			if (n->n_cost == child->n_cost) {
				nomap->n_parent = child; /* for tiebreaker */
				if (tiebreaker(nomap, n))
					continue;
			}
			child = n;
		}
		if (child == 0)
			continue;
		(void) dehash(nomap);
		l = addlink(child, nomap, INF, DEFNET, DEFDIR);	/* INF cost */
		nomap->n_parent = child;
		nomap->n_cost = costof(child, l);
		insert(l);
		heapup(l);
		if (Vflag > 1)
			fprintf(stderr, "backlink: %s <- %s\n", nomap->n_name, child->n_name);
	}
	vprintf(stderr, "%d backlinks\n", Nheap);
}

/* find a mapped copy of n if it exists */
STATIC node *
mappedcopy(n)
	register node *n;
{	register node *ncp;

	if (n->n_flag & MAPPED)
		return n;
	for (ncp = n->n_copy; ncp != n; ncp = ncp->n_copy)
		if (ncp->n_flag & MAPPED)
			return ncp;
	return 0;
}

/*
 * l has just been added or changed in the heap,
 * so reset the state bits for l->l_to.
 */
STATIC void
setheapbits(l)
	register link *l;
{	register node *n;
	register node *parent;

	n = l->l_to;
	parent = n->n_parent;
	n->n_flag &= ~(NALIAS|HASLEFT|HASRIGHT);	/* reset */

	/* record whether link is an alias */
	if (l->l_flag & LALIAS) {
		n->n_flag |= NALIAS;
		/* TERMINALity is inherited by the alias */
		if (parent->n_flag & NTERMINAL)
			n->n_flag |= NTERMINAL;
	}

	/* set left/right bits */
	if (NETDIR(l) == LLEFT || (parent->n_flag & HASLEFT))
		n->n_flag |= HASLEFT;
	if (NETDIR(l) == LRIGHT || (parent->n_flag & HASRIGHT))
		n->n_flag |= HASRIGHT;
}

STATIC void
mtracereport(from, l, excuse)
	node *from;
	link *l;
	char *excuse;
{	node *to = l->l_to;

	fprintf(stderr, "%-16s ", excuse);
	trprint(stderr, from);
	fputs(" -> ", stderr);
	trprint(stderr, to);
	fprintf(stderr, " (%ld, %ld, %ld) ", from->n_cost, l->l_cost, to->n_cost);
	if (to->n_parent) {
		trprint(stderr, to->n_parent);
		fprintf(stderr, " (%ld)", to->n_parent->n_cost);
	}
	putc('\n', stderr);
}

STATIC void
otracereport(n)
	node *n;
{
	if (n->n_parent)
		trprint(stderr, n->n_parent);
	else
		fputs("[root]", stderr);
	fputs(" -> ", stderr);
	trprint(stderr, n);
	fputs(" mapped\n", stderr);
}
	
#if 0
/* extremely time consuming, exhaustive check of heap sanity. */
chkheap(i)
{	int lhs, rhs;

	lhs = i * 2;
	rhs = lhs + 1;

	if (lhs <= Nheap) {
		if (Heap[i]->l_to->n_cost > Heap[lhs]->l_to->n_cost)
			die("chkheap failure on lhs");
		chkheap(lhs);
	}
	if (rhs <= Nheap) {
		if (Heap[i]->l_to->n_cost > Heap[rhs]->l_to->n_cost)
			die("chkheap failure on rhs");
		chkheap(rhs);
	}
#if 00
	/* this hasn't been used for years */
	for (i = 1; i < Nheap; i++) {
		link *l;

		vprintf(stderr, "%5d %-16s", i, Heap[i]->l_to->n_name);
		if ((l = Heap[i]->l_to->n_link) != 0) do {
			vprintf(stderr, "%-16s", l->l_to->n_name);
		} while ((l = l->l_next) != 0);
		vprintf(stderr, "\n");
	}
	for (i = Hashpart; i < Tabsize; i++) {
		link *l;
		node *n;

		vprintf(stderr, "%5d %-16s", i, Table[i]->n_name);
		if ((l = Table[i]->n_link) != 0) do {
			vprintf(stderr, "%-16s", l->l_to->n_name);
		} while ((l = l->l_next) != 0);
		vprintf(stderr, "\n");
	}
#endif /*00*/
		
}
#endif /*0*/

/* this isn't much use */
#if 0
STATIC int
chkgap()
{	static int gap = -1;
	int newgap;

	newgap = Hashpart - Nheap;
	if (gap == -1 || newgap < gap)
		gap = newgap;
	return gap;
}
#endif /*0*/
