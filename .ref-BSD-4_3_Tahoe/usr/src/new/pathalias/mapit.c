/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)mapit.c	8.1 (down!honey) 86/01/19";
#endif

#include "def.h"

/* privates */
extern void	reheap(), insert(), heapswap();
extern link	*min_node(), *rmlink();
extern Cost	costof();

static long	Nheap;
static long	Heaphighwater;
static link	**Heap;


/* transform the graph to a shortest-path tree by marking tree edges */

mapit()
{
	register node *n, *next;
	register link *l;
	link	*lprev, *lnext;
	Cost cost;

	/*
	 * re-use the hash table space for the heap.
	 */
	Heap = (link **) Table;

	pack();		/* remove holes in the Table */
	if (Linkout && *Linkout)	/* dump cheapest links */
		showlinks();
	if (Graphout && *Graphout)	/* dump the edge list */
		dumpgraph();

	/* invent and insert a link for Home to get things started */
	l = newlink();
	l->l_to = Home;
	(void) dehash(Home);
	insert(l);

	/* main mapping loop */
remap:
	Heaphighwater = Nheap;
	while ((l = min_node()) != 0) {
		l->l_flag |= LTREE;
		n = l->l_to;
		n->n_flag |= MAPPED;
 
		/* add children to heap */
		lprev = 0;
		for (l = n->n_link; l != 0; l = lnext) {

			next = l->l_to;		/* neighboring node */
			if (next->n_flag & MAPPED) {
				lnext = rmlink(l, lprev, n);
				continue;
			}
			cost = costof(n, l);

			if (skiplink(l, n, cost)) {
				lnext = rmlink(l, lprev, n);
				continue;
			}

			/*
			 * put this link in the heap, in a place where it may
			 * percolate up, but not down.  if new, or if cost is
			 * being increased, move to end.  otherwise, cost is
			 * same or less, so leave it where it is.  unfortunately,
			 * freeing a link already in the heap is too costly at
			 * this point.
			 *
			 * TODO: avoid heaping aliases and network members.
			 */
			if (dehash(next) == 0)	/* first time in heap */
				insert(l);	/* insert at end */
			else {
				/* replace heaped link by this one */
				if (cost > next->n_cost) {	/* gateway */
					/* move old link to end of heap */
					heapswap((long) (next->n_tloc), Nheap);
					next->n_tloc = Nheap;
				}
				Heap[next->n_tloc] = l;
			}
				
			next->n_cost = cost;
			next->n_parent = n;
			reheap(l);		/* restore heap property */

			/*
			 * note whether we got here via a gatewayed net.
			 * domains are presumed to require gateways.
			 * aliases inherit parent's gateway status.
			 */
			next->n_flag &= ~GATEWAYIN;
			if (l->l_flag & LALIAS)
				next->n_flag |= (n->n_flag & GATEWAYIN);
			else if (GATEWAYED(n))
				next->n_flag |= GATEWAYIN;
			lprev = l;	/* this link's a keeper */
			lnext = l->l_next;
		}

	}
	vprintf(stderr, "heap high water mark was %d\n", Heaphighwater);

	/* sanity check on implementation */
	if (Nheap != 0) {
		fprintf(stderr, "%s: null entry found in heap\n", ProgName);
		badmagic(1);
	}

	if (Hashpart < Tabsize) {
		/*
		 * add back links from unreachable hosts to reachable
		 * neighbors, then remap.  asymptotically, this is
		 * quadratic.  in practice, this is done exactly once.
		 */
		backlinks();
		if (Nheap)
			goto remap;
	}
	if (Hashpart < Tabsize) {
		fputs("You can't get there from here:\n", stderr);
		for ( ; Hashpart < Tabsize; Hashpart++) {
			fprintf(stderr, "\t%s", Table[Hashpart]->n_name);
			if (Table[Hashpart]->n_flag & (ISPRIVATE|COLLISION))
				fputs(" (private)", stderr);
			putc('\n', stderr);
		}
	}
}

/*
 * can this link be ignored?  if yes, return 1, o.w. 0.
 * a link can be skipped if it is not in the shortest path tree.
 */
STATIC int
skiplink(l, parent, cost)
link	*l;			/* new link to this node */
node	*parent;		/* new parent of this node */
Cost	cost;			/* new cost to this node */
{
	node	*n;		/* this node */
	link	*lheap;		/* existing link to this node */

	n = l->l_to;

	/* first time we've reached this node? */
	if (n->n_tloc >= Hashpart)
		return(0);

	lheap = Heap[n->n_tloc];

	/* examine links to nets that require gateways */
	if (GATEWAYED(n)) {
		/* if exactly one is a gateway, use it */
		if ((lheap->l_flag & LGATEWAY) && !(l->l_flag & LGATEWAY))
			return(1);	/* old is gateway */
		if (!(lheap->l_flag & LGATEWAY) && (l->l_flag & LGATEWAY))
			return(0);	/* new is gateway */

		/* no gateway or both gateways;  resolve in standard way ... */
	}

	/* examine dup link (sanity check) */
	if (n->n_parent == parent && ((lheap->l_flag & LDEAD) || (l->l_flag & LDEAD))) {
		fprintf(stderr, "%s: dup dead link not eliminated: %s -> %s\n",
			ProgName, parent->n_name, n->n_name);
		badmagic(1);
	}


	/*  examine cost */
	if (cost < n->n_cost)
		return(0);
	if (cost > n->n_cost)
		return(1);

	/* all other things being equal, consult the oracle */
	return(tiebreaker(n, parent));
}

STATIC Cost
costof(prev, l)
register node	*prev;
register link	*l;
{
	register node	*next;
	register Cost	cost;

	next = l->l_to;

	if (l->l_flag & LALIAS) {
		/* copy left/right bits */
		next->n_flag &= ~(HASLEFT|HASRIGHT);
		next->n_flag |= (prev->n_flag & (HASLEFT|HASRIGHT));
		return(prev->n_cost);	/* by definition */
	}

		
	cost = prev->n_cost + l->l_cost;	/* basic cost */

	/*
	 * heuristics:
	 *    charge for a dead link.
	 *    charge for getting out of a dead host.
	 *    charge for getting into a gatewayed net (except at a gateway).
	 *    discourage mixing of left and right syntax when next is a host.
	 *    charge for leaving a gatewayed net.
	 *
	 * life was simpler when pathalias computed true shortest paths.
	 */
	if (l->l_flag & LDEAD)		/* dead link */
		cost += INF/2;
	if (DEADHOST(prev))		/* dead host */
		cost += INF/2;
	if (GATEWAYED(next) && !(l->l_flag & LGATEWAY))	/* not gateway */
		cost += INF/2;
	if (!ISANET(next)) {
		/* charge for mixed syntax here */
		if ((NETDIR(l) == LLEFT && (prev->n_flag & HASRIGHT))
		 || (NETDIR(l) == LRIGHT && (prev->n_flag & HASLEFT)))
			cost += DEFCOST;
	}
	/*
	 * if reached by a gatewayed net, discourage further links.
	 * this has some relevance to common carriers and the FCC ...
	 * the penalty inheres to hosts, not aliases, nets, or domains.
	 */
	if ((prev->n_flag & GATEWAYIN) && !ISADOMAIN(prev) && !(prev->n_flag & NNET))
		cost += INF/2;	/* heavyweight, but appropriate */

	/* set left/right bits */
	next->n_flag &= ~(HASLEFT|HASRIGHT);
	if (NETDIR(l) == LLEFT || (prev->n_flag & HASLEFT))
		next->n_flag |= HASLEFT;
	if (NETDIR(l) == LRIGHT || (prev->n_flag & HASRIGHT))
		next->n_flag |= HASRIGHT;

	return(cost);
}

STATIC link *
rmlink(l, lprev, n)
link	*l, *lprev;
node	*n;
{
	link	*lnext;

	lnext = l->l_next;
	if (lprev)
		lprev->l_next = l->l_next;
	else
		n->n_link = l->l_next;
	free((char *) l);
	return(lnext);
}

/*
 * binary heap implementation of priority queue.
 * TODO: make the heap smaller by giving inserting a placeholder
 * for net members when the net is extracted.  this requires storing the
 * cost of a net in the net node itself -- yuck.  is it worth it?
 */

STATIC void
insert(l)
link	*l;
{
	register node	*n;

	n = l->l_to;
	Heap[n->n_tloc] = 0;
	if (Heap[Nheap+1] != 0) {
		fprintf(stderr, "%s: heap error in insert\n", ProgName);
		badmagic(1);
	}
	if (Nheap++ == 0) {
		Heap[1] = l;
		n->n_tloc = 1;
		return;
	}
	if (Vflag && Nheap > Heaphighwater)
		Heaphighwater = Nheap;	/* diagnostics */

	/* insert at the end.  caller must reheap(). */
	Heap[Nheap] = l;
	n->n_tloc = Nheap;
}

/*
 * replace existing link in heap by this one, then
 * "percolate" up the heap by exchanging with the parent
 */
STATIC void
reheap(l)
link	*l;
{
	register long	loc, parent;
	register Cost	cost;
	register node	*n, *np;

	n = l->l_to;

	cost = n->n_cost;
	for (loc = n->n_tloc; loc > 1; loc = parent) {
		parent = loc / 2;
		/* sanity check on implementation */
		if (Heap[parent] == 0) {
			fprintf(stderr, "%s: heap error in insert\n", ProgName);
			badmagic(1);
		}
		np = Heap[parent]->l_to;
		if (cost > np->n_cost)
			return;
		/* move nets below hosts for output stability */
		if (cost == np->n_cost && ((n->n_flag & NNET) || !(np->n_flag & NNET)))
			return;
		heapswap(loc, parent);
	}
}

/* extract min (== Heap[1]) from heap */
STATIC link	*
min_node()
{
	link *rval;
	register link **regheap;
	register long	i, child;
	
	if (Nheap == 0)
		return(0);

	regheap = Heap;		/* in register -- heavily used */
	rval = regheap[1];	/* return this one */
			
	/* move last entry into root, percolate down */
	regheap[1] = regheap[Nheap];
	regheap[1]->l_to->n_tloc = 1;
	regheap[Nheap] = 0;
	if (--Nheap == 0)
		return(rval);

	i = 1;
	for (;;) {
		/* swap with smaller child down the tree */
		child = i * 2;	/* lhs child is 2i, rhs is 2i+1. */
		if (child >= Nheap)
			return(rval);
		/* use rhs child if smaller than lhs child */
		if (regheap[child]->l_to->n_cost > regheap[child+1]->l_to->n_cost
		 || (regheap[child]->l_to->n_cost == regheap[child+1]->l_to->n_cost
		  && !ISANET(regheap[child+1]->l_to)))
			child++;
			
		if (regheap[i]->l_to->n_cost < regheap[child]->l_to->n_cost)
			return(rval);
		/* move nets below hosts for output stability */
		if (regheap[i]->l_to->n_cost == regheap[child]->l_to->n_cost
		 && (!ISANET(regheap[i]->l_to) || ISANET(regheap[child]->l_to)))
			return(rval);
		heapswap(i, child);
		i = child;
	}
	/*NOTREACHED*/
}

/* exchange Heap[i] and Heap[j] pointers */
STATIC void
heapswap(i, j)
long	i, j;
{
	register link	*temp, **regheap;

	regheap = Heap;	/* heavily used -- put in register */
	temp = regheap[i];
	regheap[i] = regheap[j];
	regheap[j] = temp;
	regheap[j]->l_to->n_tloc = j;
	regheap[i]->l_to->n_tloc = i;
}

/* return 1 if n is already de-hashed (n_tloc < Hashpart), 0 o.w. */
dehash(n)
register node	*n;
{
	if (n->n_tloc < Hashpart)
		return(1);

	/* swap with entry in Table[Hashpart] */
	Table[Hashpart]->n_tloc = n->n_tloc;
	Table[n->n_tloc] = Table[Hashpart];
	Table[Hashpart] = n;

	n->n_tloc = Hashpart++;
	return(0);
}

backlinks()
{
	link *l;
	node *n, *parent, *nomap;
	long i;

	for (i = Hashpart; i < Tabsize; i++) {
		nomap = Table[i];
		parent = 0;
		for (l = nomap->n_link; l; l = l->l_next) {
			n = l->l_to;
			if (!(n->n_flag & MAPPED))
				continue;
			if (parent == 0) {
				parent = n;
				continue;
			}
			if (n->n_cost > parent->n_cost)
				continue;
			if (n->n_cost == parent->n_cost) {
				nomap->n_parent = parent;
				if (tiebreaker(nomap, n))
					continue;
			}
			parent = n;
		}
		if (parent == 0)
			continue;
		(void) dehash(nomap);
		l = addlink(parent, nomap, INF, DEFNET, DEFDIR);
		nomap->n_parent = parent;
		nomap->n_cost = costof(parent, l);
		insert(l);
		reheap(l);
	}
	vprintf(stderr, "%d backlinks\n", Nheap);
}
