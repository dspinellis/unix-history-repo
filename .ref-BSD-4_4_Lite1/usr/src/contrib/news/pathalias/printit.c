/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)printit.c	9.4 89/02/07";
#endif

#include "def.h"

/*
 * print the routes by traversing the shortest path tree in preorder.
 * use lots of char bufs -- profiling indicates this costs about 5 kbytes
 */

/* exports */
extern void printit();

/* imports */
extern int Cflag, Vflag, Dflag, Fflag;
extern node *Home;
extern char *Netchars;
extern void die();
extern int strlen();

/* privates */
static link *Ancestor;	/* for -f option */
STATIC void preorder(), setpath(), printhost(), printdomain();
STATIC char *hostpath();
STATIC int printable();

/* in practice, even the longest paths are < 100 bytes */
#define PATHSIZE 512

void
printit()
{	link *l;
	char pbuf[PATHSIZE];

	/* print home */
	if (Cflag)
		printf("%ld\t", (long) Home->n_cost);
	printf("%s\t%%s\n", Home->n_name);
	
	strcpy(pbuf, "%s");
	for (l = Home->n_link; l; l = l->l_next) {
		if (l->l_flag & LTREE) {
			l->l_flag &= ~LTREE;
			Ancestor = l;
			preorder(l, pbuf);
			strcpy(pbuf, "%s");
		}
	}
	fflush(stdout);
	fflush(stderr);
}

/*
 * preorder traversal of shortest path tree.
 */
STATIC void
preorder(l, ppath)
	register link *l;
	char *ppath;
{	register node *n;
	node *ncp;		/* circular copy list */
	Cost cost;
	char npath[PATHSIZE];
	short p_dir;		/* DIR bits of parent (for nets) */
	char p_op;		/* net op of parent (for nets) */

	setpath(l, ppath, npath);
	n = l->l_to;
	if (printable(n)) {
		if (Fflag)
			cost = Ancestor->l_to->n_cost;
		else
			cost = n->n_cost;
		if (ISADOMAIN(n))
			printdomain(n, npath, cost);
		else if (!(n->n_flag & NNET)) {
			printhost(n, npath, cost);
		}
		n->n_flag |= PRINTED;
		for (ncp = n->n_copy; ncp != n; ncp = ncp->n_copy)
			ncp->n_flag |= PRINTED;
	}

	/* prepare routing bits for domain members */
	p_dir = l->l_flag & LDIR;
	p_op = l->l_netop;

	/* recursion */
	for (l = n->n_link; l; l = l->l_next) {
		if (!(l->l_flag & LTREE))
			continue;
		/* network member inherits the routing syntax of its gateway */
		if (ISANET(n)) {
			l->l_flag = (l->l_flag & ~LDIR) | p_dir;
			l->l_netop = p_op;
		}
		l->l_flag &= ~LTREE;
		preorder(l, npath);
	}
}

STATIC int
printable(n)
	register node *n;
{	node *ncp;
	link *l;

	if (n->n_flag & PRINTED)
		return 0;

	/* is there a cheaper copy? */
	for (ncp = n->n_copy; n != ncp; ncp = ncp->n_copy) {
		if (!(ncp->n_flag & MAPPED))
			continue;	/* unmapped copy */

		if (n->n_cost > ncp->n_cost)
			return 0;	/* cheaper copy */

		if (n->n_cost == ncp->n_cost && !(ncp->n_flag & NTERMINAL))
			return 0;	/* synthetic copy */
	}

	/* will a domain route suffice? */
	if (Dflag && !ISANET(n) && ISADOMAIN(n->n_parent)) {
		/*
		 * are there any interesting links?  a link
		 * is interesting if it doesn't point back
		 * to the parent, and is not an alias.
		 */

		/* check n */
		for (l = n->n_link; l; l = l->l_next) {
			if (l->l_to == n->n_parent)
				continue;
			if (!(l->l_flag & LALIAS))
				return 1;
		}

		/* check copies of n */
		for (ncp = n->n_copy; ncp != n; ncp = ncp->n_copy) {
			for (l = ncp->n_link; l; l = l->l_next) {
				if (l->l_to == n->n_parent)
					continue;
				if (!(l->l_flag & LALIAS))
					return 1;
			}
		}

		/* domain route suffices */
		return 0;
	}
	return 1;
}

STATIC void
setpath(l, ppath, npath) 
	link *l;
	register char *ppath, *npath;
{	register node *next, *parent;
	char netchar;

	next = l->l_to;
	parent = next->n_parent;
	netchar = NETCHAR(l);

	/* for magic @->% conversion */
	if (parent->n_flag & ATSIGN)
		next->n_flag |= ATSIGN;

	/*
	 * i've noticed that distant gateways to domains frequently get
	 * ...!gateway!user@dom.ain wrong.  ...!gateway!user%dom.ain
	 * seems to work, so if next is a domain and the parent is
	 * not the local host, force a magic %->@ conversion.  in this
	 * context, "parent" is the nearest ancestor that is not a net.
	 */
	while (ISANET(parent))
		parent = parent->n_parent;
	if (ISADOMAIN(next) && parent != Home)
		next->n_flag |= ATSIGN;

	/*
	 * special handling for nets (including domains) and aliases.
	 * part of the trick is to treat aliases to domains as 0 cost
	 * links.  (the author believes they should be declared as such
	 * in the input, but the world disagrees).
	 */
	if (ISANET(next) || ((l->l_flag & LALIAS) && !ISADOMAIN(parent))) {
		strcpy(npath, ppath);
		return;
	}
		
	if (netchar == '@')
		if (next->n_flag & ATSIGN)
			netchar = '%';	/* shazam?  shaman? */
		else
			next->n_flag |= ATSIGN;

	/* remainder should be a sprintf -- foo on '%' as an operator */
	for ( ; (*npath = *ppath) != 0; ppath++) {
		if (*ppath == '%') {
			switch(ppath[1]) {
			case 's':
				ppath++;
				npath = hostpath(npath, l, netchar);
				break;

			case '%':
				*++npath = *++ppath;
				npath++;
				break;

			default:
				die("unknown escape in setpath");
				break;
			}
		} else
			npath++;
	}
}

STATIC char *
hostpath(path, l, netchar)
	register char *path;
	register link *l;
	char netchar;
{	register node *prev;

	prev = l->l_to->n_parent;
	if (NETDIR(l) == LLEFT) {
		/* host!%s */
		strcpy(path, l->l_to->n_name);
		path += strlen(path);
		while (ISADOMAIN(prev)) {
			strcpy(path, prev->n_name);
			path += strlen(path);
			prev = prev->n_parent;
		}
		*path++ = netchar;
		if (netchar == '%')
			*path++ = '%';
		*path++ = '%';
		*path++ = 's';
	} else {
		/* %s@host */
		*path++ = '%';
		*path++ = 's';
		*path++ = netchar;
		if (netchar == '%')
			*path++ = '%';
		strcpy(path, l->l_to->n_name);
		path += strlen(path);
		while (ISADOMAIN(prev)) {
			strcpy(path, prev->n_name);
			path += strlen(path);
			prev = prev->n_parent;
		}
	}
	return path;
}

STATIC void
printhost(n, path, cost)
	register node *n;
	char *path;
	Cost cost;
{
	if (n->n_flag & PRINTED)
		die("printhost called twice");
	n->n_flag |= PRINTED;
	/* skip private hosts */
	if ((n->n_flag & ISPRIVATE) == 0) {
		if (Cflag)
			printf("%ld\t", (long) cost);
		fputs(n->n_name, stdout);
		putchar('\t');
		puts(path);
	}
}

STATIC void
printdomain(n, path, cost)
	register node *n;
	char *path;
	Cost cost;
{	node *p;

	if (n->n_flag & PRINTED)
		die("printdomain called twice");
	n->n_flag |= PRINTED;

	/*
	 * print a route for dom.ain if it is a top-level domain, unless
	 * it is private.
	 *
	 * print a route for sub.dom.ain only if all its ancestor dom.ains
	 * are private and sub.dom.ain is not private.
	 */
	if (!ISADOMAIN(n->n_parent)) {
		/* top-level domain */
		if (n->n_flag & ISPRIVATE) {
			vprintf(stderr, "ignoring private top-level domain %s\n", n->n_name);
			return;
		}
	} else {
		/* subdomain */
		for (p = n->n_parent; ISADOMAIN(p); p = p->n_parent)
			if (!(p->n_flag & ISPRIVATE))
				return;
		if (n->n_flag & ISPRIVATE)
			return;
	}

	/* print it (at last!) */
	if (Cflag)
		printf("%ld\t", (long) cost);
	do {
		fputs(n->n_name, stdout);
		n = n->n_parent;
	} while (ISADOMAIN(n));
	putchar('\t');
	puts(path);
}
