/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Rendell of Memorial University of Newfoundland.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tsort.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <errno.h>
#include <fcntl.h>
#include <db.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

/*
 *  Topological sort.  Input is a list of pairs of strings separated by
 *  white space (spaces, tabs, and/or newlines); strings are written to
 *  standard output in sorted order, one per line.
 *
 *  usage:
 *     tsort [inputfile]
 *  If no input file is specified, standard input is read.
 *
 *  Should be compatable with AT&T tsort HOWEVER the output is not identical
 *  (i.e. for most graphs there is more than one sorted order, and this tsort
 *  usually generates a different one then the AT&T tsort).  Also, cycle
 *  reporting seems to be more accurate in this version (the AT&T tsort
 *  sometimes says a node is in a cycle when it isn't).
 *
 *  Michael Rendell, michael@stretch.cs.mun.ca - Feb 26, '90
 */
#define	HASHSIZE	53		/* doesn't need to be big */
#define	NF_MARK		0x1		/* marker for cycle detection */
#define	NF_ACYCLIC	0x2		/* this node is cycle free */

typedef struct node_str NODE;

struct node_str {
	NODE **n_prevp;			/* pointer to previous node's n_next */
	NODE *n_next;			/* next node in graph */
	NODE **n_arcs;			/* array of arcs to other nodes */
	int n_narcs;			/* number of arcs in n_arcs[] */
	int n_arcsize;			/* size of n_arcs[] array */
	int n_refcnt;			/* # of arcs pointing to this node */
	int n_flags;			/* NF_* */
	char n_name[1];			/* name of this node */
};

typedef struct _buf {
	char *b_buf;
	int b_bsize;
} BUF;

DB *db;
NODE *graph;
NODE **cycle_buf;
NODE **longest_cycle;

void	 add_arc __P((char *, char *));
void	 err __P((const char *, ...));
int	 find_cycle __P((NODE *, NODE *, int, int));
NODE	*get_node __P((char *));
void	*grow_buf __P((void *, int));
void	 remove_node __P((NODE *));
void	 tsort __P((void));
void	 usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register BUF *b;
	register int c, n;
	FILE *fp;
	int bsize, ch, nused;
	BUF bufs[2];

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch(ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	switch(argc) {
	case 0:
		fp = stdin;
		break;
	case 1:
		if ((fp = fopen(*argv, "r")) == NULL)
			err("%s: %s", *argv, strerror(errno));
		break;
	default:
		usage();
	}

	for (b = bufs, n = 2; --n >= 0; b++)
		b->b_buf = grow_buf(NULL, b->b_bsize = 1024);

	/* parse input and build the graph */
	for (n = 0, c = getc(fp);;) {
		while (c != EOF && isspace(c))
			c = getc(fp);
		if (c == EOF)
			break;

		nused = 0;
		b = &bufs[n];
		bsize = b->b_bsize;
		do {
			b->b_buf[nused++] = c;
			if (nused == bsize)
				b->b_buf = grow_buf(b->b_buf, bsize *= 2);
			c = getc(fp);
		} while (c != EOF && !isspace(c));

		b->b_buf[nused] = '\0';
		b->b_bsize = bsize;
		if (n)
			add_arc(bufs[0].b_buf, bufs[1].b_buf);
		n = !n;
	}
	(void)fclose(fp);
	if (n)
		err("odd data count");

	/* do the sort */
	tsort();
	exit(0);
}

/* double the size of oldbuf and return a pointer to the new buffer. */
void *
grow_buf(bp, size)
	void *bp;
	int size;
{
	if ((bp = realloc(bp, (u_int)size)) == NULL)
		err("%s", strerror(errno));
	return (bp);
}

/*
 * add an arc from node s1 to node s2 in the graph.  If s1 or s2 are not in
 * the graph, then add them.
 */
void
add_arc(s1, s2)
	char *s1, *s2;
{
	register NODE *n1;
	NODE *n2;
	int bsize, i;

	n1 = get_node(s1);

	if (!strcmp(s1, s2))
		return;

	n2 = get_node(s2);

	/*
	 * Check if this arc is already here.
	 */
	for (i = 0; i < n1->n_narcs; i++)
		if (n1->n_arcs[i] == n2)
			return;
	/*
	 * Add it.
	 */
	if (n1->n_narcs == n1->n_arcsize) {
		if (!n1->n_arcsize)
			n1->n_arcsize = 10;
		bsize = n1->n_arcsize * sizeof(*n1->n_arcs) * 2;
		n1->n_arcs = grow_buf(n1->n_arcs, bsize);
		n1->n_arcsize = bsize / sizeof(*n1->n_arcs);
	}
	n1->n_arcs[n1->n_narcs++] = n2;
	++n2->n_refcnt;
}

/* Find a node in the graph (insert if not found) and return a pointer to it. */
NODE *
get_node(name)
	char *name;
{
	DBT data, key;
	NODE *n;

	if (db == NULL &&
	    (db = dbopen(NULL, O_RDWR, 0, DB_HASH, NULL)) == NULL)
		err("db: open: %s", name, strerror(errno));

	key.data = name;
	key.size = strlen(name) + 1;

	switch((*db->get)(db, &key, &data, 0)) {
	case 0:
		bcopy(data.data, &n, sizeof(n));
		return (n);
	case 1:
		break;
	default:
	case -1:
		err("db: get %s: %s", name, strerror(errno));
	}

	if ((n = malloc(sizeof(NODE) + key.size)) == NULL)
		err("%s", strerror(errno));

	n->n_narcs = 0;
	n->n_arcsize = 0;
	n->n_arcs = NULL;
	n->n_refcnt = 0;
	n->n_flags = 0;
	bcopy(name, n->n_name, key.size);

	/* Add to linked list. */
	if (n->n_next = graph)
		graph->n_prevp = &n->n_next;
	n->n_prevp = &graph;
	graph = n;

	/* Add to hash table. */
	data.data = &n;
	data.size = sizeof(n);
	if ((*db->put)(db, &key, &data, 0))
		err("db: put %s: %s", name, strerror(errno));
	return (n);
}

/* do topological sort on graph */
void
tsort()
{
	register NODE *n, *next;
	register int cnt;

	while (graph) {
		/*
		 * Keep getting rid of simple cases until there are none left,
		 * if there are any nodes still in the graph, then there is
		 * a cycle in it.
		 */
		do {
			for (cnt = 0, n = graph; n; n = next) {
				next = n->n_next;
				if (n->n_refcnt == 0) {
					remove_node(n);
					++cnt;
				}
			}
		} while (graph && cnt);

		if (!graph)
			break;

		if (!cycle_buf) {
			/*
			 * Allocate space for two cycle logs - one to be used
			 * as scratch space, the other to save the longest
			 * cycle.
			 */
			for (cnt = 0, n = graph; n; n = n->n_next)
				++cnt;
			cycle_buf = malloc((u_int)sizeof(NODE *) * cnt);
			longest_cycle = malloc((u_int)sizeof(NODE *) * cnt);
			if (cycle_buf == NULL || longest_cycle == NULL)
				err("%s", strerror(errno));
		}
		for (n = graph; n; n = n->n_next)
			if (!(n->n_flags & NF_ACYCLIC)) {
				if (cnt = find_cycle(n, n, 0, 0)) {
					register int i;

					(void)fprintf(stderr,
					    "tsort: cycle in data\n");
					for (i = 0; i < cnt; i++)
						(void)fprintf(stderr,
				"tsort: %s\n", longest_cycle[i]->n_name);
					remove_node(n);
					break;
				} else
					/* to avoid further checks */
					n->n_flags  = NF_ACYCLIC;
			}

		if (!n)
			err("internal error -- could not find cycle");
	}
}

/* print node and remove from graph (does not actually free node) */
void
remove_node(n)
	register NODE *n;
{
	register NODE **np;
	register int i;

	(void)printf("%s\n", n->n_name);
	for (np = n->n_arcs, i = n->n_narcs; --i >= 0; np++)
		--(*np)->n_refcnt;
	n->n_narcs = 0;
	*n->n_prevp = n->n_next;
	if (n->n_next)
		n->n_next->n_prevp = n->n_prevp;
}

/* look for the longest cycle from node from to node to. */
int
find_cycle(from, to, longest_len, depth)
	NODE *from, *to;
	int depth, longest_len;
{
	register NODE **np;
	register int i, len;

	/*
	 * avoid infinite loops and ignore portions of the graph known
	 * to be acyclic
	 */
	if (from->n_flags & (NF_MARK|NF_ACYCLIC))
		return (0);
	from->n_flags = NF_MARK;

	for (np = from->n_arcs, i = from->n_narcs; --i >= 0; np++) {
		cycle_buf[depth] = *np;
		if (*np == to) {
			if (depth + 1 > longest_len) {
				longest_len = depth + 1;
				(void)memcpy((char *)longest_cycle,
				    (char *)cycle_buf,
				    longest_len * sizeof(NODE *));
			}
		} else {
			len = find_cycle(*np, to, longest_len, depth + 1);
			if (len > longest_len)
				longest_len = len;
		}
	}
	from->n_flags &= ~NF_MARK;
	return (longest_len);
}

void
usage()
{
	(void)fprintf(stderr, "usage: tsort [file]\n");
	exit(1);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(const char *fmt, ...)
#else
err(fmt, va_alist)
	char *fmt;
        va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "tsort: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit(1);
	/* NOTREACHED */
}
