/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)printit.c	8.2 (down!honey) 86/01/29";
#endif

#include "def.h"

/* use lots of char bufs -- profiling indicates this costs about 5 kbytes */

/* in practice, even the longest paths are < 100 bytes */
#define PSIZE 512

printit()
{	link *l;
	char pbuf[PSIZE];

	/* print home */
	if (Cflag)
		printf("%ld\t", (long) Home->n_cost);
	printf("%s\t%%s\n", Home->n_name);
	
	strcpy(pbuf, "%s");
	for (l = Home->n_link; l; l = l->l_next) {
		if (l->l_flag & LTREE) {
			preorder(l, pbuf);
			strcpy(pbuf, "%s");
		}
	}
}

/* preorder traversal of shortest path tree */
preorder(l, ppath)
register link *l;
char *ppath;
{
	register node *n;
	char npath[PSIZE];

	setpath(l, ppath, npath);
	n = l->l_to;
	if ((n->n_flag & NNET) || ISADOMAIN(n))
		printnet(n, npath, n->n_cost);
	else
		printhost(n, npath, n->n_cost);
	for (l = n->n_link; l; l = l->l_next)
		if (l->l_flag & LTREE)
			preorder(l, npath);
}

setpath(l, ppath, npath) 
link *l;
register char *ppath, *npath;
{
	register node *next;
	char netchar;
	extern char *hostpath();

	next = l->l_to;
	/*
	 * for magic @-% conversion.
	 * assume that gateways to domains want no @'s
	 */
	if (next->n_parent->n_flag & ATSIGN || ISADOMAIN(next))
		next->n_flag |= ATSIGN;

	/* special handling for aliases , domains, and nets */
	if ((l->l_flag & LALIAS) || (next->n_flag & NNET) || ISADOMAIN(next)) {
		strcpy(npath, ppath);
		return;
	}
		
	netchar = NETCHAR(l);
	if (netchar == '@')
		if (next->n_flag & ATSIGN)
			netchar = '%';	/* shazam?  shaman? */
		else
			next->n_flag |= ATSIGN;

	/* remainder should be a sprintf -- foo on '%' as an operator */
	for ( ; *npath = *ppath; ppath++) {
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
				fprintf(stderr, "%s: %%%c found in setpath\n",
						ProgName, ppath[1]);
				badmagic(1);
				break;
			}
		} else
			npath++;
	}
}

char *
hostpath(path, l, netchar)
register char *path;
register link *l;
char netchar;
{
	register node *prev;

	prev = l->l_to->n_parent;
	if (NETDIR(l) == LLEFT) {
		/* host!user */
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
	return(path);
}

STATIC
printhost(n, path, cost)
node *n;
char *path;
Cost cost;
{
	/* skip private hosts */
	if ((n->n_flag & ISPRIVATE) == 0) {
		if (Cflag)
			printf("%ld\t", (long) cost);
		fputs(n->n_name, stdout);
		putchar('\t');
		puts(path);
	}
}

STATIC
printnet(n, path, cost)
node	*n;
char	*path;
Cost	cost;
{
	node	*parent;

	/* print domains only */
	if (!ISADOMAIN(n))
		return;

	/* print top-level domain */
	if (!ISADOMAIN(n->n_parent)) {
		if (n->n_flag & ISPRIVATE)
			fprintf(stderr, "%s: warning: private top-level domain %s ignored\n", ProgName, n->n_name);
		else
			printdomain(n, path, cost);
		return;
	}

	/* remainder is for subdomains */

	/* don't print if it has a non-private ancestor */
	for (parent = n->n_parent; parent; parent = parent->n_parent)
		if (ISADOMAIN(parent) && (parent->n_flag & ISPRIVATE) == 0)
			return;

	/* don't print if this domain is also private */
	if (n->n_flag & ISPRIVATE)
		return;

	/* ancestors all private, this domain not private */
	printdomain(n, path, cost);
}

STATIC
printdomain(n, path, cost)
node *n;
char *path;
Cost cost;
{
	/* skip subdomains, since the gateway to the parent suffices */
	if (ISADOMAIN(n->n_parent))
		return;
	if (Cflag)
		printf("%ld\t", (long) cost);
	do {
		fputs(n->n_name, stdout);
		n = n->n_parent;
	} while (ISADOMAIN(n));
	putchar('\t');
	puts(path);
}
