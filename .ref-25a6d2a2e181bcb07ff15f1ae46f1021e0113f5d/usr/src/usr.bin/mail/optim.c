/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifdef notdef
static char sccsid[] = "@(#)optim.c	5.9 (Berkeley) %G%";
#endif /* notdef */

/*
 * Mail -- a program for sending and receiving mail.
 *
 * Network name modification routines.
 */

#include "rcv.h"
#include "configdefs.h"

/*
 * Map a name into the correct network "view" of the
 * name.  This is done by prepending the name with the
 * network address of the sender, then optimizing away
 * nonsense.
 */

char *
netmap(name, from)
	char name[], from[];
{
	char nbuf[BUFSIZ], ret[BUFSIZ];
	register char *cp;

	if (strlen(from) == 0)
		return(name);
	if (any('@', name) || any('%', name))
		return(savestr(arpafix(name, from)));
	cp = revarpa(from);
	if (cp == NOSTR)
		return(name);
	strcpy(nbuf, cp);
	cp = &nbuf[strlen(nbuf) - 1];
	while (!any(*cp, metanet) && cp > nbuf)
		cp--;
	if (cp == nbuf)
		return(name);
	*++cp = 0;
	if ((cp = revarpa(name)) != NOSTR)
		strcat(nbuf, cp);
	optim(nbuf, ret);
	cp = revarpa(ret);
	if (cp && !icequal(name, cp))
		return(savestr(cp));
	return(name);
}

/*
 * Turn a network machine name into a unique character
 */
netlook(machine, attnet)
	char machine[];
{
	register struct netmach *np;
	register char *cp, *cp2;
	char nbuf[BUFSIZ];

	/*
	 * Make into lower case.
	 */

	for (cp = machine, cp2 = nbuf; *cp; *cp2++ = little(*cp++))
		if (cp2 >= &nbuf[sizeof(nbuf)-1])
			break;
	*cp2 = 0;

	/*
	 * If a single letter machine, look through those first.
	 */

	if (strlen(nbuf) == 1)
		for (np = netmach; np->nt_mid != 0; np++)
			if (np->nt_mid == nbuf[0])
				return(nbuf[0]);

	/*
	 * Look for usual name
	 */

	for (np = netmach; np->nt_mid != 0; np++)
		if (strcmp(np->nt_machine, nbuf) == 0)
			return(np->nt_mid);

	/*
	 * Look in side hash table.
	 */

	return(mstash(nbuf, attnet));
}

/*
 * Make a little character.
 */

little(c)
	register int c;
{

	if (c >= 'A' && c <= 'Z')
		c += 'a' - 'A';
	return(c);
}

/*
 * Turn a network unique character identifier into a network name.
 */

char *
netname(mid)
{
	register struct netmach *np;
	char *mlook();

	if (mid & 0200)
		return(mlook(mid));
	for (np = netmach; np->nt_mid != 0; np++)
		if (np->nt_mid == mid)
			return(np->nt_machine);
	return(NOSTR);
}

/*
 * Deal with arpa net addresses.  The way this is done is strange.
 * In particular, if the destination arpa net host is not Berkeley,
 * then the address is correct as stands.  Otherwise, we strip off
 * the trailing @Berkeley, then cook up a phony person for it to
 * be from and optimize the result.
 */
char *
arpafix(name, from)
	char name[];
	char from[];
{
	register char *cp;
	register int arpamach;
	char newname[BUFSIZ];
	char fake[5];
	char fakepath[20];

	if (debug) {
		fprintf(stderr, "arpafix(%s, %s)\n", name, from);
	}
	cp = rindex(name, '@');
	if (cp == NOSTR)
		cp = rindex(name, '%');
	if (cp == NOSTR) {
		fprintf(stderr, "Somethings amiss -- no @ or %% in arpafix\n");
		return(name);
	}
	cp++;
	arpamach = netlook(cp, '@');
	if (arpamach == 0) {
		if (debug)
			fprintf(stderr, "machine %s unknown, uses: %s\n", cp, name);
		return(name);
	}
	if (((nettype(arpamach) & nettype(LOCAL)) & ~AN) == 0) {
		if (debug)
			fprintf(stderr, "machine %s known but remote, uses: %s\n",
			    cp, name);
		return(name);
	}
	strcpy(newname, name);
	cp = rindex(newname, '@');
	if (cp == NOSTR)
		cp = rindex(newname, '%');
	*cp = 0;
	fake[0] = arpamach;
	fake[1] = ':';
	fake[2] = LOCAL;
	fake[3] = ':';
	fake[4] = 0;
	prefer(fake);
	strcpy(fakepath, netname(fake[0]));
	stradd(fakepath, fake[1]);
	strcat(fakepath, "daemon");
	if (debug)
		fprintf(stderr, "machine local, call netmap(%s, %s)\n",
		    newname, fakepath);
	return(netmap(newname, fakepath));
}

/*
 * Take a network machine descriptor and find the types of connected
 * nets and return it.
 */

nettype(mid)
{
	register struct netmach *np;

	if (mid & 0200)
		return(mtype(mid));
	for (np = netmach; np->nt_mid != 0; np++)
		if (np->nt_mid == mid)
			return(np->nt_type);
	return(0);
}

/*
 * Hashing routines to salt away machines seen scanning
 * networks paths that we don't know about.
 */

#define	XHSIZE		19		/* Size of extra hash table */
#define	NXMID		(XHSIZE*3/4)	/* Max extra machines */

struct xtrahash {
	char	*xh_name;		/* Name of machine */
	short	xh_mid;			/* Machine ID */
	short	xh_attnet;		/* Attached networks */
} xtrahash[XHSIZE];

struct xtrahash	*xtab[XHSIZE];		/* F: mid-->machine name */

short	midfree;			/* Next free machine id */

/*
 * Initialize the extra host hash table.
 * Called by sreset.
 */

minit()
{
	register struct xtrahash *xp, **tp;

	midfree = 0;
	tp = &xtab[0];
	for (xp = &xtrahash[0]; xp < &xtrahash[XHSIZE]; xp++) {
		xp->xh_name = NOSTR;
		xp->xh_mid = 0;
		xp->xh_attnet = 0;
		*tp++ = (struct xtrahash *) 0;
	}
}

/*
 * Stash a net name in the extra host hash table.
 * If a new entry is put in the hash table, deduce what
 * net the machine is attached to from the net character.
 *
 * If the machine is already known, add the given attached
 * net to those already known.
 */

mstash(name, attnet)
	char name[];
{
	register struct xtrahash *xp;
	struct xtrahash *xlocate();
	int x;

	xp = xlocate(name);
	if (xp == (struct xtrahash *) 0) {
		printf("Ran out of machine id spots\n");
		return(0);
	}
	if (xp->xh_name == NOSTR) {
		if (midfree >= XHSIZE) {
			printf("Out of machine ids\n");
			return(0);
		}
		xtab[midfree] = xp;
		xp->xh_name = savestr(name);
		xp->xh_mid = 0200 + midfree++;
	}
	x = ntype(attnet);
	if (x == 0)
		xp->xh_attnet |= SN;
	else
		xp->xh_attnet |= x;
	return(xp->xh_mid);
}

/*
 * Search for the given name in the hash table
 * and return the pointer to it if found, or to the first
 * empty slot if not found.
 *
 * If no free slots can be found, return 0.
 */

struct xtrahash *
xlocate(name)
	char name[];
{
	register int h, q, i;
	register char *cp;
	register struct xtrahash *xp;

	for (h = 0, cp = name; *cp; h = (h << 2) + *cp++)
		;
	if (h < 0 && (h = -h) < 0)
		h = 0;
	h = h % XHSIZE;
	cp = name;
	for (i = 0, q = 0; q < XHSIZE; i++, q = i * i) {
		xp = &xtrahash[(h + q) % XHSIZE];
		if (xp->xh_name == NOSTR)
			return(xp);
		if (strcmp(cp, xp->xh_name) == 0)
			return(xp);
		if (h - q < 0)
			h += XHSIZE;
		xp = &xtrahash[(h - q) % XHSIZE];
		if (xp->xh_name == NOSTR)
			return(xp);
		if (strcmp(cp, xp->xh_name) == 0)
			return(xp);
	}
	return((struct xtrahash *) 0);
}

/*
 * Return the name from the extra host hash table corresponding
 * to the passed machine id.
 */

char *
mlook(mid)
{
	register int m;

	if ((mid & 0200) == 0)
		return(NOSTR);
	m = mid & 0177;
	if (m >= midfree) {
		printf("Use made of undefined machine id\n");
		return(NOSTR);
	}
	return(xtab[m]->xh_name);
}

/*
 * Return the bit mask of net's that the given extra host machine
 * id has so far.
 */

mtype(mid)
{
	register int m;

	if ((mid & 0200) == 0)
		return(0);
	m = mid & 0177;
	if (m >= midfree) {
		printf("Use made of undefined machine id\n");
		return(0);
	}
	return(xtab[m]->xh_attnet);
}

/*
 * Take a network name and optimize it.  This gloriously messy
 * operation takes place as follows:  the name with machine names
 * in it is tokenized by mapping each machine name into a single
 * character machine id (netlook).  The separator characters (network
 * metacharacters) are left intact.  The last component of the network
 * name is stripped off and assumed to be the destination user name --
 * it does not participate in the optimization.  As an example, the
 * name "research!vax135!research!ucbvax!bill" becomes, tokenized,
 * "r!x!r!v!" and "bill"  A low level routine, optim1, fixes up the
 * network part (eg, "r!x!r!v!"), then we convert back to network
 * machine names and tack the user name on the end.
 *
 * The result of this is copied into the parameter "name"
 */

optim(net, name)
	char net[], name[];
{
	char netcomp[BUFSIZ], netstr[40], xfstr[40];
	register char *cp, *cp2;
	register int c;

	strcpy(netstr, "");
	cp = net;
	for (;;) {
		/*
		 * Rip off next path component into netcomp
		 */
		cp2 = netcomp;
		while (*cp && !any(*cp, metanet))
			*cp2++ = *cp++;
		*cp2 = 0;
		/*
		 * If we hit null byte, then we just scanned
		 * the destination user name.  Go off and optimize
		 * if its so.
		 */
		if (*cp == 0)
			break;
		if ((c = netlook(netcomp, *cp)) == 0) {
			printf("No host named \"%s\"\n", netcomp);
err:
			strcpy(name, net);
			return;
		}
		stradd(netstr, c);
		stradd(netstr, *cp++);
		/*
		 * If multiple network separators given,
		 * throw away the extras.
		 */
		while (any(*cp, metanet))
			cp++;
	}
	if (strlen(netcomp) == 0) {
		printf("net name syntax\n");
		goto err;
	}
	optim1(netstr, xfstr);

	/*
	 * Convert back to machine names.
	 */

	cp = xfstr;
	strcpy(name, "");
	while (*cp) {
		if ((cp2 = netname(*cp++)) == NOSTR) {
			printf("Made up bad net name\n");
			printf("Machine code %c (0%o)\n", cp[-1], cp[-1]);
			printf("Sorry -- dumping now.  Alert K. Shoens\n");
			core();
			goto err;
		}
		strcat(name, cp2);
		stradd(name, *cp++);
	}
	strcat(name, netcomp);
}

/*
 * Take a string of network machine id's and separators and
 * optimize them.  We process these by pulling off maximal
 * leading strings of the same type, passing these to the appropriate
 * optimizer and concatenating the results.
 */

optim1(netstr, name)
	char netstr[], name[];
{
	char path[40], rpath[40];
	register char *cp, *cp2;
	register int tp, nc;

	cp = netstr;
	prefer(cp);
	strcpy(name, "");
	/*
	 * If the address ultimately points back to us,
	 * just return a null network path.
	 */
	if (strlen(cp) > 1 && cp[strlen(cp) - 2] == LOCAL)
		return;
	while (*cp != 0) {
		strcpy(path, "");
		tp = ntype(cp[1]);
		nc = cp[1];
		while (*cp && tp == ntype(cp[1])) {
			stradd(path, *cp++);
			cp++;
		}
		switch (netkind(tp)) {
		default:
			strcpy(rpath, path);
			break;

		case IMPLICIT:
			optimimp(path, rpath);
			break;

		case EXPLICIT:
			optimex(path, rpath);
			break;
		}
		for (cp2 = rpath; *cp2 != 0; cp2++) {
			stradd(name, *cp2);
			stradd(name, nc);
		}
	}
	optiboth(name);
	prefer(name);
}

/*
 * Return the network of the separator --
 *	AN for arpa net
 *	BN for Bell labs net
 *	SN for Schmidt (berkeley net)
 *	0 if we don't know.
 */

ntype(nc)
	register int nc;
{
	register struct ntypetab *np;

	for (np = ntypetab; np->nt_char != 0; np++)
		if (np->nt_char == nc)
			return(np->nt_bcode);
	return(0);
}

/*
 * Return the kind of routing used for the particular net
 * EXPLICIT means explicitly routed
 * IMPLICIT means implicitly routed
 * 0 means don't know
 */

netkind(nt)
	register int nt;
{
	register struct nkindtab *np;

	for (np = nkindtab; np->nk_type != 0; np++)
		if (np->nk_type == nt)
			return(np->nk_kind);
	return(0);
}

/*
 * Do name optimization for an explicitly routed network (eg BTL network).
 */

optimex(net, name)
	char net[], name[];
{
	register char *cp, *rp;
	register int m;

	strcpy(name, net);
	cp = name;
	if (strlen(cp) == 0)
		return(-1);
	if (cp[strlen(cp)-1] == LOCAL) {
		name[0] = 0;
		return(0);
	}
	for (cp = name; *cp; cp++) {
		m = *cp;
		rp = rindex(cp+1, m);
		if (rp != NOSTR)
			strcpy(cp, rp);
	}
	return(0);
}

/*
 * Do name optimization for implicitly routed network (eg, arpanet,
 * Berkeley network)
 */

optimimp(net, name)
	char net[], name[];
{
	register char *cp;
	register int m;

	cp = net;
	if (strlen(cp) == 0)
		return(-1);
	m = cp[strlen(cp) - 1];
	if (m == LOCAL) {
		strcpy(name, "");
		return(0);
	}
	name[0] = m;
	name[1] = 0;
	return(0);
}

/*
 * Perform global optimization on the given network path.
 * The trick here is to look ahead to see if there are any loops
 * in the path and remove them.  The interpretation of loops is
 * more strict here than in optimex since both the machine and net
 * type must match.
 */

optiboth(net)
	char net[];
{
	register char *cp, *cp2;
	char *rpair();

	cp = net;
	if (strlen(cp) == 0)
		return;
	if ((strlen(cp) % 2) != 0) {
		printf("Strange arg to optiboth\n");
		return;
	}
	while (*cp) {
		cp2 = rpair(cp+2, *cp);
		if (cp2 != NOSTR)
			strcpy(cp, cp2);
		cp += 2;
	}
}

/*
 * Find the rightmost instance of the given (machine, type) pair.
 */

char *
rpair(str, mach)
	char str[];
{
	register char *cp, *last;

	cp = str;
	last = NOSTR;
	while (*cp) {
		if (*cp == mach)
			last = cp;
		cp += 2;
	}
	return(last);
}

/*
 * Change the network separators in the given network path
 * to the preferred network transmission means.
 */

prefer(name)
	char name[];
{
	register char *cp;
	register int state, n;

	state = LOCAL;
	for (cp = name; *cp; cp += 2) {
		n = best(state, *cp);
		if (n)
			cp[1] = n;
		state = *cp;
	}
}

/*
 * Return the best network separator for the given machine pair.
 */

best(src, dest)
{
	register int dtype, stype;
	register struct netorder *np;

	stype = nettype(src);
	dtype = nettype(dest);
	fflush(stdout);
	if (stype == 0 || dtype == 0) {
		printf("ERROR:  unknown internal machine id\n");
		return(0);
	}
	if ((stype & dtype) == 0)
		return(0);
	np = &netorder[0];
	while ((np->no_stat & stype & dtype) == 0)
		np++;
	return(np->no_char);
}

#ifdef	GETHOST
/*
 * Initialize the network name of the current host.
 */
inithost()
{
	register struct netmach *np;
	static char host[64];

	gethostname(host, sizeof host);
	for (np = netmach; np->nt_machine != 0; np++)
		if (strcmp(np->nt_machine, EMPTY) == 0)
			break;
	if (np->nt_machine == 0) {
		printf("Cannot find empty slot for dynamic host entry\n");
		exit(1);
	}
	np->nt_machine = host;
}
#endif	GETHOST

/*
 * Code to twist around arpa net names.
 */

#define WORD 257			/* Token for a string */

static	char netbuf[256];
static	char *yylval;

/*
 * Reverse all of the arpa net addresses in the given name to
 * be of the form "host @ user" instead of "user @ host"
 * This function is its own inverse.
 */

char *
revarpa(str)
	char str[];
{

	if (yyinit(str) < 0)
		return(NOSTR);
	if (name())
		return(NOSTR);
	if (strcmp(str, netbuf) == 0)
		return(str);
	return(savestr(netbuf));
}

/*
 * Parse (by recursive descent) network names, using the following grammar:
 *	name:
 *		term {':' term}
 *		term {'^' term}
 *		term {'!' term}
 *		term '@' name
 *		term '%' name
 *
 *	term:
 *		string of characters.
 */

name()
{
	register int t;
	register char *cp;

	for (;;) {
		t = yylex();
		if (t != WORD)
			return(-1);
		cp = yylval;
		t = yylex();
		switch (t) {
		case 0:
			strcat(netbuf, cp);
			return(0);

		case '@':
		case '%':
			if (name())
				return(-1);
			stradd(netbuf, '@');
			strcat(netbuf, cp);
			return(0);

		case WORD:
			return(-1);

		default:
			strcat(netbuf, cp);
			stradd(netbuf, t);
		}
	}
}

/*
 * Scanner for network names.
 */

static	char *charp;			/* Current input pointer */
static	int nexttok;			/* Salted away next token */

/*
 * Initialize the network name scanner.
 */

yyinit(str)
	char str[];
{
	static char lexbuf[BUFSIZ];

	netbuf[0] = 0;
	if (strlen(str) >= sizeof lexbuf - 1)
		return(-1);
	nexttok = 0;
	strcpy(lexbuf, str);
	charp = lexbuf;
	return(0);
}

/*
 * Scan and return a single token.
 * yylval is set to point to a scanned string.
 */

yylex()
{
	register char *cp, *dot;
	register int s;

	if (nexttok) {
		s = nexttok;
		nexttok = 0;
		return(s);
	}
	cp = charp;
	while (*cp && isspace(*cp))
		cp++;
	if (*cp == 0)
		return(0);
	if (any(*cp, metanet)) {
		charp = cp+1;
		return(*cp);
	}
	dot = cp;
	while (*cp && !any(*cp, metanet) && !any(*cp, " \t"))
		cp++;
	if (any(*cp, metanet))
		nexttok = *cp;
	if (*cp == 0)
		charp = cp;
	else
		charp = cp+1;
	*cp = 0;
	yylval = dot;
	return(WORD);
}
