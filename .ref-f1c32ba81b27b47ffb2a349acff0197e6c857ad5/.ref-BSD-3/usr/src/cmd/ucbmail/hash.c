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
	register int i;

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
	switch (attnet) {
	case '!':
	case '^':
		xp->xh_attnet |= BN;
		break;

	default:
	case ':':
		xp->xh_attnet |= SN;
		break;

	case '@':
		xp->xh_attnet |= AN;
		break;
	}
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
			q += XHSIZE;
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
	m = mid & ~0200;
	if (m >= midfree) {
		printf("Use made of undefined machine id\n");
		return(NOSTR);
	}
	return(xtrahash[xtab[m]].xh_name);
}
