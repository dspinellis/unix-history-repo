/* pathalias -- by steve bellovin, as told to peter honeyman */
#ifndef lint
static char	*sccsid = "@(#)addnode.c	8.1 (down!honey) 86/01/19";
#endif

#include "def.h"

extern void	lowercase(), rehash();
extern node	*isprivate();
extern long	hash();

/*
 * these numbers are chosen because:
 *	-> they are prime,
 *	-> they are monotonic increasing,
 *	-> each is a tad smaller than a multiple of 1024,
 *	-> they form a fibonacci sequence (almost).
 * the first point yields good hash functions, the second is used for the
 * standard re-hashing implementation of open addressing, the third
 * optimizes for quirks in some mallocs i have seen, and the fourth simply
 * appeals to me.
 */
static int Primes[]	= {
#ifndef SQUANDER
	1021, 2039, 3067, 5113, 8179,
#endif
	13309, 21499, 0
};

static int	Tabindex = -1;
static int	Collision;	/* mark host name collisions in hash() */

node	*
addnode(name)
register char	*name;
{
	register long	i;
	register node	*n;

	if (Iflag)
		lowercase(name);

	/* is it a private host? */
	n = isprivate(name);
	if (n)
		return(n);

	i = hash(name, 0);
	if (Table[i]) 
		return(Table[i]);

	n = newnode();
	n->n_name = strsave(name);
	Table[i] = n;
	n->n_tloc = i;	/* essentially a back link to the table */
	if (Collision)
		n->n_flag |= COLLISION;	/* name collision with private host */

	return(n);
}

alias(n1, n2)
node	*n1, *n2;
{
	link	*l;
	extern link *addlink();

	l = addlink(n1, n2, (Cost) 0, DEFNET, DEFDIR);
	l->l_flag |= LALIAS;
	l = addlink(n2, n1, (Cost) 0, DEFNET, DEFDIR);
	l->l_flag |= LALIAS;
	if (Tflag)
		atrace(n1, n2);
}

/*
 * fold a string into a long int.  this algorithm ignores all but the last
 * eight bytes, which affects < 15% of all host names, many of which have
 * common prefixes anyway.
 */
STATIC long
fold(str)
register char	*str;
{
	register long	sum;

	sum = *str++;
	while (*str) {
		sum <<= 4;
		sum += *str++;
	}
	if (sum < 0) 
		sum = -sum;
	return(sum);
}

#define HASH1(n) ((n) % Tabsize);
#define HASH2(n) (Tabsize - 2 - ((n) % (Tabsize-2)))	/* princeton!rs */

/*
 * with a 0.75 high water mark, probes per access should be 1.84.
 * use long constant to force promotion.
 */
#define HIGHWATER	75L
#define isfull(n, size)	((n) > ((size) * HIGHWATER) / 100)

STATIC long
hash(name, unique)
char	*name;
{
	register long	probe, hash2;
	register node	*n;

	if (Tabindex < 0) {			/* first time */
		Tabindex = 0;
		Tabsize = Primes[0];
		Table = newtable(Tabsize);
	} else if (isfull(Ncount, Tabsize))
		rehash();			/* more, more! */
				
	probe = fold(name);
	/* don't change the order of the next two lines */
	hash2 = HASH2(probe);
	probe = HASH1(probe);
	/* thank you! */

	/*
	 * probe the hash table.
	 * if unique is set, we require a fresh slot.
	 * otherwise, use double hashing to find either
	 *  (1) an empty slot, or
	 *  (2) a non-private copy of this host name
	 *
	 * as a side effect, if we notice a collision with a private host
	 * we mark the other so that it is skipped at output time.
	 */
	Collision = 0;
	while ((n = Table[probe]) != 0) {
		if (strcmp(n->n_name, name) == 0) {
			if (unique)
				n->n_flag |= COLLISION;
			else if (n->n_flag & ISPRIVATE)
				Collision++;
			else
				break;	/* this is it! */
		}
		probe -= hash2;
		if (probe < 0)
			probe += Tabsize;
	}
	return(probe);
}

STATIC void
rehash()
{
	register node	**otable, **optr;
	register long	probe;
	long	osize;

#ifdef DEBUG
	hashanalyze();
#endif
	optr = Table + Tabsize - 1;	/* ptr to last */
	otable = Table;
	osize = Tabsize;
	Tabsize = Primes[++Tabindex];
	if (Tabsize == 0) {	/* need more prime numbers */
		fprintf(stderr, "%s: > %d hosts\n", ProgName, Primes[Tabindex-1]);
		badmagic(1);
	}
	vprintf(stderr, "rehash into %d\n", Tabsize);
	Table = newtable(Tabsize);

	do {
		if (*optr == 0)
			continue;	/* empty slot in old table */
		probe = hash((*optr)->n_name, (*optr)->n_flag & ISPRIVATE);
		if (Table[probe] != 0) {
			fprintf(stderr, "%s: rehash error\n", ProgName);
			badmagic(1);
		}
		Table[probe] = *optr;
		(*optr)->n_tloc = probe;
	} while (optr-- > otable);
	freetable(otable, osize);
}

hashanalyze()
{
	long	probe, hash2;
	int	count, i, collision[8];
	int	longest = 0, total = 0, slots = 0;
	int	nslots = sizeof(collision)/sizeof(collision[0]);

	if (!Vflag)
		return;

	strclear((char *) collision, sizeof(collision));
	for (i = 0; i < Tabsize; i++) {
		if (Table[i] == 0)
			continue;
		/* private hosts too hard to account for ... */
		if (Table[i]->n_flag & ISPRIVATE)
			continue;
		count = 1;
		probe = fold(Table[i]->n_name);
		/* don't change the order of the next two lines */
		hash2 = HASH2(probe);
		probe = HASH1(probe);
		/* thank you! */
		while (Table[probe] != 0
		    && strcmp(Table[probe]->n_name, Table[i]->n_name) != 0) {
			count++;
			probe -= hash2;
			if (probe < 0)
				probe += Tabsize;
		}
		if (Table[probe] == 0) {
			fprintf(stderr, "%s: impossible hash error for %s\n",
					ProgName, Table[i]->n_name);
			badmagic(1);
		}
		
		total += count;
		slots++;
		if (count > longest)
			longest = count;
		if (count >= nslots)
			count = 0;
		collision[count]++;
	}
	for (i = 1; i < nslots; i++)
		if (collision[i])
			fprintf(stderr, "%d chains: %d (%ld%%)\n",
				i, collision[i], (collision[i] * 100L)/ slots);
		if (collision[0])
			fprintf(stderr, "> %d chains: %d (%ld%%)\n",
				nslots - 1, collision[0],
				(collision[0] * 100L)/ slots);
	fprintf(stderr, "%2.2f probes per access, longest chain: %d\n",
		(double) total / slots, longest);
}

STATIC void
lowercase(s)
register char	*s;
{
	do {
		if (isupper(*s))
			*s -= 'A' - 'a';	/* assumes ASCII */
	} while (*s++);
}

/*
 * this might have to be recoded for performance if privates catch on
 */
STATIC node	*
isprivate(name)
register char	*name;
{
	register node	*n;

	for (n = Private; n != 0; n = n->n_private)
		if (strcmp(name, n->n_name) == 0)
			return(n);
	return(0);
}

fixprivate()
{
	register node	*n, *next;
	register long	i;

	for (n = Private; n != 0; n = next) {
		n->n_flag |= ISPRIVATE;		/* overkill, but safe */
		i = hash(n->n_name, 1);
		if (Table[i] != 0) {
			fprintf(stderr, "%s: impossible private node error on %s\n",
				ProgName, n->n_name);
			badmagic(1);
		}
	
		Table[i] = n;
		n->n_tloc = i;	/* essentially a back link to the table */
		next = n->n_private;
		n->n_private = 0;	/* clear for later use */
	}
	Private = 0;
}

node	*
addprivate(name)
register char	*name;
{
	register node	*n;

	if (Iflag)
		lowercase(name);
	if ((n = isprivate(name)) != 0)
		return(n);

	n = newnode();
	n->n_name = strsave(name);
	n->n_private = Private;
	Private = n;
	return(n);
}
