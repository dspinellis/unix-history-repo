/*
 * @(#)nmalloc.c 1 (Caltech) 2/21/82
 *
 *	U of M Modified: 20 Jun 1983 ACT: strange hacks for Emacs
 *
 *	Nov 1983, Mike@BRL, Added support for 4.1C/4.2 BSD.
 *
 * This is a very fast storage allocator.  It allocates blocks of a small 
 * number of different sizes, and keeps free lists of each size.  Blocks
 * that don't exactly fit are passed up to the next larger size.  In this 
 * implementation, the available sizes are (2^n)-4 (or -16) bytes long.
 * This is designed for use in a program that uses vast quantities of
 * memory, but bombs when it runs out.  To make it a little better, it
 * warns the user when he starts to get near the end.
 *
 * June 84, ACT: modified rcheck code to check the range given to malloc,
 * rather than the range determined by the 2-power used.
 *
 * Jan 85, RMS: calls malloc_warning to issue warning on nearly full.
 * No longer Emacs-specific; can serve as all-purpose malloc for GNU.
 * You should call malloc_init to reinitialize after loading dumped Emacs.
 * Call malloc_stats to get info on memory stats if MSTATS turned on.
 * realloc knows how to return same block given, just changing its size,
 * if the power of 2 is correct.
 */

/*
 * nextf[i] is the pointer to the next free block of size 2^(i+3).  The
 * smallest allocatable block is 8 bytes.  The overhead information will
 * go in the first int of the block, and the returned pointer will point
 * to the second.
 *
#ifdef MSTATS
 * nmalloc[i] is the difference between the number of mallocs and frees
 * for a given block size.
#endif /* MSTATS */
 */

#define ISALLOC ((char) 0xf7)	/* magic byte that implies allocation */
#define ISFREE ((char) 0x54)	/* magic byte that implies free block */
				/* this is for error checking only */

extern char etext;

/* end of the program; can be changed by calling init_malloc */
static char *endofpure = &etext;

#ifdef MSTATS
static int nmalloc[30];
static int nmal, nfre;
#endif /* MSTATS */

/* If range checking is not turned on, all we have is a flag indicating
   whether memory is allocated, an index in nextf[], and a size field; to
   realloc() memory we copy either size bytes or 1<<(index+3) bytes depending
   on whether the former can hold the exact size (given the value of
   'index').  If range checking is on, we always need to know how much space
   is allocated, so the 'size' field is never used. */

struct mhead {
	char     mh_alloc;	/* ISALLOC or ISFREE */
	char     mh_index;	/* index in nextf[] */
/* Remainder are valid only when block is allocated */
	unsigned short mh_size;	/* size, if < 0x10000 */
#ifdef rcheck
	unsigned mh_nbytes;	/* number of bytes allocated */
	int      mh_magic4;	/* should be == MAGIC4 */
#endif /* rcheck */
	};

/* Access free-list pointer of a block.
  It is stored at block + 4.
  This is not a field in the mhead structure
  because we want sizeof (struct mhead)
  to describe the overhead for when the block is in use,
  and we do not want the free-list pointer to count in that.  */

#define CHAIN(a) \
  (*(struct mhead **) (sizeof (char *) + (char *) (a)))

#ifdef rcheck

/* To implement range checking, we write magic values in at the beginning and
   end of each allocated block, and make sure they are undisturbed whenever a
   free or a realloc occurs. */
/* Written in each of the 4 bytes following the block's real space */
#define MAGIC1 0x55
/* Written in the 4 bytes before the block's real space */
#define MAGIC4 0x55555555
#define ASSERT(p) if (!(p)) botch("p"); else
static
botch(s)
	char *s;
{

	printf("assertion botched: %s\n", s);
	abort();
}
#define EXTRA  4		/* 4 bytes extra for MAGIC1s */
#else
#define ASSERT(p)
#define EXTRA  0
#endif /* rcheck */

/* nextf[i] is free list of blocks of size 2**(i + 3)  */

static struct mhead *nextf[30];

#ifdef	M_WARN
/* Number of bytes of writable memory we can expect to be able to get */
static int  lim_data;
/* Level number of warnings already issued.
  0 -- no warnings issued.
  1 -- 75% warning already issued.
  2 -- 85% warning already issued.
*/
static int  warnlevel;
#endif /* M_WARN */

/* nonzero once initial bunch of free blocks made */
static int gotpool;

/* Cause reinitialization based on job parameters;
  also declare where the end of pure storage is. */
malloc_init (end)
    char *end; {
	endofpure = end;
#ifdef	M_WARN
	lim_data = 0;
	warnlevel = 0;
#endif /* M_WARN */
	}

static
morecore (nu)			/* ask system for more memory */
    register int nu; {		/* size index to get more of  */
	char   *sbrk ();
	register char  *cp;
	register int    nblks;
	register int    siz;

#ifdef	M_WARN
#ifndef BSD42
#ifdef USG
	extern long ulimit ();
	if (lim_data == 0)		/* find out how much we can get */
	    lim_data = ulimit (3, 0) - TEXT_START;
#else	/*HMS: was endif */
	if (lim_data == 0)		/* find out how much we can get */
	    lim_data = vlimit (LIM_DATA, -1);
#endif /* USG */	/HMS:* was not here */
#else
	if (lim_data == 0) {
		struct rlimit   XXrlimit;

		getrlimit (RLIMIT_DATA, &XXrlimit);
		lim_data = XXrlimit.rlim_cur;}	/* soft limit */
#endif /* BSD42 */
#endif /* M_WARN */

	/* On initial startup, get two blocks of each size up to 1k bytes */
	if (!gotpool)
	    getpool (), getpool (), gotpool = 1;

	/* Find current end of memory and issue warning if getting near max */

	cp = sbrk (0);
	siz = cp - endofpure;
#ifdef	M_WARN
	switch (warnlevel) {
	    case 0: 
		if (siz > (lim_data / 4) * 3) {
			warnlevel++;
			malloc_warning ("Warning: past 75% of memory limit");}
		break;
	    case 1: 
		if (siz > (lim_data / 20) * 17) {
			warnlevel++;
			malloc_warning ("Warning: past 85% of memory limit");}
		break;
	    case 2: 
		if (siz > (lim_data / 20) * 19) {
			warnlevel++;
			malloc_warning ("Warning: past 95% of memory limit");}
		break;}
#endif /* M_WARN */

	if ((int) cp & 0x3ff)	/* land on 1K boundaries */
	    sbrk (1024 - ((int) cp & 0x3ff));

	/* Take at least 2k, and figure out how many blocks of the desired size we're about to get */
	nblks = 1;
	if ((siz = nu) < 8)
	    nblks = 1 << ((siz = 8) - nu);

	if ((cp = sbrk (1 << (siz + 3))) == (char *) -1)
	    return;			/* no more room! */
	if ((int) cp & 7) {		/* shouldn't happen, but just in case */
		cp = (char *) (((int) cp + 8) & ~7);
		nblks--;}

	/* save new header and link the nblks blocks together */
	nextf[nu] = (struct mhead *) cp;
	siz = 1 << (nu + 3);
	while (1) {
		((struct mhead *) cp) -> mh_alloc = ISFREE;
		((struct mhead *) cp) -> mh_index = nu;
		if (--nblks <= 0) break;
		CHAIN ((struct mhead *) cp) = (struct mhead *) (cp + siz);
		cp += siz;}
/*	CHAIN ((struct mhead *) cp) = 0;	/* since sbrk() returns cleared core, this is already set */
	}

static
getpool () {
	register int nu;
	register char *cp = sbrk (0);

	if ((int) cp & 0x3ff)	/* land on 1K boundaries */
	    sbrk (1024 - ((int) cp & 0x3ff));

	/* Get 2k of storage */

	cp = sbrk (04000);
	if (cp == (char *) -1)
	    return;

	/* Divide it into an initial 8-word block
	plus one block of size 2**nu for nu = 3 ... 10.  */

	CHAIN (cp) = nextf[0];
	nextf[0] = (struct mhead *) cp;
	((struct mhead *) cp) -> mh_alloc = ISFREE;
	((struct mhead *) cp) -> mh_index = 0;
	cp += 8;

	for (nu = 0; nu < 7; nu++) {
		CHAIN (cp) = nextf[nu];
		nextf[nu] = (struct mhead *) cp;
		((struct mhead *) cp) -> mh_alloc = ISFREE;
		((struct mhead *) cp) -> mh_index = nu;
		cp += 8 << nu;}}

char *
malloc (n)		/* get a block */
    unsigned n; {
	register struct  mhead *p;
	register unsigned int  nbytes;
	register int    nunits = 0;

	/* Figure out how many bytes are required, rounding up to the nearest
	multiple of 4, then figure out which nextf[] area to use */
	nbytes = (n + sizeof *p + EXTRA + 3) & ~3;
		{
		register unsigned int   shiftr = (nbytes - 1) >> 2;

		while (shiftr >>= 1)
		    nunits++;
		}

	/* If there are no blocks of the appropriate size, go get some */
	/* COULD SPLIT UP A LARGER BLOCK HERE ... ACT */
	if (nextf[nunits] == 0)
	    morecore (nunits);

	/* Get one block off the list, and set the new list head */
	if ((p = nextf[nunits]) == 0)
	    return 0;
	nextf[nunits] = CHAIN (p);

	/* Check for free block clobbered */
	/* If not for this check, we would gobble a clobbered free chain ptr */
	/* and bomb out on the NEXT allocate of this size block */
	if (p -> mh_alloc != ISFREE || p -> mh_index != nunits)
#ifdef rcheck
	    botch ("block on free list clobbered");
#else
	    abort ();
#endif /* rcheck */

	/* Fill in the info, and if range checking, set up the magic numbers */
	p -> mh_alloc = ISALLOC;
#ifdef rcheck
	p -> mh_nbytes = n;
	p -> mh_magic4 = MAGIC4;
		{
		register char  *m = (char *) (p + 1) + n;

		*m++ = MAGIC1, *m++ = MAGIC1, *m++ = MAGIC1, *m = MAGIC1;
		}
#else
	p -> mh_size = n;
#endif /* rcheck */
#ifdef MSTATS
	nmalloc[nunits]++;
	nmal++;
#endif /* MSTATS */
	return (char *) (p + 1);}

free (mem)
    char *mem; {
	register struct mhead *p;
		{
		register char *ap = mem;

		ASSERT (ap != 0);
		p = (struct mhead *) ap - 1;
		ASSERT (p -> mh_alloc == ISALLOC);
#ifdef rcheck
		ASSERT (p -> mh_magic4 == MAGIC4);
		ap += p -> mh_nbytes;
		ASSERT (*ap++ == MAGIC1); ASSERT (*ap++ == MAGIC1);
		ASSERT (*ap++ == MAGIC1); ASSERT (*ap   == MAGIC1);
#endif /* rcheck */
		}
		{
		register int nunits = p -> mh_index;

		ASSERT (nunits <= 29);
		p -> mh_alloc = ISFREE;
		CHAIN (p) = nextf[nunits];
		nextf[nunits] = p;
#ifdef MSTATS
		nmalloc[nunits]--;
		nfre++;
#endif /* MSTATS */
		}
	}

char *
realloc (mem, n)
    char *mem;
    register unsigned n; {
	register struct mhead *p;
	register unsigned int tocopy;
	register int nbytes;
	register int nunits;

	if ((p = (struct mhead *) mem) == 0)
	    return malloc (n);
	p--;
	nunits = p -> mh_index;
	ASSERT (p -> mh_alloc == ISALLOC);
#ifdef rcheck
	ASSERT (p -> mh_magic4 == MAGIC4);
		{
		register char *m = mem + (tocopy = p -> mh_nbytes);
		ASSERT (*m++ == MAGIC1); ASSERT (*m++ == MAGIC1);
		ASSERT (*m++ == MAGIC1); ASSERT (*m   == MAGIC1);
		}
#else
	if (p -> mh_index >= 13)
	    tocopy = (1 << (p -> mh_index + 3)) - sizeof *p;
	else
	    tocopy = p -> mh_size;
#endif /* rcheck */

	/* See if desired size rounds to same power of 2 as actual size. */
	nbytes = (n + sizeof *p + EXTRA + 7) & ~7;

	/* If ok, use the same block, just marking its size as changed.  */
	if (nbytes > (4 << nunits) && nbytes <= (8 << nunits)) {
#ifdef rcheck
		register char *m = mem + tocopy;
		*m++ = 0;  *m++ = 0;  *m++ = 0;  *m++ = 0;
		p-> mh_nbytes = n;
		m = mem + n;
		*m++ = MAGIC1;  *m++ = MAGIC1;  *m++ = MAGIC1;  *m++ = MAGIC1;
#else
		p -> mh_size = n;
#endif /* rcheck */
		return mem;}

	if (n < tocopy)
	    tocopy = n;
		{
		register char *new;
		void bcopy();	/*HMS: here? */

		if ((new = malloc (n)) == 0)
		    return 0;
		bcopy (mem, new, tocopy);
		free (mem);
		return new;
		}
	}

#ifdef MSTATS
/* Return statistics describing allocation of blocks of size 2**n. */

struct mstats_value {
	int blocksize;
	int nfree;
	int nused;
	};

struct mstats_value
malloc_stats (size)
    int size; {
	struct mstats_value v;
	register int i;
	register struct mhead *p;

	v.nfree = 0;

	if (size < 0 || size >= 30) {
		v.blocksize = 0;
		v.nused = 0;
		return v;}

	v.blocksize = 1 << (size + 3);
	v.nused = nmalloc[size];

	for (p = nextf[size]; p; p = CHAIN (p))
	    v.nfree++;

	return v;}
#endif

/* how much space is available? */

unsigned freespace() {
  	register int i, j;
  	register struct mhead *p;
  	register unsigned space = 0;
	int local;	/* address only is used */

	space = (char *)&local - sbrk(0);	/* stack space */

  	for (i = 0; i < 30; i++) {
  		for (j = 0, p = nextf[i]; p; p = CHAIN (p), j++) ;
  		space += j * (1 << (i + 3));}

	return(space);}

/* How big is this cell? */

unsigned mc_size(cp)
    char *cp;{
	register struct mhead *p;

	if ((p = (struct mhead *) cp) == 0) {
		/*HMS? */
		}
	p--;
#ifdef rcheck
	return p -> mh_nbytes;
#else
	return (1 << (p -> mh_index + 3)) - sizeof *p;
/**/
/*	if (p -> mh_index >= 13)
/*	    return (1 << (p -> mh_index + 3)) - sizeof *p;
/*	else
/*	    return p -> mh_size;
/**/
#endif /* rcheck */
	}

/*HMS: Really should use memcpy, if available... */

void bcopy(source, dest, len)
    register char *source, *dest;
    register len; {
	register i;
	
	for (i = 0; i < len; i++)
	    *dest++ = *source++;}
