/*	param.h	1.1	86/01/05	*/

/*
 * Machine dependent constants for TAHOE.
 */
#define	NBPG	1024		/* bytes/page */
#define	PGOFSET	(NBPG-1)	/* byte offset into page */
#define	PGSHIFT	10		/* LOG2(NBPG) */

#define	CLSIZE		1
#define	CLSIZELOG2	0

#define	SSIZE	2			/* initial stack size/NBPG */
#define	SINCR	2			/* increment of stack/NBPG */
#define	UPAGES	6			/* pages of u-area (2 stack pages) */
#define IOBASE	((caddr_t)0xfff00000)	/* base of Versabus address space */

#define	MAXCKEY	255		/* maximal allowed code key */
#define	MAXDKEY	255		/* maximal allowed data key */
#define	NCKEY	(MAXCKEY+1)	/* # code keys, including 0 (reserved) */
#define	NDKEY	(MAXDKEY+1)	/* # data keys, including 0 (reserved) */

/*
 * Some macros for units conversion
 */
/* Core clicks (1024 bytes) to segments and vice versa */
#define	ctos(x)	(x)
#define	stoc(x)	(x)

/* Core clicks (1024 bytes) to disk blocks */
#define	ctod(x)	(x)
#define	dtoc(x)	(x)
#define	dtob(x)	((x)<<PGSHIFT)

/* clicks to bytes */
#define	ctob(x)	((x)<<PGSHIFT)

/* bytes to clicks */
#define	btoc(x)	((((unsigned)(x)+NBPG-1) >> PGSHIFT))

/*
 * Macros to decode processor status word.
 */
#define	USERMODE(ps)	(((ps) & PSL_CURMOD) == PSL_CURMOD)
#define	BASEPRI(ps)	(((ps) & PSL_IPL) == 0)

#define	DELAY(n)	{ register int N = 3*(n); while (--N > 0); }
