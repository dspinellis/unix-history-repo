/*	vmklust.h	4.1	11/9/80	*/

/*
 * Klustering constants.  Klustering is the gathering
 * of pages together for pagein/pageout, while clustering
 * is the treatment of hardware page size as though it were
 * larger than it really is.
 *
 * KLMAX gives maximum cluster size in CLSIZE page (cluster-page)
 * units.  Note that KLMAX*CLSIZE must be <= DMMIN in dmap.h.
 */

#define	KLMAX	(16/CLSIZE)
#define	KLIN	(4/CLSIZE)		/* Read up to 4 pages */
#define	KLOUT	(16/CLSIZE)		/* Write up to 16 pages */

#ifdef KERNEL
int	klin;
int	klout;
#endif
