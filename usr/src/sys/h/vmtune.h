/*	vmtune.h	4.1	11/9/80	*/

/*
 * Tunable performance parameters
 *
 * These may vary per-cpu due to configuration as well as the flavor of
 * the local job mix.  MAXPGIO in multiplied by the number of swap devices
 * available (nswdev) at boot time.
 */
#define	LOOPSIZ		((maxfree - firstfree) / CLSIZE)
					/* loop circumference */
#define	LOTSFREE	((maxfree - firstfree) / 4)
					/* very high mark to freeze scans */
#define	DESFREE 	200		/* minimum desirable free memory */
#define	MINFREE 	64		/* water mark to run swap daemon */
#define	MAXSLP 		20		/* max blocked time (in seconds) allowed
					   before being very swappable */
#define	SLOWSCAN	25		/* seconds per loop when memory easy */
#define	FASTSCAN	15		/* seconds per loop when memory tight */
#define	SAFERSS		32		/* nominal ``small'' resident set size
					   protected against replacement */
#define	MAXPGIO		50		/* max desired paging i/o per second
					   per paging device, controls max
					   paging ops per second */
/* WITH MAXPGIO SET TO 50, IT RARELY TAKES OVER */
