/*	vadvise.h	4.1	83/02/10	*/

/*
 * Parameters to vadvise() to tell system of particular paging
 * behaviour:
 *	VA_NORM		Normal strategy
 *	VA_ANOM		Sampling page behaviour is not a win, don't bother
 *			Suitable during GCs in LISP, or sequential or random
 *			page referencing.
 *	VA_SEQL		Sequential behaviour expected.
 *	VA_FLUSH	Invalidate all page table entries.
 */
#define	VA_NORM	0
#define	VA_ANOM	1
#define	VA_SEQL	2
#define	VA_FLUSH 3
