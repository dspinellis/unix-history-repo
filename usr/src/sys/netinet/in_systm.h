/* in_systm.h 4.6 81/11/26 */

/*
 * Miscellaneous internetwork
 * definitions for kernel.
 */

/*
 * Network types.
 *
 * Internally the system keeps counters in the headers with the bytes
 * swapped so that VAX instructions will work on them.  It reverses
 * the bytes before transmission at each protocol level.  The n_ types
 * represent the types with the bytes in ``high-ender'' order.
 */
typedef u_short n_short;		/* short as received from the net */
typedef u_long	n_long;			/* long as received from the net */

typedef	u_long	n_time;			/* ms since 00:00 GMT, byte rev */

/*
 * The internet code runs off software interrupts.
 * There is one software interrupt level for each IP layer protocol
 * (e.g. IP, PUP, etc), and each such interrupt traps to the lowest
 * level routine for that protocol.
 *
 * You can switch into the network by doing splnet() and return by splx().
 * The software interrupt level for the network is higher than the software
 * level for the clock (so you can enter the network in routines called
 * at timeout time).  Splimp is an ipl high enough to block all imps.
 * While manipulating the mbuf buffer pool you have to block imps.
 */
#define	splimp		spl5
#define	setipintr()	mtpr(SIRR, 12)
/* splnet is defined in ../sys/asm.sed */

#ifdef	KERNEL
n_time	iptime();
#endif

#ifdef KPROF
#include "../inet/count.h"
#define	COUNT(i)	nrcount[i]++
int	nrcount[NCOUNTERS+1];
#else
#define	COUNT(i)
#endif
