/*	in_systm.h	4.10	82/04/20	*/

/*
 * Miscellaneous internetwork
 * definitions for kernel.
 */

#ifndef LOCORE
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
#endif

/*
 * The internet code runs off software interrupts.
 *
 * You can switch into the network by doing splnet() and return by splx().
 * The software interrupt level for the network is higher than the software
 * level for the clock (so you can enter the network in routines called
 * at timeout time).  Splimp is an ipl high enough to block all imps.
 * While manipulating the mbuf buffer pool you have to block imps.
 */

/* splnet is defined in ../sys/asm.sed */
#ifdef ECHACK
#define	splimp		spl6
#else
#define	splimp		spl5
#endif
#define	setsoftnet()	mtpr(SIRR, 12)

/*
 * Each ``pup-level-1'' input queue has a bit in a ``netisr'' status
 * word which is used to de-multiplex a single software
 * interrupt used for scheduling the network code to calls
 * on the lowest level routine of each protocol.
 */
#define	NETISR_RAW	0		/* same as AF_UNSPEC */
#define	NETISR_IP	2		/* same as AF_INET */
#define	NETISR_NS	6		/* same as AF_NS */

#define	schednetisr(anisr)	{ netisr |= 1<<(anisr); setsoftnet(); }

#ifndef LOCORE
#ifdef KERNEL
int	netisr;				/* scheduling bits for network */
#endif


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
#endif
