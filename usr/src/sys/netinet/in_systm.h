/* in_systm.h 4.4 81/11/18 */

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
typedef u_long	seq_t;			/* sequence number */

typedef	u_long	n_time;			/* ms since 00:00 GMT, byte rev */

/*
 * The network runs as a software interrupt process.
 * You can switch into the network by doing splnet() and return by splx().
 * The software interrupt level for the network is higher than the software
 * level for the clock (so you can enter the network in routines called
 * at timeout time).  Splimp is an ipl high enough to block all imps.
 * While manipulating the mbuf buffer pool you have to block imps.
 */
#define	splimp		spl5
#define	setsoftnet()	mtpr(SIRR, 12)
/* splnet is defined in ../sys/asm.sed */

/*
 * Network statistics record.
 *
 * SHOULD BE KEPT PER INTERFACE, AND WITH CNT, RATE, SUM.
 */
struct	net_stat {
	int	imp_resets;		/* # times imp reset */
	int	imp_flushes;		/* # packets flushed by imp */
	int	imp_drops;		/* # msgs from imp no-one wants */
	int	m_drops;		/* # mbuf drops from lack of bufs */
	int	ip_badsum;		/* # bad ip checksums */
	int	t_badsum;		/* # bad tcp checksums */
	int	t_badsegs;		/* # bad tcp segments */
	int	t_unack;		/* # tcp segs placed on rcv_unack */
};

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
