/*
 * Copyright (c) 1982, 1986, 1993, 1995
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tcp_seq.h	8.3 (Berkeley) %G%
 */

/*
 * TCP sequence numbers are 32 bit integers operated
 * on with modular arithmetic.  These macros can be
 * used to compare such integers.
 */
#define	SEQ_LT(a,b)	((int)((a)-(b)) < 0)
#define	SEQ_LEQ(a,b)	((int)((a)-(b)) <= 0)
#define	SEQ_GT(a,b)	((int)((a)-(b)) > 0)
#define	SEQ_GEQ(a,b)	((int)((a)-(b)) >= 0)

/*
 * Macros to initialize tcp sequence numbers for
 * send and receive from initial send and receive
 * sequence numbers.
 */
#define	tcp_rcvseqinit(tp) \
	(tp)->rcv_adv = (tp)->rcv_nxt = (tp)->irs + 1

#define	tcp_sendseqinit(tp) \
	(tp)->snd_una = (tp)->snd_nxt = (tp)->snd_max = (tp)->snd_up = \
	    (tp)->iss

#ifdef KERNEL
/*
 * Increment for tcp_iss each second.
 * This is designed to increment at the standard 250 KB/s,
 * but with a random component averaging 128 KB.
 * We also increment tcp_iss by a quarter of this amount
 * each time we use the value for a new connection.
 * If defined, the tcp_random18() macro should produce a
 * number in the range [0-0x3ffff] that is hard to predict.
 */
#ifndef tcp_random18
#define	tcp_random18()	((random() >> 14) & 0x3ffff)
#endif
#define	TCP_ISSINCR	(122*1024 + tcp_random18())

tcp_seq	tcp_iss;		/* tcp initial send seq # */
#else
#define	TCP_ISSINCR	(250*1024)	/* increment for tcp_iss each second */
#endif
