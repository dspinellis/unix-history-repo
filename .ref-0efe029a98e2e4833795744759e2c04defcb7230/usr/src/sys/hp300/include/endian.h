/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)endian.h	7.5 (Berkeley) %G%
 */

/*
 * Definitions for byte order,
 * according to byte significance from low address to high.
 */
#define	LITTLE_ENDIAN	1234	/* least-significant byte first (vax) */
#define	BIG_ENDIAN	4321	/* most-significant byte first (IBM, net) */
#define	PDP_ENDIAN	3412	/* LSB first in word, MSW first in long (pdp) */

#define	BYTE_ORDER	BIG_ENDIAN	/* byte order on tahoe */

/*
 * Macros for network/external number representation conversion.
 */
#if BYTE_ORDER == BIG_ENDIAN && !defined(lint)
#define	ntohl(x)	(x)
#define	ntohs(x)	(x)
#define	htonl(x)	(x)
#define	htons(x)	(x)

#define	NTOHL(x)	(x)
#define	NTOHS(x)	(x)
#define	HTONL(x)	(x)
#define	HTONS(x)	(x)

#else

unsigned short	ntohs(), htons();
unsigned long	ntohl(), htonl();

#define	NTOHL(x)	(x) = ntohl(x)
#define	NTOHS(x)	(x) = ntohs(x)
#define	HTONL(x)	(x) = htonl(x)
#define	HTONS(x)	(x) = htons(x)
#endif
