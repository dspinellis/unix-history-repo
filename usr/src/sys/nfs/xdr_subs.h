/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)xdr_subs.h	7.4 (Berkeley) %G%
 */

/*
 * Macros used for conversion to/from xdr representation by nfs...
 * These use the MACHINE DEPENDENT routines ntohl, htonl
 * As defined by "XDR: External Data Representation Standard" RFC1014
 */
#if BYTE_ORDER == LITTLE_ENDIAN
#define fxdr_unsigned(t, v)	((t)ntohl((long)(v)))
#define	fxdr_time(f, t)	{ \
	((struct timeval *)(t))->tv_sec = \
		ntohl(((struct timeval *)(f))->tv_sec); \
	((struct timeval *)(t))->tv_usec = \
		ntohl(((struct timeval *)(f))->tv_usec); \
}

/*
 * To handle quad conversions, define a struct of two longs and use
 * ntohl and htonl. Maybe someday there should be ntohq and htonq?
 */
union _hq {
	quad_t	hq;
	struct {
		long val[2];
	} lq;
};
#define	fxdr_hyper(f, t) { \
	((union _hq *)(t))->lq.val[1] = ntohl(((union _hq *)(f))->lq.val[0]); \
	((union _hq *)(t))->lq.val[0] = ntohl(((union _hq *)(f))->lq.val[1]); \
}
#define	txdr_hyper(f, t) { \
	((union _hq *)(t))->lq.val[0] = htonl(((union _hq *)(f))->lq.val[1]); \
	((union _hq *)(t))->lq.val[1] = htonl(((union _hq *)(f))->lq.val[0]); \
}

#define	txdr_unsigned(v)	(htonl((long)(v)))
#define	txdr_time(f, t)	{ \
	((struct timeval *)(t))->tv_sec = \
		htonl(((struct timeval *)(f))->tv_sec); \
	((struct timeval *)(t))->tv_usec = \
		htonl(((struct timeval *)(f))->tv_usec); \
}
#else	/* BIG_ENDIAN */
#define fxdr_unsigned(t, v)	((t)(v))
#define	fxdr_time(f, t) \
	*((struct timeval *)(t)) = *((struct timeval *)(f))
#define	fxdr_hyper(f, t) \
	*((quad_t *)(t)) = *((quad_t *)(f))

#define	txdr_unsigned(v)	((long)(v))
#define	txdr_time(f, t) \
	*((struct timeval *)(t)) = *((struct timeval *)(f))
#define	txdr_hyper(f, t) \
	*((quad_t *)(t)) = *((quad_t *)(f))
#endif	/* ENDIAN */
