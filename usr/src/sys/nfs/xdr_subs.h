/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)xdr_subs.h	7.5 (Berkeley) %G%
 */

/*
 * Macros used for conversion to/from xdr representation by nfs...
 * These use the MACHINE DEPENDENT routines ntohl, htonl
 * As defined by "XDR: External Data Representation Standard" RFC1014
 */
#if BYTE_ORDER == LITTLE_ENDIAN
#define fxdr_unsigned(t, v)	((t)ntohl((long)(v)))
#define	fxdr_nfstime(f, t) { \
	(t)->ts_sec = \
		ntohl(((struct nfsv2_time *)(f))->nfs_sec); \
	(t)->ts_nsec = 1000 * \
		ntohl(((struct nfsv2_time *)(f))->nfs_usec); \
}

#define	fxdr_nqtime(f, t) { \
	(t)->ts_sec = \
		ntohl(((struct nqnfs_time *)(f))->nq_sec); \
	(t)->ts_nsec = \
		ntohl(((struct nqnfs_time *)(f))->nq_nsec); \
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
#define	txdr_nqtime(f, t) { \
	((struct nqnfs_time *)(t))->nq_sec = \
		htonl((f)->ts_sec); \
	((struct nqnfs_time *)(t))->nq_nsec = \
		htonl((f)->ts_nsec); \
}
#define	txdr_nfstime(f, t) { \
	((struct nfsv2_time *)(t))->nfs_sec = \
		htonl((f)->ts_sec); \
	((struct nfsv2_time *)(t))->nfs_usec = \
		htonl((f)->ts_nsec) / 1000; \
}
#else	/* BIG_ENDIAN */
#define fxdr_unsigned(t, v)	((t)(v))
#define	fxdr_nqtime(f, t) \
	*(t) = *((struct timespec *)(f))
#define	fxdr_nfstime(f, t) { \
	(t)->ts_sec = ((struct nfsv2_time *)(f))->nfs_sec; \
	(t)->ts_nsec = ((struct nfsv2_time *)(f))->nfs_usec * 1000; \
}
#define	fxdr_hyper(f, t) \
	*((quad_t *)(t)) = *((quad_t *)(f))

#define	txdr_unsigned(v)	((long)(v))
#define	txdr_nqtime(f, t) \
	*(t) = *((struct timespec *)(f))
#define	txdr_nfstime(f, t) { \
	((struct nfsv2_time *)(t))->nfs_sec = (f)->ts_sec; \
	((struct nfsv2_time *)(t))->nfs_usec = (f)->ts_nsec / 1000; \
}
#define	txdr_hyper(f, t) \
	*((quad_t *)(t)) = *((quad_t *)(f))
#endif	/* ENDIAN */
