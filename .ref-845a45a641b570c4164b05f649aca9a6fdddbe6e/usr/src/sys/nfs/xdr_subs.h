/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)xdr_subs.h	7.7 (Berkeley) %G%
 */

/*
 * Macros used for conversion to/from xdr representation by nfs...
 * These use the MACHINE DEPENDENT routines ntohl, htonl
 * As defined by "XDR: External Data Representation Standard" RFC1014
 *
 * To simplify the implementation, we use ntohl/htonl even on big-endian
 * machines, and count on them being `#define'd away.  Some of these
 * might be slightly more efficient as quad_t copies on a big-endian,
 * but we cannot count on their alignment anyway.
 */

#define	fxdr_unsigned(t, v)	((t)ntohl((long)(v)))
#define	txdr_unsigned(v)	(htonl((long)(v)))

#define	fxdr_nfstime(f, t) { \
	(t)->ts_sec = ntohl(((struct nfsv2_time *)(f))->nfs_sec); \
	(t)->ts_nsec = 1000 * ntohl(((struct nfsv2_time *)(f))->nfs_usec); \
}
#define	txdr_nfstime(f, t) { \
	((struct nfsv2_time *)(t))->nfs_sec = htonl((f)->ts_sec); \
	((struct nfsv2_time *)(t))->nfs_usec = htonl((f)->ts_nsec) / 1000; \
}

#define	fxdr_nqtime(f, t) { \
	(t)->ts_sec = ntohl(((struct nqnfs_time *)(f))->nq_sec); \
	(t)->ts_nsec = ntohl(((struct nqnfs_time *)(f))->nq_nsec); \
}
#define	txdr_nqtime(f, t) { \
	((struct nqnfs_time *)(t))->nq_sec = htonl((f)->ts_sec); \
	((struct nqnfs_time *)(t))->nq_nsec = htonl((f)->ts_nsec); \
}

#define	fxdr_hyper(f, t) { \
	((long *)(t))[_QUAD_HIGHWORD] = ntohl(((long *)(f))[0]); \
	((long *)(t))[_QUAD_LOWWORD] = ntohl(((long *)(f))[1]); \
}
#define	txdr_hyper(f, t) { \
	((long *)(t))[0] = htonl(((long *)(f))[_QUAD_HIGHWORD]); \
	((long *)(t))[1] = htonl(((long *)(f))[_QUAD_LOWWORD]); \
}
