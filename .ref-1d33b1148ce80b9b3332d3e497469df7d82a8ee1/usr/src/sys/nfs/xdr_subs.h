/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rick Macklem at The University of Guelph.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)xdr_subs.h	8.3 (Berkeley) %G%
 */


#ifndef _NFS_XDR_SUBS_H_
#define _NFS_XDR_SUBS_H_

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

#define	fxdr_nfsv2time(f, t) { \
	(t)->ts_sec = ntohl(((struct nfsv2_time *)(f))->nfsv2_sec); \
	if (((struct nfsv2_time *)(f))->nfsv2_usec != 0xffffffff) \
		(t)->ts_nsec = 1000 * ntohl(((struct nfsv2_time *)(f))->nfsv2_usec); \
	else \
		(t)->ts_nsec = 0; \
}
#define	txdr_nfsv2time(f, t) { \
	((struct nfsv2_time *)(t))->nfsv2_sec = htonl((f)->ts_sec); \
	if ((f)->ts_nsec != -1) \
		((struct nfsv2_time *)(t))->nfsv2_usec = htonl((f)->ts_nsec / 1000); \
	else \
		((struct nfsv2_time *)(t))->nfsv2_usec = 0xffffffff; \
}

#define	fxdr_nfsv3time(f, t) { \
	(t)->ts_sec = ntohl(((struct nfsv3_time *)(f))->nfsv3_sec); \
	(t)->ts_nsec = ntohl(((struct nfsv3_time *)(f))->nfsv3_nsec); \
}
#define	txdr_nfsv3time(f, t) { \
	((struct nfsv3_time *)(t))->nfsv3_sec = htonl((f)->ts_sec); \
	((struct nfsv3_time *)(t))->nfsv3_nsec = htonl((f)->ts_nsec); \
}

#define	fxdr_hyper(f, t) { \
	((long *)(t))[_QUAD_HIGHWORD] = ntohl(((long *)(f))[0]); \
	((long *)(t))[_QUAD_LOWWORD] = ntohl(((long *)(f))[1]); \
}
#define	txdr_hyper(f, t) { \
	((long *)(t))[0] = htonl(((long *)(f))[_QUAD_HIGHWORD]); \
	((long *)(t))[1] = htonl(((long *)(f))[_QUAD_LOWWORD]); \
}

#endif
