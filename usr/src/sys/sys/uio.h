/*
 * Copyright (c) 1982, 1986, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)uio.h	8.4 (Berkeley) %G%
 */

#ifndef _SYS_UIO_H_
#define	_SYS_UIO_H_

/*
 * XXX
 * iov_base should be a void *.
 */
struct iovec {
	char	*iov_base;	/* Base address. */
	size_t	 iov_len;	/* Length. */
};

#ifdef KERNEL
enum	uio_rw { UIO_READ, UIO_WRITE };

/*
 * Segment flag values.
 */
enum uio_seg {
	UIO_USERSPACE,		/* from user data space */
	UIO_SYSSPACE,		/* from system space */
	UIO_USERISPACE		/* from user I space */
};

struct uio {
	struct	iovec *uio_iov;
	int	uio_iovcnt;
	off_t	uio_offset;
	int	uio_resid;
	enum	uio_seg uio_segflg;
	enum	uio_rw uio_rw;
	struct	proc *uio_procp;
};

/*
 * Limits
 */
#define UIO_MAXIOV	1024		/* max 1K of iov's */
#define UIO_SMALLIOV	8		/* 8 on stack, else malloc */
#endif /* KERNEL */

#ifndef	KERNEL
#include <sys/cdefs.h>

__BEGIN_DECLS
ssize_t	readv __P((int, const struct iovec *, int));
ssize_t	writev __P((int, const struct iovec *, int));
__END_DECLS
#endif /* !KERNEL */
#endif /* !_SYS_UIO_H_ */
