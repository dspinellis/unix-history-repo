/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)uio.h	6.4 (Berkeley) 2/23/86
 */

#ifndef _UIO_
#define	_UIO_

/*
 * Description of an I/O operation which potentially
 * involves scatter-gather, with individual sections
 * described by iovec, below.  uio_resid is initially
 * set to the total size of the operation, and is
 * decremented as the operation proceeds.  uio_offset
 * is incremented by the amount of each operation.
 * uio_iov is incremented and uio_iovcnt is decremented
 * after each vector is processed.
 */
struct uio {
	struct	iovec *uio_iov;
	int	uio_iovcnt;
	off_t	uio_offset;
	int	uio_resid;
	enum	uio_rw uio_rw;
};

enum	uio_rw { UIO_READ, UIO_WRITE };

/*
 * Description of a contiguous section of an I/O operation.
 * If iov_op is non-null, it is called to implement the copy
 * operation, possibly by remapping, with the call
 *	(*iov_op)(from, to, count);
 * where from and to are caddr_t and count is int.
 * Otherwise, the copy is done in the normal way,
 * treating base as a user or kernel virtual address
 * according to iov_segflg.
 */
struct iovec {
	caddr_t	iov_base;
	int	iov_len;
	enum	uio_seg iov_segflg;
	int	(*iov_op)();
};

/*
 * Segment flag values.
 */
enum	uio_seg {
	UIO_USERSPACE,		/* from user data space */
	UIO_SYSSPACE,		/* from system space */
	UIO_USERISPACE		/* from user I space */
};
#endif
