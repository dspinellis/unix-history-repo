/*-
 * Copyright (c) 1982, 1986, 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)types.h	7.20 (Berkeley) %G%
 */

#ifndef _TYPES_H_
#define	_TYPES_H_

/* Machine type dependent parameters. */
#include <machine/endian.h>

typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned long	u_long;
typedef	unsigned short	ushort;		/* Sys V compatibility */

#ifdef _NOQUAD
typedef	struct	_uquad	{ u_long val[2]; } u_quad_t;
typedef	struct	_quad	{   long val[2]; } quad_t;
typedef	long *	qaddr_t;
#define	QUADNE(q1, q2) \
	((q1).val[_QUAD_LOWWORD] != (q2).val[_QUAD_LOWWORD] || \
	(q1).val[_QUAD_HIGHWORD] != (q2).val[_QUAD_HIGHWORD])
#define	QUADEQ(q1, q2) \
	((q1).val[_QUAD_LOWWORD] == (q2).val[_QUAD_LOWWORD] && \
	(q1).val[_QUAD_HIGHWORD] == (q2).val[_QUAD_HIGHWORD])
#define	QUADGT(q1, q2) \
	(((q1).val[_QUAD_HIGHWORD] == (q2).val[_QUAD_HIGHWORD]) ? \
	 ((q1).val[_QUAD_LOWWORD] > (q2).val[_QUAD_LOWWORD]) : \
	 ((q1).val[_QUAD_HIGHWORD] > (q2).val[_QUAD_HIGHWORD]))
#define	INCRQUAD(q) \
	((++((q).val[_QUAD_LOWWORD]) == 0) ? ++((q).val[_QUAD_HIGHWORD]) : 0)
#define	ZEROQUAD(q) \
	(q).val[0] = (q).val[1] = 0

#else /* QUAD support */
typedef	unsigned long long u_quad_t;
typedef	long long quad_t;
typedef	quad_t * qaddr_t;
#define	QUADNE(q1, q2)	(q1) != (q2)
#define	QUADEQ(q1, q2)	(q1) == (q2)
#define	QUADGT(q1, q2)	(q1) > (q2)
#define	INCRQUAD(q)	(q)++
#define	ZEROQUAD(q)	(q) = 0
#endif /* QUAD */

typedef	char *	caddr_t;		/* core address */
typedef	long	daddr_t;		/* disk address */
typedef	short	dev_t;			/* device number */
typedef	u_long	ino_t;			/* inode number */
typedef	long	off_t;			/* file offset (should be a quad) */
typedef	u_short	nlink_t;		/* link count */
typedef	long	swblk_t;		/* swap offset */
typedef	long	segsz_t;		/* segment size */
typedef	u_short	uid_t;			/* user id */
typedef	u_short	gid_t;			/* group id */
typedef	short	pid_t;			/* process id */
typedef	u_short	mode_t;			/* permissions */
typedef u_long	fixpt_t;		/* fixed point number */

#ifndef _POSIX_SOURCE
#define	major(x)	((int)(((u_int)(x) >> 8)&0xff))	/* major number */
#define	minor(x)	((int)((x)&0xff))		/* minor number */
#define	makedev(x,y)	((dev_t)(((x)<<8) | (y)))	/* create dev_t */
#endif

#include <machine/ansi.h>
#if !defined(_ANSI_SOURCE) && !defined(_POSIX_SOURCE)
#include <machine/types.h>
#endif

#ifdef	_CLOCK_T_
typedef	_CLOCK_T_	clock_t;
#undef	_CLOCK_T_
#endif

#ifdef	_SIZE_T_
typedef	_SIZE_T_	size_t;
#undef	_SIZE_T_
#endif

#ifdef	_SSIZE_T_
typedef	_SSIZE_T_	ssize_t;
#undef	_SSIZE_T_
#endif

#ifdef	_TIME_T_
typedef	_TIME_T_	time_t;
#undef	_TIME_T_
#endif

#ifndef _POSIX_SOURCE
#define	NBBY	8		/* number of bits in a byte */

/*
 * Select uses bit masks of file descriptors in longs.  These macros
 * manipulate such bit fields (the filesystem macros use chars).
 * FD_SETSIZE may be defined by the user, but the default here should
 * be enough for most uses.
 */
#ifndef	FD_SETSIZE
#define	FD_SETSIZE	256
#endif

typedef long	fd_mask;
#define NFDBITS	(sizeof(fd_mask) * NBBY)	/* bits per mask */

#ifndef howmany
#define	howmany(x, y)	(((x)+((y)-1))/(y))
#endif

typedef	struct fd_set {
	fd_mask	fds_bits[howmany(FD_SETSIZE, NFDBITS)];
} fd_set;

#define	FD_SET(n, p)	((p)->fds_bits[(n)/NFDBITS] |= (1 << ((n) % NFDBITS)))
#define	FD_CLR(n, p)	((p)->fds_bits[(n)/NFDBITS] &= ~(1 << ((n) % NFDBITS)))
#define	FD_ISSET(n, p)	((p)->fds_bits[(n)/NFDBITS] & (1 << ((n) % NFDBITS)))
#define	FD_ZERO(p)	bzero((char *)(p), sizeof(*(p)))

#if defined(__STDC__) && defined(KERNEL)
/*
 * Forward structure declarations for function prototypes.
 * We include the common structures that cross subsystem boundaries here;
 * others are mostly used in the same place that the structure is defined.
 */
struct	proc;
struct	pgrp;
struct	ucred;
struct	rusage;
struct	file;
struct	buf;
struct	tty;
struct	uio;
#endif

#endif /* !_POSIX_SOURCE */
#endif /* !_TYPES_H_ */
