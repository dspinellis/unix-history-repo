/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)types.h	7.12 (Berkeley) %G%
 */

#ifndef _TYPES_
#define	_TYPES_

typedef	short	dev_t;
#ifndef _POSIX_SOURCE
					/* major part of a device */
#define	major(x)	((int)(((unsigned)(x)>>8)&0377))
					/* minor part of a device */
#define	minor(x)	((int)((x)&0377))
					/* make a device number */
#define	makedev(x,y)	((dev_t)(((x)<<8) | (y)))
#endif

typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned long	u_long;
typedef	unsigned short	ushort;		/* Sys V compatibility */

#include <machine/types.h>

#ifdef	_CLOCK_T_
typedef	_CLOCK_T_	clock_t;
#undef	_CLOCK_T_
#endif

#ifdef	_TIME_T_
typedef	_TIME_T_	time_t;
#undef	_TIME_T_
#endif

#ifdef	_SIZE_T_
typedef	_SIZE_T_	size_t;
#undef	_SIZE_T_
#endif

#ifndef _POSIX_SOURCE
typedef	struct	_uquad { unsigned long val[2]; } u_quad;
typedef	struct	_quad { long val[2]; } quad;
#endif
typedef	long *	qaddr_t;	/* should be typedef quad * qaddr_t; */

typedef	long	daddr_t;
typedef	char *	caddr_t;
typedef	u_long	ino_t;
typedef	long	swblk_t;
typedef	long	segsz_t;
typedef	long	off_t;
typedef	u_short	uid_t;
typedef	u_short	gid_t;
typedef	short	pid_t;
typedef	u_short	nlink_t;
typedef	u_short	mode_t;
typedef u_long	fixpt_t;

#ifndef _POSIX_SOURCE
#define	NBBY	8		/* number of bits in a byte */

/*
 * Select uses bit masks of file descriptors in longs.  These macros
 * manipulate such bit fields (the filesystem macros use chars).
 * FD_SETSIZE may be defined by the user, but the default here should
 * be >= NOFILE (param.h).
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

#endif /* !_POSIX_SOURCE */
#endif /* _TYPES_ */
